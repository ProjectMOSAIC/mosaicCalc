#' Specify a domain over which a function is to be graphed
#'
#' @export
domain <- function(...) {
  args <- list(...)
  if (length(args) == 2) args
  else if (length(args)  == 1) c(args, NULL)
  else stop("Must specify a frame for one or two variables, e.g. `domain(x=c(0,1), y =  c(-1,1))`.")
}

validate_domain <- function(dom, free_args) {
  if (length(names(dom)) == length(free_args)) {
    if (all(names(dom) %in% free_args)) return("") #worked!
    else {
      stop(paste("Domain has variable(s)",
                  paste(names(dom), collapse = " & "),
                  "but function has argument(s) named",
                  paste(free_args, collapse = " & ")))
    }
  } else {
    stop(paste("Domain involves",
               length(names(dom)),
               "variables, but function has",
               length(free_args)))
  }
}

# Convert a function on a domain to a data frame containing a grid of points.
eval_on_domain <- function(formula, domain, n=100) {
  fun <- mosaicCore::makeFun(formula)
  arg_names <- names(formals(fun))
  arg_vals <- as.character(formals(fun))
  constants <- arg_names[arg_vals != ""]
  free_args <- arg_names[arg_vals == ""]
  validate_domain(domain, free_args)
  make_seq <- function(interval, length = n) {
    seq(interval[1], interval[2], length = length)
  }
  grid <- lapply(domain, make_seq)
  grid <- do.call(expand.grid, grid)

  # try to evaluate as if it were vectorized
  vals <- try(do.call(fun, grid))
  if (inherits(vals, "try-error")) stop("Need to write non-vectorized evaluation of function  on  grid.")
  grid$.output. <- vals
  return(grid)
}

eval_as_vector_and_matrix <- function(formula, domain, n=100)  {
  fun <- mosaicCore::makeFun(formula)
  arg_names <- names(formals(fun))
  arg_vals <- as.character(formals(fun))
  constants <- arg_names[arg_vals != ""]
  free_args <- arg_names[arg_vals == ""]
  validate_domain(domain, free_args)
  make_seq <- function(interval, length = n) {
    seq(interval[1], interval[2], length = length)
  }
  grid <- lapply(domain, make_seq)
  grid <- do.call(expand.grid, grid)

  # try to evaluate as if it were vectorized
  vals <- try(do.call(fun, grid))
  if (inherits(vals, "try-error")) stop("Need to write non-vectorized evaluation of function  on  grid.")

  # Now turn it into list with  x,  y, vals-matrix
  result <- list(x = sort(unique(grid[[1]])),
                 y = sort(unique(grid[[2]])))
  result$.output. = matrix(vals, ncol = length(result$x))

  result
}


