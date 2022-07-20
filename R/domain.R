#' Specify a domain over which a function is to be graphed
#'
#' domain() is used with slice_plot(), contour_plot(), or interactive_plot()
#' to describe the plotting region. There is a standard syntax for
#' domain() (see the first example) but there are also shortcuts.
#' In the shorthand syntaxes, you can but don't have to specify the name of
#' the input. If it's not specified, the plotting programs will try
#' to do something sensible. But better to specify the names explicitly.
#'
#' @param \dots One or more expression of the form x=-5:5
#'
#' @examples
#' \dontrun{
#' contour_plot(x / y ~ x + y, bounds(x=-5:5, y=1:4))
#' slice_plot(x^2 ~ x, bounds(x = 2.5:4.2)) # overrides colon operator
#' slice_plot(x^2 ~ x, bounds(x = c(2.5, 4.2)))
#' slice_plot(x^2 ~ x, bounds(x = 1 %pm% 0.5))
#' }
#'
#' @export
bounds <- function(...) {
  {
    args <- quos(...) #enexprs(...)
    if (length(args) == 0) domain(-5:5)
    res_names <- names(args)
    res <- as.list(rep("",length(args)))
    dim_class <- ifelse(length(args) == 1, "xdomain",
                        ifelse(length(args)==2, "xydomain",
                               "multi-domain")
                        )
    for (k in 1:length(args)) {
      ex <- args[[k]]
      command <- as.character(rlang::quo_get_expr(ex)[[1]])
      if (nchar(names(args)[k]) > 0) {
        res_names[k] <- names(args)[k]
      } else {
        # It's unnamed, leave it for later
        res_names[k] <- paste0(".unknown_", k)
      }
      if (command == "c") {
        res[[k]] <- range(rlang::eval_tidy(rlang::quo_get_expr(ex)))
      } else if (command == "[") {
        res_names[k] <- as.character(rlang::quo_get_expr(ex)[[2]])
        res[[k]] <- c(rlang::eval_tidy(rlang::quo_get_expr(ex)[[3]]), rlang::eval_tidy(rlang::quo_get_expr(ex)[[4]]))
      } else if (command == "%pm%") {
        center <- rlang::eval_tidy(rlang::quo_get_expr(ex)[[2]])
        margin <- rlang::eval_tidy(rlang::quo_get_expr(ex)[[3]])
        res[[k]] <- c(center - margin, center + margin)
      } else if (command %in% c("%%","|", "||", "&" ,"<", "<=", ">", ">=", ":")) {
        res[[k]] <- c(rlang::eval_tidy(rlang::quo_get_expr(ex)[[2]]),
                      rlang::eval_tidy(rlang::quo_get_expr(ex)[[3]]))
      } else {
        res[[k]] <- rlang::eval_tidy(ex)
      }

    }
    names(res) <- res_names
    class(res) <- c("list", "domain", dim_class)
    res
  }
  # OLD VERSION OF domain()
  # args <- list(...)
  # args <- lapply(args, range) # reduce them to two numbers
  # if (length(args) == 2) args
  # else if (length(args)  == 1) c(args, NULL)
  # else stop("Must specify a frame for one or two variables, e.g. `domain(x=c(0,1), y =  c(-1,1))`.")
}

#' @rdname bounds
#' @export
domain <- bounds

validate_domain <- function(dom, free_args) {
  if (!is.list(dom)) dom <- list(dom)
  dom <- lapply(dom, range)# make sure it's two numbers for each component
  if (is.null(names(dom))) {
    # A list with one element doesn't have any names. We need to put it in.
    names(dom) <- ".unknown_only_one"
  }
  known_names <- names(dom)[!(names(dom) == "" |grepl("\\.unknown_", names(dom)))]
  if (length(known_names) > 0 && any(!known_names %in% free_args)) {
    stop(paste("Bounds has variable(s)",
               paste(names(dom), collapse = " & "),
               "but function has argument(s) named",
               paste(free_args, collapse = " & ")))
  }
  missing <-  length(free_args) - length(dom)
  if (missing != 0) {
    # add placeholders for any missing domain specifiers
    rest_of_them <- lapply(1:missing, function(x) c(-5,5))
    names(rest_of_them) <- paste0(".unknown_a", 1:missing)
    dom <- c(dom, rest_of_them)
    warning("Using -5 to 5 in bounds for missing domain names.")
  }

  if (length(dom) == length(free_args)) {
    if (all(names(dom) %in% free_args)) return(dom) #worked!
    if (any(unknown <- names(dom) == "" | grepl("\\.unknown_", names(dom)))) {
      missing_names <- setdiff(free_args, names(dom)[!unknown])
      names(dom)[unknown] <- missing_names
      warning(paste("Missing bounds names:", paste(missing_names, collapse=", ")))
      return (validate_domain(dom, free_args))
    }
    else { # there's a mis-match with names needed for plotting.
      stop(paste("Bounds has variable(s)",
                  paste(names(dom), collapse = " & "),
                  "but function has argument(s) named",
                  paste(free_args, collapse = " & ")))
    }
  } else {
    stop(paste("Bounds involve",
               length(names(dom)),
               "variables, but function has",
               length(free_args)))
  }

  return(dom)
}

# Convert a function on a domain to a data frame containing a grid of points.
eval_on_domain <- function(formula, domain, n=100, params) {
  fun <- mosaicCore::makeFun(formula)
  # Fill in any parameter updates from the function calling this one
  if (!missing(params)) fun <- bind_params(fun, params)
  arg_names <- names(formals(fun))
  arg_vals <- as.character(formals(fun))
  constants <- arg_names[arg_vals != ""]
  free_args <- arg_names[arg_vals == ""]
  domain <- validate_domain(domain, free_args)
  make_seq <- function(interval, length = n) {
    seq(interval[1], interval[2], length = length)
  }
  grid <- lapply(domain, make_seq)
  grid <- do.call(expand.grid, grid)

  # test if function can be evaluated with the names from `grid`
  test_result <- try(do.call(fun, grid[1, , drop=FALSE]), silent=TRUE)
  if (inherits(test_result, "try-error")) {
    
    stop(glue::glue("Function specified by {capture.output(formula)[1]} 
                    cannot be evaluated using input names 
                    {paste0(names(grid), collapse=', ')}"))
  }
    
  # try to evaluate as if it were vectorized
  vals <- try(do.call(fun, grid), silent=TRUE)
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
  domain <- validate_domain(domain, free_args)
  make_seq <- function(interval, length = n) {
    seq(interval[1], interval[2], length = length)
  }
  grid <- lapply(domain, make_seq)
  grid <- do.call(expand.grid, grid)

  # try to evaluate as if it were vectorized
  vals <- try(do.call(fun, grid), silent=TRUE)
  if (inherits(vals, "try-error")) stop("Need to write non-vectorized evaluation of function  on  grid.")

  # Now turn it into list with  x,  y, vals-matrix
  result <- list(x = sort(unique(grid[[1]])),
                 y = sort(unique(grid[[2]])))
  result$.output. = matrix(vals, byrow = TRUE, ncol = length(result$x))

  result
}

# An idea for a replacement to <domain()>

interval <- function(...) {
  args <- enquos(...)
  if (any(duplicated(names(args))))
    stop("Duplicated input name.")
  res <- list()
  res_names <- character(length(args))
  for (k in 1:length(args)) {
    ex <- args[[k]]
    command <- as.character(rlang::quo_get_expr(ex)[[1]])
    if (nchar(names(args)[k]) > 0) {
      res_names[k] <- names(args)[k]
    } else {
      # It's unnamed, leave it for later
      res_names[k] <- paste0(".unknown_", k)
    }
    if (command == "[") {
      res_names[k] <- as.character(rlang::quo_get_expr(ex)[[2]])
      res[[k]] <- c(rlang::eval_tidy(rlang::quo_get_expr(ex)[[3]]), rlang::eval_tidy(rlang::quo_get_expr(ex)[[4]]))
    } else if (command == "%pm%") {
      center <- rlang::eval_tidy(rlang::quo_get_expr(ex)[[2]])
      margin <- rlang::eval_tidy(rlang::quo_get_expr(ex)[[3]])
      res[[k]] <- c(center - margin, center + margin)
    } else if (command %in% c("%%","|", "||", "&" ,"<", "<=", ">", ">=", ":")) {
      res[[k]] <- c(rlang::eval_tidy(rlang::quo_get_expr(ex)[[2]]),
                    rlang::eval_tidy(rlang::quo_get_expr(ex)[[3]]))
    } else {
      res[[k]] <- rlang::eval_tidy(rlang::quo_get_expr(ex))
    }
    
  }
  names(res) <- res_names
  res
}

