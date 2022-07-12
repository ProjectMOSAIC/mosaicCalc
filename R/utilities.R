#' Utilities for formulas and graphics arguments
#' 
#' `infer_RHS` turns a one-sided formula into a two-sided formula suitable for `makeFun()`.
#' `formals_from_expression` creates a "formals" list for creating a function.
#' The list will have arguments in the canonical order.
#'
#' @param ex An expression as in `quote(x^2)` or the left side of a tilde expression.
#' @param others Character string(s) with names of additional arguments to be 
#' included in the formals
#' 
#' @export
infer_RHS <- function(ex) {
  RHS <- paste(
    sort_args_by_convention(all.vars(ex)),
    collapse="&")
  res <- as.formula(paste("a ~", RHS))
  res[[2]] <- ex
  
  res
}

#' 
#' @rdname infer_RHS
#' @export
formals_from_expr <- function(ex, others=character(0)) {
  names <- sort_args_by_convention( c(all.vars(ex), others))
  command <- paste0("alist(", 
                    paste0(names, "=", collapse=", "),
                    ")")
  
  eval(parse(text=command))
}


sort_args_by_convention <- function(vars) {
  special <- c("x", "y", "t", "u", "v", "w", "z")
  hits <- special %in% vars
  first_ones <- special[hits]
  remaining_ones <- sort(setdiff(vars, first_ones))
  
  c(first_ones, remaining_ones)
}

#' Handle the first three arguments of graphics functions
#' 
#' This function is intended for package developers, not end-users. Canonically, `mosaicCalc` functions that produce layerable graphics have
#' three initial arguments in a specific order: (1) a previous gg layer, (2) a tilde expression, and (3)
#' a domain. But either (1) or (3) can be missing. `first_three_args()` translates a leading
#' ... argument into the list of the canonical three initial arguments, returning them
#' as components of a list.  In addition, there may be additional arguments
#' in ... that specify other aspects of the plot, e.g. color. 
#' 
#' In constructing a mosaicCalc graphics layer, the function (e.g. `slice_plot()` or `contour_plot()`)
#' whould have ... as its first argument. Intercept that ... with `first_three_args()` to
#' extract the first three canonical arguments as components `gg`, `tilde`, and `domain` of a 
#' list. Any remaining arguments in ... will be placed in the `dots` component.
#' 
#' @param \dots unnamed arguments to be translated into a list with the three canonical arguments
#' and any other arguments not named explicitly in the parent function definition.
#' @param two_tildes if `TRUE` then look for the first FOUR arguments, the middle 
#' two of which will be tilde expressions.
#' 
#' 
#' @export
first_three_args <- function(..., two_tildes = FALSE) {
  args <- list(...)
  
  res <- list(gg = NULL, tilde = NULL, domain=NULL, dots = list())
  
  fault_msg <- "First argument must be a tilde expression, optionally preceeded by a graphics layer."
  if (!(inherits(args[[1]], "formula") || inherits(args[[2]], "formula"))) {
    stop(fault_msg)
  } 
  
  if (inherits(args[[1]], "gg") || is.null(args[[1]])) {
    res$gg <- args[[1]]
    args <- args[-1] # take off the stack
  }
  
  if (inherits(args[[1]], "formula")) {
    res$tilde = args[[1]]
    args <- args[-1] # take off the stack
  } else {
    stop(fault_msg)
  }
  
  # special case when two functions are called for as in 
  # vectorfield_plot()
  if (two_tildes) {
    if (inherits(args[[1]], "formula")) {
      res$tilde2 = args[[1]]
      args <- args[-1] # take off the stack
    } else {
      stop(fault_msg)
    } 
  }
  
  if (length(args) > 0 && inherits(args[[1]], "domain")) {
    res$domain <- args[[1]]
    args <- args[-1] # take off the stack
  }
  
  res$dots <- args
  
  res
  
}
