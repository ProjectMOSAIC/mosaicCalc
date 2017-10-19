#' Plots of functions
#' 
#' Draws graphs in the ggformula system of functions of one and several variables.
#' 
#' @details Specify the function to be plotted as a formula. The LHS of the formula gives
#' the value of the function, the RHS specifies one (TO BE ADDED or two) variables to be used for 
#' the spatial coordinates of the plot and any faceting variables. Additional aethetics,
#' for instance, color, can be assigned to values in the usual ggformula way, e.g. `color = ~ z`.
#'
#' All arguments to the function *must* have a value or a range of variables specified, e.g.
#' `x = c(0, 10)` or `a = 3` or `b = 1:5`. The input variable(TO BE ADDED: s) used for the spatial coordinates
#' will have a finer grid produced if just two values are given (e.g. `x = c(0, 10)`).
#' 
#' @param object When chaining, this holds an object produced in the earlier portions of the chain. Most users can safely ignore this argument. See details and examples.
#' @param formula The formula specifying the layout of the plot, e.g. `sin(x) ~ x`. See examples 
#' and \link{`mosaicCore::makeFun()`}.
#' @param ... Additional arguments. These must include specifications of the ranges or values of all parameters used in the formula.
#' @param inherit A logical indicating whether graphical attributes should be inherited from earlier 
#' plots chained into this one.
#' @examples
#' plot_f1(sqrt(x) ~ x, x = c(0,10))
#' if (require(mosaicData)) {
#'   gf_histogram(..density.. ~ age, data = HELPrct, binwidth = 3, alpha = 0.6) %>%
#'   plot_f1(dnorm(age, mean, sd) ~ age, age = c(0,100), color = ~ sd,
#'     mean = mean(HELPrct$age), sd = 1:3 * sd(HELPrct$age))
#'   # Utility bill is quadratic in month?
#'   f <- makeFun(lm(totalbill ~ poly(month, 2), data = Utilities))
#'   gf_point(totalbill ~ month, data = Utilities, alpha = 0.6) %>%
#'    plot_f1(f(month) ~ month, color = "red")
#'   f2 <- makeFun(lm(totalbill ~ poly(month, 2) * ccf, data = Utilities))
#'   gf_point(totalbill ~ month, color = ~ ccf, data = Utilities, alpha = 0.6) %>%
#'    plot_f1(f2(month, ccf) ~ month, color = ~ ccf, ccf = 100*(0:2))
#' } 

#' @export
plot_f1 <- function(object = NULL, formula, ..., inherit = FALSE, npts = 101) {
  if (rlang::is_formula(object) && missing(formula)) {
    formula <- object
    object <- NULL
  }
  if (is.null(object)) {
    #  object <- ggplot(data = data.frame(x = xlim), aes(x))
    inherit <- TRUE
  }
  dots <- rlang::quos(...)  # rlang format
  dots <- eval_tidy(dots)
  # for_eval <- dots <- list(...) # Why not do it simply like this?
  xvar <- all.vars(rhs(formula))
  if (length(xvar) != 1) stop("Must specify exactly one variable for the x-axis.")
  
  fun <- if (is.function(formula)) formula else mosaicCore::makeFun(formula)
  
  
  for_eval <- dots[names(dots) %in% names(formals(fun))]
  # set some default for the x axis
  if ( ! xvar %in% names(for_eval) ) {
    if ( ! is.null(object) && xvar %in% names(object$data)) {
      for_eval[[xvar]] <- range(object$data[[xvar]])
    } else {
      if (length(for_eval[[xvar]]) < 2) {
        for_eval[[xvar]] <- c(0,1)
        warning("Must specify range of values for the x-axis variable '", xvar, "'")
      }
    }
  }
  
  xvals <- for_eval[[xvar]]
  if (length(xvals) == 2) for_eval[[xvar]] <- seq(xvals[1], xvals[2], length.out = npts)
  # arguments that aren't specifying input-variable ranges
  remaining <- dots[ ! names(dots) %in% names(for_eval)]
  
  # evaluate the function at the specified values
  Pts <- do.call(expand.grid, for_eval)
  formula_vars <- c(mosaicCore::rhs(formula), mosaicCore::condition(formula))
  grouping_vars <- setdiff(names(Pts), formula_vars)
  Pts[["..output"]] <- do.call(fun, Pts)
  
  # if there are any variables left to group with, set up those groups explicitly.
  if (length(grouping_vars) > 0) {
    For_groups <- unique(Pts[grouping_vars])
    For_groups[["..group"]] <- 1:nrow(For_groups)
    Pts <- merge(Pts, For_groups)
  } else {
    Pts[["..group"]] <- 1
  }

  args_for_gf <- c(list(object, formula, data = Pts), 
                   remaining, group = ~ ..group,
                   inherit = inherit)
  do.call(gf_line, args_for_gf)
  
}