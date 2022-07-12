#' Symbolic Derivatives
#'
#' Constructs symbolic derivatives of some mathematical expressions
#'
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu})
#' @rdname symbolicD
#' @name symbolicD
#' @aliases symbolicD
#'
#' @param tilde a tilde expression with the function call on the left side and the w.r.t. variables 
#' on the right side. 
#' @param \dots additional parameters, typically default values for mathematical parameters
#' @param .order a number specifying the order of a derivative with respect to a single variable
#'
#' @return a function implementing the derivative
#'
#' @details
#' Uses the Derivs package for constructing the derivative
#' The \code{.order} argument is just for convenience when programming
#' high-order derivatives, e.g. the 5th derivative w.r.t. one variable.
#'
#' When re-assigning default values for arguments in a function
#' being called, as in `D(dnorm(x, mean=3) ~ x)`, you will get a
#' numerical derivative even when the analytic form is known. To avoid
#' this (when possible) use `D(dnorm(x) ~ x, mean=3)`
#'
#' @seealso \code{\link{D}}, \code{\link{numD}}, \code{\link{makeFun}}, \code{\link{antiD}}, \code{\link{plotFun}}
#'
#' @examples
#' symbolicD( a*x^2 ~ x)
#' symbolicD( a*x^2 ~ x&x)
#' symbolicD( a*sin(x)~x, .order=4)
#' symbolicD( a*x^2*y+b*y ~ x, a=10, b=100 )
#' symbolicD( dnorm(x, mn, sd) ~ x, mn=3, sd=2)
#' @export

symbolicD <- function(tilde, ..., .order) {
  if (length(tilde) != 3)
    stop("Must provide a two sided tilde expression. With-respect-to-variable(s) go on the right-hand side.")
  
  
  if (length(all.vars(tilde[[2]])) == 0) {
    # It's a constant
    tilde[[2]] <- quote(1)
  }
  left <- rlang::f_lhs(tilde)
  dots <- list(...)
  vars <- all.vars(rlang::f_rhs(tilde), unique=FALSE)
  new_defaults <- list(...)
  # new_formals <- formals_from_expr(left, vars) # will be in canonical order.
  # fun <- function(){}
  # body(fun) <- left
  # formals(fun) <- new_formals
  if (is.call(left) && length(left) <= 2) {
    # tilde calls a function. Make sure all args of that function
    # are included, even if they are not mentioned in the tilde
    # The length(left) <= 2 makes sure it's a function of at most one argument
    inside <- get(left[[1]]) # function being called
    if (is.function(inside)) old_formals <- formals(inside)
    else old_formals = list()
    missing_args <- setdiff(names(old_formals), c(all.vars(left), "pi"))
    if (length(missing_args) > 0) {
      new_defaults[missing_args] <- old_formals[missing_args]
      n <- length(left)
      for (k in 1:length(missing_args)) {
        left[[n+k]] = as.name(missing_args[k])
        if (!missing_args[k] %in% names(dots)) {
          if (!is.name(old_formals[missing_args[k]]))
            dots <- c(dots, old_formals[missing_args[k]])
        }
      }
      tilde[[2]] <- left
    }
  } 
  
  fun <- do.call(makeFun, c(tilde, dots, list(suppress.warnings=TRUE)))
  
  dfun <- Deriv::Deriv(fun, x=vars[1])
    
  if (!missing(.order)) {
    .order <- .order-1
    while(.order > 0) {
      dfun <- Deriv::Deriv(dfun, x=vars[1])
      .order <- .order-1
    }
  } else {
    vars <- vars[-1]
    while(length(vars) > 0) {
      dfun <- Deriv::Deriv(dfun, x=vars[1])
      vars <- vars[-1]
    }
  }
  # combine new default values with old formals
  new_defaults <- list(...)
  newformals <- formals(fun)
  newformals[names(new_defaults)] <- new_defaults

  formals(dfun) <- newformals

  dfun
}

