#' Numerical Derivatives
#'
#' Constructs the numerical derivatives of mathematical expressions
#'
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu})
#'
#'
#' @param tilde a mathematical expression (see examples and \code{\link{plotFun}})
#' @param \dots additional parameters, typically default values for mathematical parameters
#' @param .h numerical step size to enforce.
#' 
#' @return a function implementing the derivative as a finite-difference approximation.
#' This has a second argument, `.h`, that allow the finite-difference to be set when evaluating
#' the function. The default values are set for reasonable numerical precision.
#'
#' @details
#' Uses a simple finite-difference scheme to evaluate the derivative.  The function created
#' will not contain a formula for the derivative.  Instead, the original function is stored
#' at the time the derivative is constructed and that original function is re-evaluated at the
#' finitely-spaced points of an interval.  If you redefine the original function, that won't affect
#' any derivatives that were already defined from it.
#' Numerical derivatives, particularly high-order ones, are unstable.  The finite-difference parameter
#' `.h` is set, by default, to give reasonable results for first- and second-order derivatives.
#' It's tweaked a bit so that taking a second derivative by differentiating a first derivative
#' will give reasonably accurate results.  But,
#' if taking a second derivative, much better to do it in one step to preserve numerical accuracy.
#'
#' @note WARNING: In the expressions, do not use variable names beginning with a dot, particularly \code{.f} or \code{.h}
#'
#' @examples
#' g = numD( a*x^2 + x*y ~ x, a=1)
#' g(x=2,y=10)
#' gg = numD( a*x^2 + x*y ~ x&x, a=1)
#' gg(x=2,y=10)
#' ggg = numD( a*x^2 + x*y ~ x&y, a=1)
#' ggg(x=2,y=10)
#' h = numD( g(x=x,y=y,a=a) ~ y, a=1)
#' h(x=2,y=10)
#' f = numD( sin(x)~x)
#' # slice_plot( f(3,.h=hlim)~h, bounds(h=.00000001 :000001)) %>% gf_hline(yintercept = cos(3))
#'

# Strategy:
# The various dfdx, d2fdx2, d2fdxdy functions create a new function.
# The new function grabs the function f constructed from the formula
# and grabs the arguments, adding and subtracting the finite-difference step h
# as appropriate, then evaluating f at the new points to find the finite difference.

#' @rdname numD
#' @export
numD <- function(tilde, ..., .h=NULL) {
  tildeEnv = environment(tilde) # where did the tilde come from?
  # translate the tilde into a function
  fun_define_tilde <- tilde
  fun_define_tilde[[3]] <- quote(.) # Get the arguments in the conventional order.
  f <- makeFun(fun_define_tilde, ...)
  environment(f) <- tildeEnv  # was parent.frame()
  # find the variables with respect to which the derivative is to be taken
  # keeping repeated names so that 2nd derivatives can be spotted.
  dvars <- all.vars(rlang::f_rhs(tilde), unique=FALSE)
  # What sort of derivative?
  if (length(dvars)==1) { 
    #Simple first derivative
    the_h <- ifelse(is.null(.h), 0.000001, .h)
    res = make_dfdx( f, dvars[1], .h = the_h) %>% 
      bind_params(formals(f))
  }else if (length(dvars==2) && dvars[1]==dvars[2]) {
    # Second unmixed partial
    the_h <- ifelse(is.null(.h), 0.0001, .h)
    res = make_d2fd2x( f, dvars[1], .h = the_h) %>% 
      bind_params(formals(f))
  } else if (length(dvars)==2) {
    # mixed partial
    the_h <- ifelse(is.null(.h), 0.001, .h)
    res = make_d2fdfdy(f, dvars[1], dvars[2], .h = the_h) %>% 
      bind_params(formals(f))
  } else if (length(dvars)>2){
    stop("Derivative order greater than 2 not yet implemented.")
  }
  
  res
}
# =================
# Formal arguments are named to avoid conflicts with the contents of the mathematical function
# whose derivative is sought.  Similarly for the others: d2fdx2, d2fdxdy
#
# @param f function to be differentiated
# @param .wrt character string naming the variable with respect to which
# differentiation is to be done
# @param .h the finite-difference step size
make_dfdx <- function(f, .wrt, .h = 0.000001) {
  right_args <- left_args <- names(formals(f))
  right_args[right_args==.wrt] <- glue::glue("{.wrt} + .h")
  right_args <- paste0(right_args, collapse=", ")
  left_args[left_args==.wrt] <- glue::glue("{.wrt} - .h")
  left_args <- paste0(left_args, collapse=", ")
  command <- glue::glue("function({.wrt}){{(f({right_args}) - f({left_args}))/(2*.h)}}") 
  dfun <- eval(parse(text=command))
  formals(dfun) <- c(formals(f), list(.h=.h))
  
  conventional_argument_order(dfun, ".h")
}
# Create the mixed partials function
make_d2fdfdy <- function(f, .wrt1, .wrt2, .h) {
  # find the four corners of the box, right-upper, left-upper, etc.
  RU <- RB <- LU <- LB <- Args <- names(formals(f))
  RU[RU==.wrt1] <- glue::glue("{.wrt1} + .h")
  RU[RU==.wrt2] <- glue::glue("{.wrt2} + .h")
  RB[RB==.wrt1] <- glue::glue("{.wrt1} + .h")
  RB[RB==.wrt2] <- glue::glue("{.wrt2} - .h")
  LU[LU==.wrt1] <- glue::glue("{.wrt1} - .h")
  LU[LU==.wrt2] <- glue::glue("{.wrt2} + .h")
  LB[LB==.wrt1] <- glue::glue("{.wrt1} - .h")
  LB[LB==.wrt2] <- glue::glue("{.wrt2} - .h")
  RU_args <- paste0(RU, collapse=", ")
  RB_args <- paste0(RB, collapse=", ")
  LU_args <- paste0(LU, collapse=", ")
  LB_args <- paste0(LB, collapse=", ")
  # Character string describing the new function
  command <- glue::glue("function({.wrt1}, {.wrt2}){{((f({RU_args}) + f({LB_args})) - (f({RB_args}) + f({LU_args})))/(4*.h^2)}}") 
  # create the function itself from the character string
  dfun <- eval(parse(text=command))
  # put the complete list of arguments in place, including .h
  formals(dfun) <- c(formals(f), list(.h=.h))
  # put args in conventional order, but with .h at the end
  conventional_argument_order(dfun, ".h")
}
# ==============
#
# @note Helper function for \code{numD} for second-order derivs

make_d2fd2x <- function(f, .wrt, .h = 0.000001) {
  right_args <- left_args <- args <- names(formals(f))
  args <- paste0(args, collapse=", ")
  right_args[right_args==.wrt] <- glue::glue("{.wrt} + .h")
  right_args <- paste0(right_args, collapse=", ")
  left_args[left_args==.wrt] <- glue::glue("{.wrt} - .h")
  left_args <- paste0(left_args, collapse=", ")
  command <- glue::glue(
    "function({.wrt}){{(f({right_args}) + f({left_args}) - 2*f({args}))/(.h^2)}}") 
  dfun <- eval(parse(text=command))
  formals(dfun) <- c(formals(f), list(.h=.h))
  
  conventional_argument_order(dfun, ".h")
}
