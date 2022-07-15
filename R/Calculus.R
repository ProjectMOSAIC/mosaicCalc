#' Derivative and Anti-derivative operators
#'
#' Operators for computing derivatives and anti-derivatives as
#' functions.
#'
#' @rdname Calculus
#' @importFrom stats runif uniroot optimize median as.formula na.omit integrate optim quantile
#' @importFrom mosaicCore rhs lhs makeFun parse.formula
#' @importFrom mosaic inferArgs expandFun fitModel spliner
#' @importFrom MASS fractions
#' @importFrom dplyr bind_rows
#' @importFrom Deriv Deriv
#' @importFrom Ryacas yac as_r
#' @importFrom orthopolynom legendre.polynomials
#' @importFrom glue glue
#' @importFrom tibble tibble tribble
#' @importFrom Matrix norm
#' @importFrom grDevices extendrange
#' @importFrom utils capture.output
#' @importFrom sp point.in.polygon
#'
#'
#' @param tilde A tilde expression. The right side of a formula specifies
#'   the variable(s) with which to
#'   carry out the integration or differentiation.  On the left side should be
#'   an expression or a function that returns a numerical vector
#'   of the same length as its argument.
#'   The expression can contain unbound variables.  Functions
#'   will be differentiated as if the formula {f(x) ~ x} were specified
#'   but with \code{x} replaced by the first argument of \code{f}.
#'
#'
#' @param \dots Default values to be given to unbound variables in the expression \code{expr}.
#' See examples.#'  Note that in creating anti-derivative functions,
#' default values of "from" and "to" can be assigned.  They are to be written with
#' the name of the variable as a prefix, e.g. \code{y.from}.
#'
#' @param .hstep  horizontal distance between points used for secant slope
#'   calculation in numerical derivatives.
#'
#' @param add.h.control logical indicating whether the returned derivative function
#'   should have an additional parameter for setting .hstep.  Meaningful only for numerical
#'   derivatives.
#'   
#' @param .tol Tolerance for numerical integration. Unless you know what this means, don't
#' use this argument.
#'
#' @return For derivatives, the return value is a function of the variable(s)
#' of differentiation, as well as any other symbols used in the expression.  Thus,
#' \code{D(A*x^2 + B*y ~ x + y)} will compute the mixed partial with respect to x
#' then y (that is, \eqn{\frac{d^2 f}{dy\;dx}}{d2f/dydx}).  The returned value will be a function of x and y,
#' as well as A and B.  In evaluating the returned function, it's best to use the
#' named form of arguments, to ensure the order is correct.
#'
#' @details
#' \code{D} attempts to find a symbolic derivative for simple expressions, but
#' will provide a function that is a numerical derivative if the attempt at
#' symbolic differentiation is unsuccessful.  The symbolic derivative can be of
#' any order (although the expression may become unmanageably complex).  The
#' numerical derivative is limited to first or second-order partial derivatives
#' (including mixed partials).
#' \code{antiD} will attempt simple symbolic integration but if it fails
#' it will return a numerically-based anti-derivative.
#'
#' \code{antiD} returns a function with the same arguments as the
#' expression passed to it.  The returned function is the anti-derivative
#' of the expression, e.g., antiD(f(x)~x) -> F(x).
#' To calculate the integral of f(x), use F(to) - F(from).
#'
#' @examples
#' D(sin(t) ~ t)
#' D(A*sin(t) ~ t )
#' D(A*sin(2*pi*t/P) ~ t, A=2, P=10) # default values for parameters.
#' f <- D(A*x^3 ~ x + x, A=1) # 2nd order partial -- note, it's a function of x
#' f(x=2)
#' f(x=2,A=10) # override default value of parameter A
#' g <- D(f(x=t, A=1)^2 ~ t)  # note: it's a function of t
#' g(t=1)
#' gg <- D(f(x=t, A=B)^2 ~ t, B=10)  # note: it's a function of t and B
#' gg(t=1)
#' gg(t=1, B=100)
#' f <- makeFun(x^2~x)
#' D(f(cos(z))~z) #will look in user functions also
#' @export
D <- function(tilde, ..., .hstep=NULL,add.h.control=FALSE){
    UseMethod("D")
}

#'
#' @export
D.default <- function(tilde, ..., .hstep=NULL,add.h.control=FALSE){
  # Defer to stats::D()
  tryCatch( return( stats::D(tilde, ...) ), error=function(e) {}  )
  stop( paste("First argument should be a formula that explicitly identifies the",
              "variable with respect to which the derivative is to be taken. ",
              "Example:  D(sin(x) ~ x).", sep ="\n  " ) )
}

#'
#' @export
D.formula <- function(tilde, ..., .hstep=NULL,add.h.control=FALSE){
  
  tildeEnv = environment(tilde) # where was the formula made?
  #Try to construct a symbolic derivative
  res = try(symbolicD(tilde, ...), silent=TRUE)
  if( inherits(res, "try-error") ){ # first symbolic attempt unsuccessful
    # replace by DTK on Sept. 28, 2021
    vars <- all.vars(rlang::f_rhs(tilde))
    inline_results <- inline_expr(rlang::f_lhs(tilde),
                as.name(vars[1]), as.name(vars[1]),
                environment(tilde))
    newformula <- tilde
    newformula[[2]] <- inline_results$ex
    args <- inline_results$args
    res = try(symbolicD(newformula, ...), silent=TRUE)
    if( inherits(res, "try-error") ) # second symbolic attempt unsuccessful
      res = numD( tilde, ..., .hstep=.hstep, add.h.control=add.h.control)
  }
  
  res <- conventional_argument_order(res) %>% bind_params(args)
  return(simplify_fun(res))
}

# ============================
#' @rdname Calculus
#'
#' @param lower.bound for numerical integration only, the lower bound used
#'
#' @param force.numeric If \code{TRUE}, a numerical integral is performed even when a
#' symbolic integral is available.
#' 
#' @param .tol Tolerance for numerical integration. Most users do not need this.
#'
#' @return a function of the same arguments as the original expression with a
#' constant of integration set to zero by default, named "C", "D", ... depending on the first
#' such letter not otherwise in the argument list.
#' @examples
#' antiD( a*x^2 ~ x, a = 3)
#' G <- antiD( A/x~x ) # there will be an unbound parameter in G()
#' G(2, A=1) # Need to bound parameter. G(2) will produce an error.
#' F <- antiD( A*exp(-k*t^2 ) ~ t, A=1, k=0.1)
#' F(t=Inf)
#' one = makeFun(1 ~ x)
#' by.x = antiD(one(x) ~ x)
#' by.xy = antiD(by.x(sqrt(1-y^2)) ~ y)
#' 4 * by.xy(y = 1) # area of quarter circle
#' @export
antiD <- function(tilde, ..., lower.bound=0, force.numeric=FALSE, .tol=0.0001){
  wrt <- all.vars(rhs(tilde), unique=FALSE) # "with respect to" variable name
  if (length(wrt) != 1)  stop("Integration with respect to multiple variables not supported directly.")

  # if tilde is a call to a one-line function, substitute the body of the function
  vars <- all.vars(rlang::f_rhs(tilde))
  from_inline <- inline_expr(rlang::f_lhs(tilde),
                                as.name(vars[1]), as.name(vars[1]),
                                environment(tilde))
  tilde[[2]] <- from_inline$ex
  args <- from_inline$args
  
  if (!force.numeric){ # Try symbolic integral
    # First try Ryacas 
    Fun <- try(simpleYacasIntegrate(tilde, ...), silent=TRUE)
    if (is.function(Fun) && ! "AntiDeriv" %in% all.names(body(Fun))) return(Fun)
  }
  # Do integral numerically
  res <- makeNumericalAntiD(tilde, wrt, lower.bound=lower.bound, .tol=.tol, ...) 
  res <- res %>% bind_params(args)
  
  return(conventional_argument_order(res))
}

# helper function for evaluating a function
evalF <- function(fun, ...) {
  do.call(fun, list(...))
}

makeNumericalAntiD <- function(tilde, wrt, lower.bound, .tol, ...) {
  new_fun_formals <- formals_from_expr(tilde[[2]])
  # new_fun_formals[[wrt]] <- NULL # keep
  dots <- list(...)
  for (nm in names(dots)) new_fun_formals[[nm]] = dots[[nm]]
  
  free_params <- names(new_fun_formals)
  if (length(free_params) == 0) assign <- ""
  else assign <- paste0(paste(free_params, "=", free_params, collapse=", "), ", ")
  
  # Figure out a name for the constant of integration
  intC <- LETTERS[-(1:2)][!LETTERS[-(1:2)]%in% names(new_fun_formals)][1]
  intC_list <- list(C=0)
  names(intC_list) <- intC

  # Note: No comma after {assign} in the next line
  command <- glue::glue("{{F <- makeF({capture.output(tilde)[1]}); evalFun(F, {assign}.const={intC})}}")
  
  .F <- create_num_antiD(tilde, ..., lower = lower.bound, .tol=0.0001)
  makeF <- function(tilde) .F # very, very simple. Just hides .F
  evalFun <- function(F, ..., .const)  {do.call(F, c(list(...))) + .const}
  
  
  res <- function(){ }
  body(res) <- parse(text = command)  
  formals(res) <- c(new_fun_formals, intC_list)
  
  
  conventional_argument_order(res)
}
