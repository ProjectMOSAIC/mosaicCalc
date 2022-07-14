#' Integrate a function
#'
#' Calculates the definite integral of a function. That is, the result of the
#' integration will be a number. There can be no free parameters in the function
#' being integrated. (If you want free parameters, use `antiD()`.) `Integrate()` can
#' handle integration over up to 3 variables.
#'
#' @details For functions constructed as a spline interpolant, `Integrate()` can handle
#' more segments than `antiD()`. It's reasonable to do up to 2000 segments with `Integrate()`,
#' whereas `antiD()` handles only about 100.
#'
#' @param tilde A tilde expression describing the function.
#' @param domain The domain over which to perform the integration
#' @param \dots values assigned to free parameters in the tilde expression, e.g. a=1
#' @param tol numerical tolerance for integration, see `stats::integrate()`.
#' 
#' @examples 
#' Integrate(dnorm(x) ~ x, domain(x=-2:2))
#' Integrate(dnorm(x, sd=sigma) ~ x, domain(x=-2:2), sigma=2)
#' Integrate(sqrt(1- x^2) ~ x, domain(x=-1:1)) # area of semi-circle
#' 
#' @export

# CHANGE THIS SO THAT IT HANDLES SYMBOLIC INTEGRALS symbolically if they exist
# THAT WILL REPLACE symbolicInt.R
# This will make the difference between antiD() and Integrate() only whether they
# take numeric values for the w.r.t. input or a domain() in terms of the w.r.t. input.

Integrate <- function(tilde, domain, ...,  tol=0.00001) {
  if (missing(domain))
      stop("Must specify either domain or both from and to arguments.")
  
  f <- makeFun(tilde, suppress.warnings=TRUE,
               strict.declaration = FALSE, 
               use.environment = FALSE) %>% 
    bind_params(list(...))
  ivars <- all.vars(rhs(tilde))

  # Check the domain
  if (length(domain) != length(ivars))
    stop("Must provide one domain element for each variable of integration.")
  if (any(sapply(domain, length) != 2))
    stop("Each domain element must have 2 components: bottom & top.")
  if (any(! ivars %in% names(domain)))
    stop("Domain names do not match names of function arguments.")
  # Get the order right
  domain <- domain[ivars]
  lowerLimit <- sapply(domain, min)
  upperLimit <- sapply(domain, max)
  # handle sign of result
  # For whatever reason, on splines error is smaller if integral's
  # lower bound is less than the upper bound
  multiplier <- 1
  for (k in 1:length(domain)) {
    if (lowerLimit[k] != domain[[k]][1]) multiplier <- -multiplier
  }

  # Check that it's just a function of the variables being integrated over
  unbound_parameters <- setdiff(unbound(f), ivars)
  if (length(unbound_parameters) > 0) {
    msg <- paste(
      "Parameters", paste0("<", unbound_parameters, ">", collapse=", "),
      "have not yet been bound to numbers."
    )
    stop(msg)
  }
  # Create a function with a vector argument
  # for functions of one variable, this is already the case
  if (length(ivars) == 1) {
    vf <- f
  } else if (length(ivars) == 2) {
    vf <- function(v) { f(v[1], v[2]) }
  } else if (length(ivars) == 3) {
    vf <- function(v) { f(v[1], v[2], v[3]) }
  } else {
    stop("Integrate() handles only functions with 3 or fewer arguments.")
  }


  res <- cubature::hcubature(vf, lowerLimit, upperLimit, tol = tol,
                      maxEval = 100000)

  multiplier * res$integral
}
  
