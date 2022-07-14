#' Create a data frame for a circle marking the curvature of a function.
#'
#' @returns `inscribed_circle()` return a data frame with `x` and `y` components describing the inscribed circle
#' located at the point $(x_0, f(x_0))$. Plot this with `gf_path(y ~ x)`
#' `curvature_function()` returns a function that gives the curvature of the specified
#' function for any input $x$. It's much like `D()` in the way it is used.
#'
#' @param ftilde A tilde expression defining a function
#' @param x0 The value for the input at which the curvature is to be calculated.
#' 
#' @export
inscribed_circle <- function(ftilde, x0=0) {
  ff <- makeFun(ftilde)
  df <- D(ff(x) ~ x)
  ddf <- D(ff(x) ~ x & x)
  K <- abs(ddf(x0)) / abs(1+ df(x0)^2)^(3/2)
  direction <- sign(ddf(x0))
  angle <- sign(ddf(x0))*pi + atan2(-1, df(x0))
  centerx <- x0 + direction*cos(angle)/K
  centery <- ff(x0) + direction*sin(angle)/K
  #print(c(K, angle, centerx, centery, f(x0)))
  angles <- seq(0, 2*pi, length=100)
  data.frame(x = centerx + cos(angles)/K,
             y = centery + sin(angles)/K)

}


