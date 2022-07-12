#' Simple 3D plot of a trajectory
#'
#' Takes a trajectory with three state variables as produced by `integrateODE()`
#' and plots out in a 3-dimensional perspective plot, which can be rotated.
#'
#' @param x Name of one of the state variables to be plotted.
#' @param y Similar to `x`
#' @param z Similar to `y` and `x`
#' @param soln Solution output from `integrateODE()`
#' @param domain Optional list like `domain=domain(t=c(0,100))`. By default, this
#' will be inferred from `soln`
#' @param npts Number of points at which to evaluate the solution.
#' @examples
#' Lorenz <- makeODE(dx ~ sigma*(y-x), dy ~(x*(rho-z) - y), dz ~ (x*y - beta*z), 
#'                   rho=28, sigma=10, beta = 8/3)
#' T1 <- integrateODE(Lorenz, domain(t=0:50), x=-5, y=-7, z=19.4)
#' traj_plot_3D(x, y, z, T1, npts=5000)
#' @export
traj_plot_3D <- function(x, y, z, soln, domain=NULL, npts=1000) {
  x <- substitute(x)
  y <- substitute(y)
  z <- substitute(z)
  if (is.null(domain)) {
    extent <- environment(soln[[x]])$x
    domain <- list(t = range(extent))
  }
  # create the times at which to evaluate the solutions
  times <- seq(domain$t[[1]], domain$t[[2]], length=npts)
  Pts <- tibble(
    xpts = soln[[x]](times),
    ypts = soln[[y]](times),
    zpts = soln[[z]](times)
  )
  names(Pts) <- as.character(c(x, y, z))
  f1 <- as.formula(paste("~", x))
  f2 <- as.formula(paste("~", y))
  f3 <- as.formula(paste("~", z))
  plotly::plot_ly(Pts, x = f1, y = f2, z = f3, type = 'scatter3d', mode = 'lines',
                  opacity = 1, line = list(width = 6, reverscale = FALSE))
}

