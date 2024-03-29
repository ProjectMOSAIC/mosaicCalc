% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traj_plot_3D.R
\name{traj_plot_3D}
\alias{traj_plot_3D}
\title{Simple 3D plot of a trajectory}
\usage{
traj_plot_3D(x, y, z, soln, domain = NULL, npts = 1000)
}
\arguments{
\item{x}{Name of one of the state variables to be plotted.}

\item{y}{Similar to \code{x}}

\item{z}{Similar to \code{y} and \code{x}}

\item{soln}{Solution output from \code{integrateODE()}}

\item{domain}{Optional list like \code{domain=domain(t=c(0,100))}. By default, this
will be inferred from \code{soln}}

\item{npts}{Number of points at which to evaluate the solution.}
}
\description{
Takes a trajectory with three state variables as produced by \code{integrateODE()}
and plots out in a 3-dimensional perspective plot, which can be rotated.
}
\examples{
Lorenz <- makeODE(dx ~ sigma*(y-x), dy ~(x*(rho-z) - y), dz ~ (x*y - beta*z), 
                  rho=28, sigma=10, beta = 8/3)
T1 <- integrateODE(Lorenz, domain(t=0:50), x=-5, y=-7, z=19.4)
traj_plot_3D(x, y, z, T1, npts=5000)
}
