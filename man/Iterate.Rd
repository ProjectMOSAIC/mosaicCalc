% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iterate.R
\name{Iterate}
\alias{Iterate}
\title{Iterate a function on an initial condition}
\usage{
Iterate(f = NULL, A = NULL, x0 = 0, n = 10, fargs = list())
}
\arguments{
\item{f}{a function of one or more state variables which returns a vector containing
those same state variables. Parameters to \code{f()}, if any, should be named and at
the end of the argument list. State variables should \strong{not} have default values.}

\item{A}{As an alternative to \code{f} you can give a square matrix and iteration
will be done on the system \verb{x[n+1] = A x[n]}. In lieu of a square matrix
you can also give a vector to be rendered into a square matrix rowwise.}

\item{x0}{a vector with the numerical initial condition. There should be 1 component
in \code{x0} for each of the state variables in \code{f()}.}

\item{n}{an integer specifying the number of iterations}

\item{fargs}{list containing values for numerical parameters to the function \code{f()}}
}
\value{
A data frame with a column \code{.i} listing the iteration number
and columns for each component of the initial condition. There will be n+1
rows, the first for the initial condition and the remaining for the n iterations.
}
\description{
Iterates a function a specified number of times on an initial condition.
}
\details{
The function \code{f} can take one or more arguments. The first of these
should represent the \emph{state} of the dynamical system, e.g. x, or x and y, etc.
At the end of the argument list to \code{f} can come named parameters. The length of the
initial condition \code{x0} must match the number of state arguments. Numerical values for
parameters (if any) must be provided in the \code{...} slot.
}
\examples{
Iterate(function(x, mu=3.5) mu*x*(1-x), x0=.232, n=10, list(mu=4)) # chaos
Iterate(function(x, y) c(x+y, x-y), x0 = c(1,1), n=10)
Iterate(function(x, y) c(x+y, x), x0=c(1,0), n=10) # fibonacci
Iterate(A = cbind(rbind(1, 1), rbind(1, 0)), x0=c(1,0), n=5) # fibonacci described by a matrix
}
