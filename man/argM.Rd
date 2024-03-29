% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/argM.R
\name{argM}
\alias{argM}
\title{Find local extreme points}
\usage{
argM(tilde, domain)
}
\arguments{
\item{tilde}{specification of a function as in makeFun()}

\item{domain}{a domain to search in.}
}
\value{
A data frame with values for x, the function
output at those values of x, and the concavity.
}
\description{
Find local extreme points
}
\details{
End-points of the domain may be included in the output.
}
\examples{
argM(x^2 ~ x, domain(x=-1:1))

}
