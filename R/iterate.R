#' Iterate a function on an initial condition
#'
#' Iterates a function a specified number of times on an initial condition.
#'
#' @details The function `f` can take one or more arguments. The first of these
#' should represent the *state* of the dynamical system, e.g. x, or x and y, etc.
#' At the end of the argument list to `f` can come named parameters. The length of the
#' initial condition `x0` must match the number of state arguments. Numerical values for
#' parameters (if any) must be provided in the `...` slot.
#'
#' @param f a function of one or more state variables which returns a vector containing
#' those same state variables. Parameters to `f()`, if any, should be named and at
#' the end of the argument list. State variables should **not** have default values.
#' @param A As an alternative to `f` you can give a square matrix and iteration
#' will be done on the system `x[n+1] = A x[n]`. In lieu of a square matrix
#' you can also give a vector to be rendered into a square matrix rowwise.
#' @param x0 a vector with the numerical initial condition. There should be 1 component
#' in `x0` for each of the state variables in `f()`.
#' @param n an integer specifying the number of iterations
#' @param fargs list containing values for numerical parameters to the function `f()`
#'
#' @return A data frame with a column `.i` listing the iteration number
#' and columns for each component of the initial condition. There will be n+1
#' rows, the first for the initial condition and the remaining for the n iterations.
#'
#' @examples
#' Iterate(function(x, mu=3.5) mu*x*(1-x), x0=.232, n=10, list(mu=4)) # chaos
#' Iterate(function(x, y) c(x+y, x-y), x0 = c(1,1), n=10)
#' Iterate(function(x, y) c(x+y, x), x0=c(1,0), n=10) # fibonacci
#' Iterate(A = cbind(rbind(1, 1), rbind(1, 0)), x0=c(1,0), n=5) # fibonacci described by a matrix
#' @export
Iterate <- function(f=NULL, A=NULL, x0=0,  n=10, fargs=list()) {
  if (is.null(f)) {
    if (is.null(A)) stop("One of f or A must be specified.")
    if (inherits(A, "matrix")) { # make sure it's square
      if (diff(dim(A)) != 0) stop("Matrix A must be square.")
      nA <- dim(A)[1]
      if (nA != length(x0)) stop("x0 must be a vector commensurate with A")
    } else { # it's a vector. Convert to a matrix
      nA <- sqrt(length(A))
      if ((nA %% 1) != 0)
        stop("A must have enough components for a square matrix.")
      if (nA != length(x0))
        stop("x0 must be commensurate with A")

      A <- matrix(A, nrow=nA, byrow=TRUE)
    }
    if (length(names(x0)) == nA) vnames <- names(x0)
    else {
      if (nA < 7) vnames <- c("x", "y", "z", "w", "u", "v")[1:nA]
      else vnames <- paste0("x", 1:nA)
    }
    outnames <- c("n", vnames)
  } else { # it's a function or a formula
    if (inherits(f, "formula")) f <- makeFun(f)
    vnames <- names(formals(f))
    has_default <- sapply(formals(f), class) != "name"
    # handle built-ins like sin() where formals() doesn't work
    if (is.null(vnames)) vnames <- "x"

    if ((length(x0)+ length(fargs)) < length(vnames) - sum(has_default))
      stop("x0 must be a vector with one element for each argument to f.")
    outnames <- c("n", setdiff(vnames, vnames[has_default]))
  }

  res <- matrix(0, nrow=n+1, ncol=length(x0))
  res[1,] <- x0
  if (!is.null(f)) {
    for (k in 2:(n+1)) res[k,] <- do.call(f, c(as.list(res[k-1,]), fargs))
  } else {
    for (k in 2:(n+1)) res[k,] <- t(A %*% cbind(res[k-1,]))
  }
  out <- cbind(0:n, as.data.frame(res))
  names(out) <- outnames

  out
}
