#' Basis sets for for function approximation
#'
#' These functions generate the mathematical functions for three
#' different basis sets: Fourier (sines), Legendre (orthogonal polynomials),
#' and Splines (low-order smooth approximation)
#'
#' @param x inputs at which to evaluate the functions (in the `_M` functions)
#' @param df number of basis functions to construct
#' @param left number giving left-hand boundary of the interval
#' @param right number giving right-hand boundary of the interval
#' @param fperiod number giving the fundamental period length for the Fourier basis
#' @param n number of fourier components to generate
#' @inheritParams splines::ns
#' 
#'
#' @details For each basis, there are two different forms for the
#' generating functions. Names ending in `_set` create a set of functions with
#' arguments `x` and `n`, where integer `n` provides an index into the set.
#' The same names with a `_M` suffix produce a model matrix
#' corresponding to a specified set of x values. These are useful
#' with `lm()` and similar model-building functions in the same way that
#' `poly()` and `ns()` are useful. (`ns_M()` is just an alias for `splines::ns()`.) Like
#' `poly()` and `ns()`, the `_M` suffix functions do *NOT* include an
#' intercept column.
#'
#' @return The `_M` functions return a model matrix. The `_set` functions
#' return a function with arguments `x` and `n`. The integer `n` specifies
#' which function to use, while `x` is the set of values at which to evaluate
#' that function.
#'
#' @name basis_sets
#' @rdname basis_sets
#' @export
legendre_set <- function(df = 3, left=-1, right=1) {
  mid = (left+right)/2
  halflength  = (right - left)/2
  polys <- orthopolynom::legendre.polynomials(df, TRUE)
  polys <- lapply(polys, as.function)
  function(x, n) {
    if (length(n) != 1)
      stop("Must use a single value for n.")
    if (n != round(n) || n > df || n < 1)
      stop("n  must be an  integer greater than  zero and less than df.")
    polys[[n]]((x - mid)/halflength)
  }
}
#' @rdname basis_sets
#' @export

legendre_M <- function(x, df, left = -1, right = 1) {
  polys <- legendre_set(df, left, right)
  M <- matrix(0, nrow =  length(x), ncol = df-1)
  for (k in 2L:df)
    M[,k-1] <- polys(x,  as.integer(k))

  M
}
#' @rdname basis_sets
#' @export

ns_set <- function(df = 3, left = -1, right = 1) {
  x <- seq(left, right, length=1000)
  M <- splines::ns(x, df=df)
  funs <- list()
  for (k in 1:df) funs[[k]] <- approxfun(x, M[,k])
  function(x, n) {
    if (length(n) != 1)
      stop("Must use a single value for n.")
    if (n != round(n) || n > df || n < 1)
      stop("n  must be an  integer between 1 and ", df)
    funs[[n]](x)
  }
}
#' @rdname basis_sets
#' @export

# Like splines::ns(), but for fourier basis
fourier_M <- function(x, n, fperiod = NULL) {
  nx <- names(x)
  x <- as.vector(x)
  nax <- is.na(x)
  if (nas <- any(nax))
    x <- x[!nax]
  if (is.null(fperiod)) {
    fperiod <- diff(extendrange(x, f=c(0, 1/length(x))))
  }
  M <- matrix(0, ncol = 2*n, nrow = length(x))

  for (k in 1:n) {
    M[, 2*k - 1] <- sin(2*pi*x*k/fperiod)
    M[, 2*k] <- cos(2*pi*x*k/fperiod)
  }

  M
}

#' @rdname basis_sets
#' @export
ns_M <- splines::ns

# (Discrete) Fourier basis set
#' @rdname basis_sets
#' @export

fourier_set <- function(df, left = -1, right = 1) {
  base_period <- right - left
  funs <- list()
  for (index  in 1:df) {
    # Each period needs its own environment
    this_env <- new.env()
    this_env$index <- index
    S <- function(x) sin(2*pi*x*index / base_period)
    environment(S) <- this_env
    C <- function(x) cos(2*pi*x*index / base_period)
    environment(C) <- this_env
    funs[[2*index - 1]] <- S
    funs[[2*index]] <- C

  }
  # return(funs)
  function(x, n) {
    if (length(n) != 1)
      stop("Must use a single value for n.")
    if (n != round(n) || n > (2*df) || n < 1)
      stop("n  must be an  integer between 1 and ", 2*df)
    funs[[n]](x)
  }
}
