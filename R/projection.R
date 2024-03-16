#' Utilities for vector calculations
#'
#' `%dot%`, `%onto%`, and `%perp%` are infix operators.
#' The left-hand argument is a vector.
#'
#' Convenience functions for basic operations relating to vector projection. These use
#' the *MOSAIC Calc* conventions that require vectors to be one column matrices.
#'
#' @returns either a number (for `%dot%`) or a vector
#'
#' @param A a matrix
#' @param b a column vector
#' @param u a row vector, but a column vector is acceptable too
#' @param metric metric to use for matrix norm
#'
#' @name vectors
NULL
#> NULL
#'
#' @rdname vectors
#' @export
`%dot%` <- function(u, b) {
  u <- matrix(u, nrow=1) # force u to row vector
  b <- matrix(b, ncol=1)
  if (nrow(b) != ncol(u))
    stop("Vector <u> must have the same number of elements as vector <b>.")

  c(u %*% b)
}
#' @rdname vectors
#' @export
`%onto%` <- function(b, A) {
  b <- matrix(b, ncol=1) # force a column vector
  if (!is.matrix(A))
    A <- matrix(A, nrow=nrow(b))
  else if (nrow(A) != nrow(b))
    stop("<A> must have same number of rows as <b>.")
  A %*% qr.solve(A, b)
}
#' @rdname vectors
#' @export
`%perp%` <- function(b, A) {
  b <- matrix(b, ncol=1) # force a column vector
  b - (b %onto% A)
}
#' @rdname vectors
#' @export
normalize <- function(A) {
  if (!inherits(A, "matrix"))
    stop("Argument <A> must be a matrix.")
  helper <- function(v) { v / sqrt(sum(v^2)) }
  apply(A, 2, helper)
}
#' @rdname vectors
#' @export
as_magnitude <- function(A, metric=c("2", "O", "I", "F", "M")) {
  if (!inherits(A, "matrix"))
    stop("Argument <A> must be a matrix.")
  metric <- match.arg(metric)
  helper <- function(v) {Matrix::norm(v, type=metric)}

  apply(A, 2, helper)
}
