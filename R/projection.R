#' Utilities for vector/matrix calculations
#'
#' Convenience functions for basic operations relating to vector projection. These use
#' the *MOSAIC Calc* conventions that require vectors to be one column matrices.

#'
#' - `vec(1,2,3)` makes a **column** vector
#' - `rvec(n)` random vector with <n> elements
#' - `vec_names(v, c("one", "two", ...))` gives names to the
#' vectors in a matrix.
#' - `vbind()` collects column vectors into a matrix. Will also concatenate
#' matrices by column.
#' - `M_cols()` operates on the columns of a matrix one-by-one. Operations
#' include `"center"`, `"unit"`, `"unitcenter"`, which scale the columns, and
#' `"mean"`, `"var"`, `"vlen"` which return a scalar for each column.)
#' - `%dot%` infix operator for dot product. Raw R vectors will work
#' - `%cang%` like `%dot%` but returns the cosine of the angle
#' - `%onto` infix projects left arg onto space (vector or matrix) defined
#' by right argument.
#' - `%perp%` infix. Like `%onto%` but returns the reciprocal. 
#' - `vbind()` collects column vectors into a matrix
#' - `Q(A)` the Q part of a qr decomposition
#' - `R_from_Q(A)` the R part of a qr decomposition. Can apply 
#' directly to the output of `Q(A)`
#' 
#'
#' Convenience functions for basic operations relating to vector projection. These use
#' the *MOSAIC Calc* conventions that require vectors to be one column matrices.
#'
#' @param A a matrix
#' @param b a column vector
#' @param u a row vector, but a column vector is acceptable too
#' @param nrow number of rows for matrix
#' @param ncol number of columns for matrix
#' @param metric metric to use for matrix norm
#' @param fun either a function producing random numbers or, for `M_cols()` 
#' the name of action to perform on each column. 
#' @param \ldots values to put in `vec()` **or** params for random
#' number generator
#' 
#' @rdname vector_matrix
#' @export
vec <- function(...) {
  vals <- list(...) |> unlist()
  matrix(vals, ncol=1)
}
#' @rdname vector_matrix
#' @export
rvec <- function(n = 3L, fun=rnorm, ...) {
  suppressWarnings(n <- as.integer(n))
  if (!is.integer(n) || n <= 0) stop("<n> must be a positive integer")
  
  vec(fun(n, ...))
}
#' @rdname vector_matrix
#' @export
M_cols <- function(M, fun = "center") {
  choices <- c("center", "unit",
               "unitcenter", "mean", "var", "vlen")
  
  str <- as.character(substitute(fun))
  if (str[1] %in% choices) {
    fun <- switch (str[1],
                   center = \(x) (x - mean(x, na.rm = TRUE)),
                   unit = \(x) x / sqrt(sum(x^2)),
                   mean = \(x) mean(x, na.rm = TRUE),
                   var = \(x) var(x, na.rm = TRUE),
                   len = \(x) sqrt(sum(x*x)),
                   len2 = \(x) sum(x*x),
                   unitcenter = function(x) {
                     v <- x - mean(x, na.rm = TRUE)
                     v / sqrt(sum(v^2))
                   }
    )
  }
  if (!is.function(fun)) stop("Not a recognized function.")
  first <- fun(M[,1])
  res <- matrix(0, length(first), ncol(M))
  for (k in 1:ncol(M)) {
    res[, k] <- fun(M[,k])
  }
  res
}
#' @rdname vector_matrix
#' @export
# create a model matrix pipe style
data_M <- function(.data, tilde) {
  M <- model.matrix(tilde, data = .data |> tibble::remove_rownames())
  M[ , - which(colnames(M) == "(Intercept)"), drop = FALSE]
}
#' @rdname vector_matrix
#' @export
veclen <- function(v) sqrt(v %dot% v)
# not needed. Use M_cols() instead unitvec <- function(vec) vec/length(vec)
#' @rdname vector_matrix
#' @export
`%cang%` <- function(v, w) {(v %dot% w) / (veclen(v) * veclen(w))}
#' @rdname vector_matrix
#' @export
Q <- function(A) {
  if (inherits(A, "qr_complex")) qr.Q(A)
  else {
    tmp <- qr(A)
    Q <- qr.Q(tmp)
    attributes(Q) <- list(R = qr.R(tmp))
    class(Q) <-c(class(Q), "qr_complex")
  }
  
  return(Q)
}

#' @rdname vector_matrix
#' @export
R_for_Q <- function(A) {
  if (inherits(A, "qr_complex")) return (attributes(A)$R)
  else return(qr.R(qr(A))) 
}

#' @rdname vector_matrix
#' @export
vbind <- cbind

#' @rdname vector_matrix
#' @export
vec_names <- `colnames<-`
#'
#' @rdname vector_matrix
#' @export
`%dot%` <- function(u, b) {
  u <- matrix(u, nrow=1) # force u to row vector
  b <- matrix(b, ncol=1)
  if (nrow(b) != ncol(u))
    stop("Vector <u> must have the same number of elements as vector <b>.")

  c(u %*% b)
}
#' @rdname vector_matrix
#' @export
`%onto%` <- function(b, A) {
  b <- matrix(b, ncol=1) # force a column vector
  if (!is.matrix(A))
    A <- matrix(A, nrow=nrow(b))
  else if (nrow(A) != nrow(b))
    stop("<A> must have same number of rows as <b>.")
  A %*% qr.solve(A, b)
}
#' @rdname vector_matrix
#' @export
`%perp%` <- function(b, A) {
  b <- matrix(b, ncol=1) # force a column vector
  b - (b %onto% A)
}
#' @rdname vector_matrix
#' @export
normalize <- function(A) {
  if (!inherits(A, "matrix"))
    stop("Argument <A> must be a matrix.")
  helper <- function(v) { v / sqrt(sum(v^2)) }
  apply(A, 2, helper)
}
#' @rdname vector_matrix
#' @export
as_magnitude <- function(A, metric=c("2", "O", "I", "F", "M")) {
  if (!inherits(A, "matrix"))
    stop("Argument <A> must be a matrix.")
  metric <- match.arg(metric)
  helper <- function(v) {Matrix::norm(v, type=metric)}

  apply(A, 2, helper)
}
#' @rdname vector_matrix
#' @export
rmat <- function(nrow = 3, ncol = 6, fun = rnorm) {
  matrix(runif(nrow * ncol), nrow = nrow, ncol = ncol)
  
}



