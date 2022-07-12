#' Construct a model matrix from data as if by hand
#'
#' The *MOSAIC Calculus* course includes a block on linear algebra.
#' As part of this block, we want to cover creating model matrices and evaluating
#' the model constructed from them. One way to do this is to introduce
#' `lm()` and the domain specific language for specifying model terms. However,
#' that introduces oddities. For instance, `lm(mpg ~ hp + hp^2, data = mtcars)` does
#' NOT add the quadratic term to the model matrix. Also, `lm()` doesn't produce
#' a residual or the model vector (ahem ... the fitted values) in the form of vectors.
#' That's fine if you're teaching statistical modeling, but in the *MOSAIC Calculus*
#' linear algebra block we are not teaching statistics, but the mathematical
#' pre-requisites to understanding statistics.
#'
#' Specifically for *MOSAIC Calculus*, we have added this `df2matrix()` function.
#' It serves much the same purpose as `cbind()`, that is, collecting vectors into a
#' matrix. But is has two additional features:
#'
#' 1. It has a `data=` argument so that it can refer to a data frame.
#' 2. It names the columns of the matrix with the code that was used to create
#' each column.
#'
#' The consequence of (2) is that the `x` vector produced by `qr.solve()` will have the
#' same names as the matrix. That helps in interpreting `x`. But those names
#' can also be used by `makeFun()` to generate a function from `x`.
#'
#' @param \dots Expressions, written in terms of the variable names in the data frame,
#' that are to be collected into the model matrix.
#' @param data The data frame from which to which the variable names will be bound.
#'
#' @details `1` is a good form in which to write the intercept term.
#'
#' @examples
#' A <- df2matrix(1, disp, log(hp), sin(cyl)*sqrt(hp), data = mtcars)
#' b <- df2matrix(mpg, data = mtcars)
#' x <- qr.solve(A, b)
#' f <- makeFun(x)
#' f(hp=3, disp=2, cyl=4)
#'
#' @export
df2matrix <- function(..., data=NULL) {
  columns <- enquos(...)

  column_names <- gsub("^\\~", "", as.character(columns))
  M <- lapply(columns, FUN = function(x) eval(rlang::get_expr(x), envir=data))
  M <- do.call(cbind, M)
  colnames(M) <- column_names

  M
}

#' @importFrom mosaicCore makeFun

#' @export
makeFun.numeric <- function(object, ...){
  namedx <- object
  if (inherits(namedx, "matrix") && dim(namedx)[2]==1) {
    namedx <- as.numeric(object)
    names(namedx) <- rownames(object)
  }
  # check if namedx is named. If not bawk.
  if (length(names(namedx)) < length(namedx))
    stop("Input to makeFun.numeric() must be a named numerical vector.")


  f <- function(){}
  b <- paste(as.numeric(namedx), "*", names(namedx), collapse="+")
  b <- gsub("\\+ ?\\-", "-", b)
  body(f) <- parse(text = b)
  argnames <- all.vars(parse(text=names(namedx)))
  newformals <- rep(alist(x=), length(argnames))
  names(newformals) <- argnames
  formals(f) <- newformals

  f
}
