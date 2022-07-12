# #' Find the symbolic integral of a formula
# #'
# #' @param form an object of type formula to be integrated.
# #' Rhs of formula indicates which variable to
# #' integrate with respect to.  Must only have one variable.
# #' @param \ldots extra parameters
# #'
# #' @details This symbolic integrator recognizes simple polynomials and functions such as
# #' \code{sin}, \code{cos}, \code{tan}, \code{sinh}, \code{cosh}, \code{tanh}, \code{sqrt}, and \code{exp}.
# #'
# #' It will not perform more complicated substitutions
# #' or integration by parts.
# #'
# #' @return symbolicInt returns a function whose body is the symbolic antiderivative of
# #' the formula.  If this method does not recognize the formula, it will return an error.
#'
#' @importFrom methods getGroup
#'
#' @export
symbolicInt<- function(tilde, ...){
  if (length(tilde) != 3)
    stop("Must provide a two sided tilde expression. With-respect-to-variable(s) go on the right-hand side.")
  
  # First try Ryacas 
  f <- simpleYacasIntegrate(tilde, ...)
  if (is.function(f) && ! "AntiDeriv" %in% all.names(body(f))) return(f)

  return(function(x) 0*x) # for debugging purposes
  #First check if it's a polynomial.  If it is, simplify it.
  params <- setdiff(all.vars(tilde), all.vars(rhs(tilde)))
  if(length(params)==0)
    params <- ""
#   res <- try(.polyExp(lhs(tilde), all.vars(rhs(tilde)), params), silent=TRUE)
#   if(!inherits(res, "try-error")){
#     tilde <- .makePoly(tilde, res)
#   }
  antiDeriv <- symbolicAntiD(tilde, ...)

  #determine which letter the constant will be
  intc = LETTERS[!LETTERS[-(1:2)]%in%all.vars(tilde)][-(1:2)][1]

  antiDeriv <- .makeNice(antiDeriv)
  #add the constant into the expression
  antiDeriv[[2]] <- parse(text = paste(deparse(lhs(antiDeriv), width.cutoff=500), "+", intc, sep=""))[[1]]

  #make the integral tilde into a function
  intfun = eval(parse(text=paste("do.call(makeFun, list(antiDeriv, ..., ",intc, "=0))", sep=""))[[1]])
  return(intfun)
}

