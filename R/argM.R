#' Find local extreme points
#'
#' @param tilde specification of a function as in makeFun()
#' @param domain a domain to search in.
#' 
#' @examples
#' argM(x^2 ~ x, domain(x=-1:1))
#'
#' @export
argM <- function(tilde, domain) {
  # What are the variables?
  vars <- all.vars(rhs(tilde))
  f <- makeFun(tilde, 
               suppress.warnings = TRUE,
               strict.declaration = FALSE, 
               use.environment = FALSE)
  if (length(vars) == 1) {
    # single variable optimization
    mins <- optimize(f, range(domain))
    maxes <- optimize(f, range(domain), maximum=TRUE)

    res <- tibble::tibble(
      x = c(mins$minimum, maxes$maximum)) %>%
      mutate(.output. = f(.data$x), concavity=c(1, -1))
    names(res)[1] <- vars

    return(res)
  } else {
    args <- formals(f)
    missing <- setdiff(names(args), names(domain))
    if (length(missing) > 0) stop("Names of function arguments and of domain elements must match.")
    vf <- vector_arg(f)
    x0 <- sapply(domain, mean)[names(args)]
    best <- optim(x0, vf) # for maximum, set control=list(fnscale=-1)
    res <- as_tibble(as.list(best$par))
    res$.output. = best$value

     return(res)
  }
}

#' convert a function with separate arguments
#' into one with a single vector argument
#' For use with optim.
#' @param f a function with multiple arguments
vector_arg <- function(f) {
  arg <- formals(f)
  res <- function(params) {
    do.call(f, as.list(params))
  }
}
