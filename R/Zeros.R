#' Finds zeros of a function within a specified domain
#'
#' @param tilde tilde expression defining a function, suitable
#' for `makeFun()`
#' @param domain specification of domain, as in `slice_plot()`
#' @param nsegs Subdivide the domain into this many segments, looking for a zero 
#' in each of those segments. This helps to find multiple zeros.
#' @param \dots assignments for parameters in the tilde expression
#'
#' @returns A data frame with two columns. The first has the name
#' of the input in the tilde expression, and gives the values
#' for that input at which the function is approximately zero.
#' The second column, `.output.` gives
#' the actual value of the function at the inputs in the first column.
#'
#' @examples
#' Zeros(a*x + b ~ x, a=1, b=2)
#'
#' @export
Zeros <- function(tilde, domain=NULL, nsegs=131, ...) {

  f <- makeFun(tilde, ..., suppress.warnings=TRUE)
  unbd <- unbound(f)
  if (length(unbd) > 1) 
    stop(paste("Must give numerical values for all parameters.", 
               paste0("<", unbd, ">", collapse=" & "),
               "are unbound."))
  
  xpts <- if (is.null(domain)) {
    points <- c(2^(0:100))
    sort(c(-points, -0.011101, 0.010101, points))
  } else {
    seq(min(unlist(domain)), max(unlist(domain)), length=nsegs)
  }
  foundx <- rep(NA, nsegs)
  foundfx <- rep(NA, nsegs)
  for (k in 2:length(xpts)) {
    raw <- try(uniroot(f, c(xpts[k-1], xpts[k])),
               silent = TRUE)
    if (inherits(raw, "try-error")) next
    foundx[k] <- raw$root
    foundfx[k] <- raw$f.root
  }

  result <- tibble::tibble(
    x = foundx[!is.na(foundx)],
    .output. = f(.data$x)
  ) %>% unique() %>%
    filter(abs(.data$.output.) <= 10*median(abs(.data$.output.), na.rm=TRUE)) # avoid singularities
  names(result)[1] <- all.vars(rhs(tilde))[1]

  result
}
