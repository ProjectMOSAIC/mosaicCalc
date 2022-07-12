#' check whether a value is in a domain
#'
#' A convenience function to see if a value is inside
#' (or on the boundaries of) a domain or a set of domain segments.
#' In the case of multiple segments, the check is
#' whether `val` is within any of them.
#'
#' @param val the values to be checked
#' @param domain the domain
#' @param \dots additional domains
#'
#' @examples
#' is_in_domain(1:10, domain(x=5.5:8.5), domain(x=1:2))
#' is_in_domain(mtcars, domain(mpg=15:20, wt=2:4.5), domain(mpg=25:28))
#' @export
is_in_domain <- function(val, domain, ...) {
  if (missing(domain)) stop("Must specify at least one domain segment.")


  domain_set = c(list(domain), list(...))
  if (is.numeric(val)) {
    # Don't worry about the name (if any) given to the domain segment.
    inside <- rep(FALSE, length(val))
    for (k in 1:length(domain_set)) {
      dom <- range(domain_set[[k]])
      a <- min(dom)
      b <- max(dom)
      inside <- inside | (a <= val & val <= b)
    }
  } else if (is.data.frame(val)) {
    inside <- rep(FALSE, nrow(val))
    for (k in 1:length(domain_set)) {
      this_dom <- domain_set[[k]]
      in_this_one <- rep(TRUE, length(inside))
      vnames <- names(val)
      dnames <- names(this_dom)
      if (! all(dnames %in% vnames)) {
        extras <- dnames[!dnames %in% vnames]
        stop(paste("Domain description doesn't match data frame variables for",
                   paste(dnames, collapse=", ")))
      }
      for (v in dnames) {
        dom <- range(this_dom[[v]])
        a <- min(dom); b <- max(dom)
        in_this_one <- in_this_one &
          a <= val[[v]] & val[[v]] <= b
      }
      inside <- inside | in_this_one
    }
  } else {
    stop("val must be a numeric vector or a data frame")
  }

  inside

}
