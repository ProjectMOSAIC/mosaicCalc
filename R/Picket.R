#' Creates a "picket fence" of points for illustrating numerical integration
#'
#' @param domain domain of integration as used in `slice_plot()`, `Integrate()`, and the similar mosaicCalc functions
#' @param h number giving the width between pickets. Could also have been called `dt` or `dx`, and so on.
#' @param method determines the weights for each element in the picket
#'
#' @export
Picket <- function(domain, h=0.1, method=c("left", "right", "center", "trapezoid", "gauss")) {
  method <- match.arg(method)

  vnames <- names(domain)
  if (is.null(vnames)) {
    vnames <- "x"
    warning("Unspecified input name in domain. Using `x`. See help(\"domain\") to fix this.")
  }
  # translate domain into the bottom and top on the integrateion
  bottom <- domain[[1]][1]
  top <- domain[[1]][2]

  direction <- ifelse(bottom > top, -1, 1)

  P <- tibble::tibble(
    x = seq(bottom, top, by=direction*h),
    preweight = direction,
    weight = h
  )

  last_one <- P$x[nrow(P)]
  if (last_one == top) {
    if (method %in% c("left", "center")) {
      P <- P[-nrow(P),] # Get rid of last picket
      if (method == "center") P$x <- P$x + h/2
    } else if (method == "right") {
      P <- P[-1,] # get rid of first picket
    }
      # OTHER METHODS?
  } else {
    # need to trim the last element
      fraction <- (top - last_one)/h
      P$preweight[nrow(P)] <- P$preweight[nrow(P)] * fraction
  }
  P$weight <- P$weight * P$preweight


  names(P)[1:length(vnames)] <- vnames


  return(P)

}
