#' Create a quadratic spline (inefficiently)
#'
#' A handmade function to construct quadratic splines.
#'
#' @details Unless you have a good reason otherwise, you should be using `spliner()`,
#' which generates cubic splines,
#' rather than `qspliner()`.  `qspliner()` is intended only for demonstration purposes.
#'
#' @param tilde A tilde expression of the form `y ~ x` specifying the output variable (on the LHS)
#' and the input variable (on the RHS). Together with `data`, these (x,y) pairs will be the
#' knots for the spline.
#' @param data A data frame containing the variables in `tilde`.
#' @param free A number specifying the slope of the output function at the last knot. Default is 0: that is, flat.
#'
#' @examples
#' Pts <- tibble(x = seq(-4,4, by=.7), y = dnorm(x))
#' f <- qspliner(y ~ x, data = Pts)
#' slice_plot(dnorm(x) ~ x, domain(x=-4:4)) %>%
#'   slice_plot(f(x) ~ x, color= "blue") %>%
#'   gf_point(y ~ x, data = Pts, color = "orange", size=4, alpha=0.3) %>%
#'   gf_lims(y= c(NA,.5))
#' @export
qspliner <- function(tilde, data, free=0) {
  yname <- all.names(tilde[[2]])[1]
  tname <- all.names(tilde[[3]])[1]
  y <- data[[yname]]
  times  <- data[[tname]]
  inds  <- order(times)
  times <- times[inds]
  y     <- y[inds]
  nsegs <- nrow(data) - 1
  M <- qsplineM(times)
  a <- y[-length(y)]
  b <- c(y[-1] - a, rep(0, nsegs-1), free)
  X <- qr.solve(M, b)
  acoefs <- a
  bcoefs <- X[1:nsegs]
  ccoefs <- X[-(1:nsegs)]
  function(t) {
    res <- 0 * t
    for(k in 1:length(t)) {
      if (t[k] < times[1]) seg <- 1
      else if (t[k] >= times[length(times)]) seg <- length(times) -1
      else {
        seg <- max(which(times <= t[k]))
      }
      offset <- t[k] - times[seg]
      res[k] <- acoefs[seg] + bcoefs[seg]*offset + ccoefs[seg]*offset^2
    }
    return(res)
  }
}

qsplineM <- function(times) {
  t <- times[-1] - times[-length(times)]
  nsegs <- length(t)
  top <- cbind(diag(t), diag(t^2))
  left <- suppressWarnings(
    matrix(c(1,-1, rep(0, nsegs-1)),
           nrow=nsegs,ncol=nsegs, byrow=TRUE)
  )
  bottom <- cbind(left, 2*diag(t))
  rbind(top, bottom)
}
