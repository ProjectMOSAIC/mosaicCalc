#' Construct a random function that is smooth
#' 
#' Sometimes it's nice to have a simple, smooth but curving function 
#' to illustrate some mathematical point or to support an exercise. `doodle_fun()`
#' generates such functions. The function support tends to be in -10 to 10. For the
#' mathematically adept ... the functions are linear combinations of gaussians 
#' with randomly selected centers, widths, and random coefficients. The `mosaic::rfun()` function
#' is very similar.
#' 
#' @param vars A right-sided tilde expression (like `~ x & y`) that indicates how many
#' inputs there should be and their names.
#' @param seed An integer setting the seed for random number generation.
#' @param n A positive integer. If non-zero, this is a hint about how many local 
#' maxes and mins there should be. Just a hint.
#' 
#' @examples 
#' # Two functions of two variables. They are different because the seeds are different.
#' f <- doodle_fun(~ x & y, seed = 101)
#' g <- doodle_fun(~ z & u, seed = 121)
#' @export
doodle_fun <- function (vars = ~x & y, seed = NULL, n = 0) 
{
  if (!is.null(seed)) 
    set.seed(seed)
  if (!inherits(vars, "formula")) 
    stop("Must provide a formula, e.g. ~x&y, to identify the variables")
  nmaxes <- ifelse(n == 0, ceiling(runif(1, min = 4, max = 10)), 
                   n)
  varnames <- all.vars(vars)
  nvars <- length(varnames)
  locs <- list()
  for (k in 1:nvars) locs[[k]] <- runif(nmaxes, min = -3, max = 3)
  signsmax <- runif(nmaxes, min = 3, max = 10) * sign(runif(nmaxes, 
                                                            min = -1, max = 1))
  xscales <- runif(nmaxes, min = 0.1, max = 5)
  if (nvars == 1) {
    f <- function() {
      x <- eval(parse(text = varnames[1]))
      res <- 0
      for (k in 1:nmaxes) {
        res <- res + signsmax[k] * exp(-(xscales[k] * 
                                           (x - locs[[1]][k])^2)/9)
      }
      return(res)
    }
  }
  if (nvars == 2) {
    f <- function() {
      x <- eval(parse(text = varnames[1]))
      y <- eval(parse(text = varnames[2]))
      res <- 0
      for (k in 1:nmaxes) {
        res <- res + signsmax[k] * exp(-(xscales[k] * 
                                           (x - locs[[1]][k])^2 + (y - locs[[2]][k])^2)/9)
      }
      return(res)
    }
  }
  if (nvars == 3) {
    f <- function() {
      x <- eval(parse(text = varnames[1]))
      y <- eval(parse(text = varnames[2]))
      z <- eval(parse(text = varnames[3]))
      res <- 0
      for (k in 1:nmaxes) {
        res <- res + signsmax[k] * exp(-(xscales[k] * 
                                           (x - locs[[1]][k])^2 + (y - locs[[2]][k])^2 + 
                                           (z - locs[[3]][k])^2)/9)
      }
      return(res)
    }
  }
  if (nvars > 3) {
    f <- function() {
      x <- eval(parse(text = varnames[1]))
      res <- 0
      for (k in 1:nmaxes) {
        foo <- xscales[k] * (x - locs[[1]][k])^2
        for (j in 2:nvars) {
          x <- eval(parse(text = varnames[j]))
          foo <- foo + (x - locs[[j]][k])^2
        }
        res <- res + signsmax[k] * exp(-foo/9)
      }
      return(res)
    }
  }
  tmp <- paste("alist( ", paste(varnames, "=", collapse = ",", 
                                sep = ""), ")")
  tmp <- eval(parse(text = tmp))
  formals(f) <- tmp
  return(f)
}