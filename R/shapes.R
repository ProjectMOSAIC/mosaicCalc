#' Shapes in the book
#'
#' Used as examples for moment of inertial calculations
#' 
shapes <- list()
seeds <- c(939,950, 955, 962)
xoffsets <- c(0, 5, -2, 10)
yoffsets <- c(0, 2.4, 6.8, -3.2)
radii <- c(1, 3, 41, 16)
for (k in 1:length(seeds)) {
  set.seed(seeds[k])
  n <- 10
  Shape <- tibble(
    theta = seq(0, 2*pi, length=n),
    r = radii[k]*(1 + runif(n)),
    x = xoffsets[k] + r*cos(theta),
    y = yoffsets[k] + r*sin(theta)
  )
  
  suppressWarnings({
    xfun <- mosaic::spliner(x ~ theta, data = Shape, method="periodic")
    yfun <- mosaic::spliner(y ~ theta, data = Shape, method="periodic")
  })
  
  shapes[[k]] <- tibble(
    theta = seq(0, 2*pi, length=20*n),
    x = xfun(theta),
    y = yfun(theta)
  ) %>% select(-theta)
}

#' @export
Blob1 <- shapes[[1]]
#' @export
Blob2 <- shapes[[2]]
#' @export
Blob3 <- shapes[[3]]
#' @export
Blob4 <- shapes[[4]]