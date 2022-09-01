test_that("can pipe into contour plots", {
  P1 <- contour_plot(2*x - y ~ x & y, bounds(x=-1:1, y=-1:1)) %>%
    contour_plot(x +2 * y ~ x & y, contour_color="red")
  P2 <- contour_plot(sin(x)*exp(-y) ~ x & y, bounds(x=0:10,y=-1:1) ) %>%
    slice_plot(sin(x) ~ x)
  
  expect_true(inherits(P1, "gg"))
  expect_true(inherits(P2, "gg"))
  
})

test_that("order of layers doesn't matter", {
  dt_rabbit <- function(r, f, alpha=2/3, beta=4/3) {
    alpha*r - beta*r*f
  }
  dt_fox <- function(r, f, delta=1, gamma=1) {
    -delta*f + gamma*r*f
  }
  P1 <- contour_plot(dt_rabbit(r,f) ~ r & f, bounds(r=0.2:2, f=0.05:1), 
               contours_at = 0, contour_color = "red",
               labels=FALSE) %>%
    contour_plot(dt_fox(r,f) ~ r & f,
                 contours_at = 0, contour_color = "blue",
                 labels=FALSE) %>%
    vectorfield_plot(r ~ dt_rabbit(r,f),
                     f ~ dt_fox(r,f),
                     transform=function(x) x^0.3, env=environment(dt_rabbit))
  expect_true(inherits(P1, "gg"))
  P2 <- contour_plot(dt_rabbit(r,f) ~ r & f, bounds(r=0.2:2, f=0.05:1), 
                     contours_at = 0, contour_color = "red",
                     labels=FALSE) %>%
    vectorfield_plot(r ~ dt_rabbit(r,f),
                     f ~ dt_fox(r,f),
                     transform=function(x) x^0.3, env=environment(dt_rabbit)) %>%
    contour_plot(dt_fox(r,f) ~ r & f,
                 contours_at = 0, contour_color = "blue",
                 labels=FALSE) 
  expect_true(inherits(P2, "gg"))
  P3 <- vectorfield_plot(r ~ dt_rabbit(r,f), bounds(r=0.2:2, f=0.05:1),
                     f ~ dt_fox(r,f),
                     transform=function(x) x^0.3, env=environment(dt_rabbit)) %>%
    contour_plot(dt_rabbit(r,f) ~ r & f,  
                     contours_at = 0, contour_color = "red",
                     labels=FALSE) %>%
    contour_plot(dt_fox(r,f) ~ r & f,
                 contours_at = 0, contour_color = "blue",
                 labels=FALSE) 
  expect_true(inherits(P3, "gg"))
})
