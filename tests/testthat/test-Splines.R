test_that("Differentiation of splines work", {
  # Cubic splines are a special case for differentiation, since
  # the function created by R also can take it's own derivative.
  Data <- tibble(
    x = 1:10,
    y = rnorm(length(x))
  )
  fspline <- mosaic::spliner(y ~ x, data = Data)
  dx_fspline <- D(fspline(z) ~ z)
  expect_equal(formals(dx_fspline)$deriv, 1)
  dxx_fspline <- D(fspline(z) ~ z & z)
  expect_equal(formals(dxx_fspline)$deriv, 2)
  dxxx_fspline <- D(fspline(x) ~ x & x & x)
  expect_equal(formals(dxxx_fspline)$deriv, 3)
  dxy_fspline <- D(fspline(x) ~ x & y)
  expect_equal(dxy_fspline(3.232), 0)
  dy_fspline <- D(fspline(x) ~ y)
  expect_equal(dy_fspline(3.232), 0)
})
