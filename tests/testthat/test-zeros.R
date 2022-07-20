test_that("Zeros() can operate without a domain.", {
  res <- Zeros(a*x + b ~ x, a=1, b=2)
  expect_equal(nrow(res), 1)
  expect_equal(res$x, -2)
})

test_that("Zeros() finds multiple zeros, when they exist.", {
  res <- Zeros(sin(omega*x) ~ x, omega=1, bounds(x=0:100))
  expect_equal(nrow(res), 32)
  expect_equal(mean(diff(res$x)), pi, tol=0.00001)
})

test_that("Zeros() looks at the edges of the bounds for a zero.", {
  res <- Zeros(sin(x) ~ x, bounds(x=0:3.1415926535898))
  expect_equal(nrow(res), 2)
})
