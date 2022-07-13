test_that("D() can look inside simple functions", {
  expect_equal(body(D(a*x + b ~ x)), quote(a))
  f <- makeFun(a*x + b ~ .)
  expect_equal(body(D(f(x) ~ x)), quote(a))
})

test_that("antiD() can look inside simple functions", {
  expect_equal(body(antiD(a*x + b ~ x)), quote((x^2 * a + 2 * x * b)/2 + C))
  f <- makeFun(a*x + b ~ .)
  expect_equal(body(antiD(f(x) ~ x)), quote(a))
})

