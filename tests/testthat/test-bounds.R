test_that("bounds() and domain() handle their arguments specially", {
  B <- bounds(x=c(1.5:10.5))
  expect_equal(names(B), "x")
  expect_equal(B$x, c(1.5, 10.5))
})
