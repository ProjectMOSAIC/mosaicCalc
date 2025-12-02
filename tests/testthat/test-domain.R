test_that("domain() handles its arguments specially", {
  B <- domain(x=c(1.5:10.5))
  expect_equal(names(B), "x")
  expect_equal(B$x, c(1.5, 10.5))
})
