test_that("Can identify unbound arguments.", {
  expect_equal(2 * 2, 4)
   f <- makeFun(a*x + b ~ x)
   g <- makeFun(a*x + b ~ x, a=1)
   h <- makeFun(a*x + b ~ x, a=1, b=2)
   expect_equal(mosaicCalc:::unbound(f), c("x", "a", "b"))
   expect_equal(mosaicCalc:::unbound(g), c("x", "b"))
   expect_equal(mosaicCalc:::unbound(h), c("x"))
})

test_that("Arguments are put in canonical order", {
  f1 <- makeFun(a*x + b*d ~ d & b & x & a) # not in canonical order
  f2 <- mosaicCalc:::conventional_argument_order(f1)
  expect_equal(names(formals(f2)), c("x", "a", "b", "d"))
  Df1 <- D(f1(x) ~ x, d=3)
  expect_equal(names(formals(Df1)), c("x", "a", "b", "d"))
  F1 <- antiD(f1(x) ~ x)
  expect_equal(names(formals(Df1)), c("x", "a", "b", "d"))
})
