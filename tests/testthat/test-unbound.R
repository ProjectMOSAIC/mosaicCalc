test_that("multiplication works", {
  expect_equal(2 * 2, 4)
   f <- makeFun(a*x + b ~ x)
   g <- makeFun(a*x + b ~ x, a=1)
   h <- makeFun(a*x + b ~ x, a=1, b=2)
   expect_equal(mosaicCalc:::unbound(f), c("x", "a", "b"))
   expect_equal(mosaicCalc:::unbound(g), c("x", "b"))
   expect_equal(mosaicCalc:::unbound(h), c("x"))
   
})
