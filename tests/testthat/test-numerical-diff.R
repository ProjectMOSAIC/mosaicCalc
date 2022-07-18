test_that("numerical differentiation works", {
  f <- makeFun(x^2 + 2*x*y + y^3 ~ .)
  dfdx <-    numD(f(x,y) ~ x)
  dfdy <-    numD(f(x,y) ~ y)  
  d2fd2x <-  numD(f(x,y) ~ x & x)
  d2fd2y <-  numD(f(x,y) ~ y & y)
  d2fdxdy <- numD(f(x,y) ~ x & y)
  d2fdydx <- numD(f(x,y) ~ y & x)
  expect_equal(dfdx(1,1), 4) # deriv with respect to x works
  expect_equal(dfdx(2,2), 8)
  expect_equal(dfdy(1,1), 5) #  ditto
  expect_equal(dfdy(2,2), 16)
  expect_equal(d2fd2x(1,1), 2) # second-order unmixed by x
  expect_equal(d2fd2y(1,1), 6) # second-order unmixed by x
  expect_equal(d2fdxdy(0,3), d2fdydx(0,3)) # mixed partials are identical
  expect_equal(d2fdxdy(1,1), 2)   # and the values are right
  expect_equal(d2fdxdy(2,2), 2)   # and the values are right
})

