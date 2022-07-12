context('Symbolic vs numerical differentiation')


f <- makeFun(3*x^2 ~ x)
ff <- makeFun(2*x ~ x)
# fun1 <- makeFun(f(ff(x)) + f(ff(x)) ~ x)
fun1 <- makeFun(x^2 ~ x)
fun2 <- makeFun(dnorm(f(ff(x^2)*f(sin(x)))) + pnorm(f(ff(x))) ~ x)
xpts <- rnorm(10)
df1 <- mosaicCalc::D(fun1(y) ~ y)
df2 <- mosaicCalc::D(b*fun2(x)*fun1(x) ~ x)
ndf1 <- numD(fun1(y) ~ y)
ndf2 <- numD(b*fun2(x)*fun1(x) ~ x)


# test_that("Symbolic diff. give pretty much the same results as numerical.", {
#   expect_true(
#     sum((df1(xpts) - ndf1(xpts))^2) < .01
#   )
#   expect_true(
#     sum((df2(xpts, b=3) - ndf2(xpts, b=3))^2) < .01
#   )
# })

test_that("Symbolic diff. uses only symbols, not the value bound to those symbols.", {
  x <-3
  expect_true(body(D(x^2 ~ x)) != 0)
})

