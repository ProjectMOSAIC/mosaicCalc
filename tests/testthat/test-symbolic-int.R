context('Symbolic Integration')

test_that('Simple polynomial expressions work',{
  ff = antiD(2*x^6+9*x^3+2 ~ x)
  gg = function(x)2*1/7*x^7+9*1/4*x^4+2*x
  expect_that(ff(seq(1,10, len=30)), equals(gg(seq(1,10, len=30)), tol=0.00001))

  ff = antiD((2+3)*x^5+(2+9)*x^2+(8*6+2)*x ~ x)
  gg = function(x){(2+3)*1/(6)*x^6+(2+9)*1/(3)*x^3+(8*6+2)*1/(2)*x^2}
  expect_that(ff(seq(1,10, len=28)), equals(gg(seq(1,10, len=28)), tol=0.00001))
})

test_that('Declared variables do not interfere with constants',{
  a <- 5
  ff <- antiD((a+3)*x^7-a*4*x+a ~ x)
  gg <- function(x,a){(a+3)*1/(8)*x^8-a*4*1/(2)*x^2+a*x}
  expect_that(ff(seq(1,10, len=26), a=2), equals(gg(seq(1,10, len=26), a=2), tol=0.00001))
})

test_that('Negative exponents on polynomials work',{
  ff <- antiD(3*x^-4+x^-2-x^-3 ~ x)
  gg <- function(x){3*1/(-3)*x^-3+1/(-1)*x^-1-1/(-2)*x^-2}
  expect_that(ff(seq(1,10, len=28)), equals(gg(seq(1,10, len=28)), tol=0.00001))

  ff <- antiD(3/x ~ x)
  gg <- function(x){3*log(x)}
  expect_that(ff(seq(1,10, len=28)), equals(gg(seq(1,10, len=28)), tol=0.00001))
})

test_that('Simple trigonometric functions are working',{
  ff <- antiD(27*sin(3*x) ~ x)
  gg <- function(x){-9*cos(3*x)}
  expect_that(ff(seq(1,10, len=28)), equals(gg(seq(1,10, len=28)), tol=0.00001))

  ff <- antiD(-sin(x) ~ x)
  gg <- function(x){cos(x)}
  expect_that(ff(seq(1,10, len=28)), equals(gg(seq(1,10, len=28)), tol=0.00001))

})

test_that('Numerical anti-derivative returned when appropriate', {
 F <- antiD(sin(cos(x)) ~ x)
 expect_true("evalFun" %in% all.names(body(F)))
 F <- antiD(sin(x^3) ~ x)
 expect_true("evalFun" %in% all.names(body(F)))
 F <- antiD(x^(x) ~ x)
 expect_true("evalFun" %in% all.names(body(F)))
# This is only true on OS-X
# F <- antiD(1/x^2+1/x^3 ~ x) # I don't know why Ryacas doesn't handle this
# expect_true("evalFun" %in% all.names(body(F)))
})

test_that('Everything works',{
  checkFun <- function(formula, integral, ...){
    ff <- antiD(formula, ...)
    gg <- makeFun(integral, ...)
    vars <- list()
    for(i in (1:length(formals(ff)))){
      if(class(formals(ff)[[i]])=="name")
        vars[[names(formals(ff))[[i]]]] <- seq(-10,10, len = 40)
    }
    expect_that(do.call(ff, vars), equals(do.call(gg, vars), tol=0.00001))
  }

  checkFun((r^2 - x^2) ~ x, r^2*x - x^3/3  ~ x, r = 1 ) # Issue 180
  checkFun(x^12+x^9-x^6+x^3-1 ~ x, 1/13*x^13+1/10*x^10-1/7*x^7+1/4*x^4-x ~ x)
  checkFun((2+3)*x^5+(2+9)*x^2+(8*6+2)*x ~ x, (2+3)*1/(6)*x^6+(2+9)*1/(3)*x^3+(8*6+2)*1/(2)*x^2 ~ x)
  checkFun((a+3)*y^7-a*4*y+a ~ y, (a+3)*1/(8)*y^8-a*4*1/(2)*y^2+a*y ~ y, a = 3)
  checkFun(x^n ~ x, 1/(n+1)*x^(n+1) ~ x, n=4)
  checkFun(3*x^-4+x^-2-x^-3 ~ x,3*1/(-3)*x^-3+1/(-1)*x^-1-1/(-2)*x^-2 ~ x )
  suppressWarnings(checkFun(3/y+3*y^2 ~ y, 3*log(y)+y^3 ~ y)) # produces NaNs
  checkFun(((2+((3*((x)))))) ~ x, 2*x+3/2*x^2 ~ x)
  checkFun(3*(2*x)^2 ~ x, 1/2*(2*x)^3 ~ x)
  suppressWarnings(checkFun(1/(x+1) ~ x, log(1+x) ~ x))   # produces NaNs
  suppressWarnings(checkFun((x+1)^-1 ~ x, log(1+x) ~ x))  # produces NaNs
  checkFun(3*sin(3*x+1) ~ x, -cos(3*x+1) ~ x)
  checkFun(7*(1+x) ~ x, 7*x + (7/2)*x^2 ~ x) # DTK See issue 13
  checkFun(sin(2*(x)) ~ x, -1/2*cos(2*x) ~ x)
  checkFun(2*pi*(x/P) ~ x, pi/P*x^2 ~ x, P = 4)
  checkFun(3^2 ~ y, 9*y ~ y)
  checkFun((x*8)^2 ~ x, 1/24*(8*x)^3 ~ x)
  checkFun(exp(3*x+9) ~ x, 1/3*exp(3*x+9) ~ x)
  suppressWarnings(checkFun(x^(1/2) ~ x, 2/3*x^(3/2) ~ x))
  suppressWarnings(checkFun(x^(-31/52) ~ x, 52/21*x^(21/52) ~ x))
  checkFun(2*z+2*z+2*z+2*z+2*z+2*z+2*z+2*z+2*z+2*z+2*z+2*z ~ z,
           z^2+z^2+z^2+z^2+z^2+z^2+z^2+z^2+z^2+z^2+z^2+z^2 ~ z)
})


test_that('trig subs work', {
  tilde <- 1/(x^2 + 1) ~ x
  f <- antiD(tilde)
  expect_true( "atan" %in% all.names(body(f)))
})
