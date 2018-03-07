
context("optfun in my package")

test_that("optfun test", {
  ga = function(theta, x) dgamma(x, shape = theta, log = TRUE)
  cau = function(theta, x) dcauchy(x, location = theta, log = TRUE)
  bin = function(theta, x) dbinom(x, 20, prob = 1 / (1 + exp(- theta)), log = TRUE)
  testing<- data.frame(testx=c(1,2,3),testy=c(4,5,6))
  
  
  intervalb <- c(-100,0)
  logl2 <- function(theta,x) sum(dbinom(testing$testx, 20, prob = 1 / (1 + exp(- theta)), log = TRUE))
  bob<- optimize(logl2, maximum = TRUE, intervalb,x=x)
  
  intervalc <- c(-10,10)
  logl3 <- function(theta,x)  sum (dcauchy(testing$testx, location = theta, log = TRUE))
  cat<- optimize(logl3, maximum = TRUE, intervalc,x=x)
  
  intervala <- c(0,3)
  logl1 <- function(theta,x) sum(dgamma(testing$testx, shape = theta, log = TRUE))
  good<- optimize(logl1, maximum = TRUE, intervala,x=x)
  
  help(round)
  test1 =optfun(testing$testx,ga,c(0,3))
  test2 = optfun(testing$testx,cau,c(-10,10))
  test3 = optfun(testing$testx,bin,c(-100,0))
  
  expect_identical(test1,optimize(logl1, maximum = TRUE, intervala,x=x)$maximum)
  expect_identical(test2,optimize(logl3, maximum = TRUE, intervalc,x=x)$maximum)
  expect_identical(test3,optimize(logl2, maximum = TRUE, intervalb,x=x)$maximum)
  
})



