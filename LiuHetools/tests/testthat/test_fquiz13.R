context("fhw13 in my package")

test_that("if fhw13 working",{
  x<- quiz13
  fquiz13(x)
  alpha = pi
  logl <- function(x)
    sum(dgamma(length(x),shape=alpha ,  log = TRUE))
  interval <- mean(x) + c(-1, 1) * 3 * sd(x)
  interval <- pmax(mean(x) / 1e3, interval)
  oout <- optimize(logl, maximum = TRUE, interval)
  
  expect_identical(fquiz13(quiz13),  oout$maximum)
})









