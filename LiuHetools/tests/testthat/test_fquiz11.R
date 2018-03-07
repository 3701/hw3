context("fquiz11 in my package")

test_that("if fquiz11 working",{
  x<-quiz11
  testingmean<-(1/length(x))*sum(x[1:length(x)])
  a=0
  for (k in 1:length(x)){
    
    a = a + (x[k]-testingmean)^2}
  
  testingvar<- (1/length(x))*a
  testingsd<- sqrt(testingvar)

  fquiz11(x)
  expect_identical(fquiz11(x),list(mean = testingmean,var =testingvar,sd = testingsd))
})


