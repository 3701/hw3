context("fquiz12 in my package")

test_that("if fquiz12 working",{
  x<-quiz12
  testingmean<-(1/length(x))*sum(x[1:length(x)])
  a=0
  for (k in 1:length(x)){
    
    a = a + (x[k]-testingmean)^2}
  
  testingvar<- (1/length(x))*a
  testingsd<- sqrt(testingvar)
  
  fquiz12(x)
  expect_identical(fquiz12(x),list(mean = testingmean,var =testingvar,sd = testingsd))
})


