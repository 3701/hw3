context("fquiz23 in my package")

test_that("if fquiz23 working",{
  x<-quiz23
 testing<- (x[,1:4]-apply(x,2,mean))/apply(x,2,sd)
  
  expect_identical(fquiz23(x),testing)
})




