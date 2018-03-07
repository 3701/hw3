context("fquiz26 in my package")

test_that("if fquiz26 working",{
  
  x<-hw26

 testing<-list( apply(x,c(1,3),median),
  apply(x,c(1,2),median),
  apply(x,c(2,3),median),

  apply(x,c(3),median),
  apply(x,c(1),median),
  apply(x,c(2),median))
  
  
  
  expect_identical(fhw26(x),testing)
})




