context("fquiz22 in my package")

test_that("if fquiz22 working",{
  a<-quiz21a
  x<-quiz21x
  
  testing<-t(x)%*%solve(a)%*%x
  expect_identical(a%fquiz22%x,testing)
})
