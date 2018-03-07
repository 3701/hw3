context("fquiz21 in my package")

test_that("if fquiz21 working",{
  a<-quiz21a
  x<-quiz21x
  
  testing<-t(x)%*%solve(a)%*%x
  expect_identical(fquiz21(quiz21a,quiz21x),testing)
})


