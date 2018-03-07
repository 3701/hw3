context("fhw14 in my package")

test_that("if fhw14 working",{
  testingmu<-sum(hw14$x[1:30]*hw14$p[1:30])
  testingvar <-0
  for(i in 1:30)
  {
  testingvar<-testingvar + hw14$p[i]*(hw14$x[i]-testingmu)^2
  }
  output<-fhw14(hw14$x,hw14$p)
  expect_identical(output,c(testingmu,testingvar))
})
