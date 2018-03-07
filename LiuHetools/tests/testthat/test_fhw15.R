context("fhw15 in my package")

test_that("if fhw15 working",{
  testingmu<-sum(hw15$x[1:30]*hw15$p[1:30])
  testingvar <-0
  for(i in 1:30)
  {
    testingvar<-testingvar + hw15$p[i]*(hw15$x[i]-testingmu)^2
  }
  output<-fhw15(hw15$x,hw15$p)
  expect_identical(output,c(testingmu,testingvar))
})

