context("fhw25 in my package")

test_that("if fhw25 working",{
fred<-hw25
expect_identical(fhw25(fred,1,"mean"),apply(fred,1,"mean"))
expect_identical(fhw25(fred,2,"mean"),apply(fred, 2, "mean"))
expect_identical(fhw25(fred, 2, max),apply(fred,2,max))

})


