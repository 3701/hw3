
           context("hselect in my package")
           
           test_that("hselect here produce same as select in dplyr", {
           testing<- data.frame(testx=c(1,2,3),testy=c(4,5,6))
           expect_identical(hselect(testing,testx), dplyr::select(testing,testx))
           })
           
           
           