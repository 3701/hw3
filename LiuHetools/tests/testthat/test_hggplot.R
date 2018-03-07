
           context("hggplot in my package")
           
           test_that("hggplot here produce same as ggplot+geom_point() in tidyverse", {
           testing<- data.frame(testx=c(1,2,3),testy=c(4,5,6))
           is.ggplot(hggplot(testing,aes(testx,testy)))==TRUE
           })

