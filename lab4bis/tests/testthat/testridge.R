#tests ridgereg
# library(Lab4)
library(stringr)
data("iris")
my_ridge<-ridgereg(Sepal.Length ~ Sepal.Width, iris)
my_ridge_lambda<-ridgereg(Sepal.Length ~ Sepal.Width, iris,lambda=3)
newdata=data.frame(c(1,2,3,4,5))
test_that("We can use method predict without errors is being thrown", {
        expect_that(predict(my_ridge,newdata), not(throws_error()))
})
test_that("We can use methods coef without errors is being thrown", {
        expect_that(coef(my_ridge), not(throws_error()))
})
test_that("We can use methods print without errors is being thrown", {
        expect_that(print(my_ridge), not(throws_error()))
})
test_that("test test",{
  expect_equal(6,6)
})

