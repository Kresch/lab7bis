library(stringr)
data("iris")
my_reg<-linreg(Sepal.Length ~ Sepal.Width, iris)
R_reg<-lm(iris$Sepal.Length ~ iris$Sepal.Width)
test_that("Output of linreg() mimics lm()", {
        expect_true(abs(coefficients(my_reg)[1]-R_reg$coefficients[1])< 0.01)
        expect_true(abs(coefficients(my_reg)[2]-R_reg$coefficients[2])< 0.01)
        expect_true(abs(resid(my_reg)[1]-R_reg$residuals[1])<0.01)
        expect_true(abs(fitted.values(my_reg)[1]-R_reg$fitted.values[1])< 0.01)
})


my_reg_mult<-linreg(Sepal.Length ~ Sepal.Width+Petal.Length, iris)
R_reg_mult<-lm(iris$Sepal.Length ~ iris$Sepal.Width+iris$Petal.Length)

test_that("Output of linreg() mimics lm() for multiple reg", {
        expect_true(abs(coefficients(my_reg_mult)[2]-R_reg_mult$coefficients[2])< 0.01)
        expect_true(abs(resid(my_reg_mult)[2]-R_reg_mult$residuals[2])< 0.01)
        expect_true(abs(fitted.values(my_reg_mult)[2]-R_reg_mult$fitted.values[2])< 0.01)
})

