context("RidgeReg Test")
library(MASS)

data("iris")

test_that("Compare coefficients", {
  item1 <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length,data = iris)
  item2 <- lm.ridge(Petal.Length ~ Sepal.Width + Sepal.Length,data = iris)
  expect_equal(round(item1$coef()[-1],1), round(item2$coef,1))
})

test_that("Error Warning", {
  expect_error(item3 <- ridgereg(formula = Petal.Length~Sepdsal.Width+Sepal, data = iris))
  expect_error(item4 <- ridgereg(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data = wrongname))
})
