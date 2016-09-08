context("linreg")

data("iris")

Polygon <- setRefClass("Polygon", fields = c("sides"))
square <- Polygon$new(sides = 4)
test_that("lenreg rejects errounous input", {
  expect_error(linreg_mod <- linreg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris))
  expect_error(linreg_mod <- linreg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis))
})


test_that("class is correct", {
  linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_true(class(linreg_mod)[1] == "linreg")
})

test_that("print() method works", {
  linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

  expect_output(linreg_mod$print(),"linreg(formula = Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)")  
  expect_output(linreg_mod$print()," (Intercept)   Sepal.Width  Sepal.Length ")  
})

test_that("pred() method works", {
  linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

  expect_equal(round(unname(linreg_mod$pred()[c(1,5,7)]),2), c(1.85, 1.53, 1.09))    
})

test_that("resid() method works", {
  linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_equal(round(unname(linreg_mod$resid()[c(7,13,27)]),2), c(0.31, -0.58, -0.20))
})

test_that("coef() method works", {
  linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

  expect_true(all(round(unname(linreg_mod$coef()),2) %in% c(-2.52, -1.34, 1.78)))
})


test_that("summary() method works", {
  linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_output(linreg_mod$summary(), "\\(Intercept\\)( )*-2.5[0-9]*( )*0.5[0-9]*( )*-4.4[0-9]*( )*.*( )*\\*\\*\\*")  
  expect_output(linreg_mod$summary(), "Sepal.Width( )*-1.3[0-9]*( )*0.1[0-9]*( )*-10.9[0-9]*( )*.*( )*\\*\\*\\*")
  expect_output(linreg_mod$summary(), "Sepal.Length( )*1.7[0-9]*( )*0.0[0-9]*( )*27.5[0-9]*( )*.*( )*\\*\\*\\*")
  expect_output(linreg_mod$summary(), "Residual standard error: 0.6[0-9]* on 147 degrees of freedom")
})

