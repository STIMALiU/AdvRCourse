### Assignment : add_median_variable() ###

context("add_median_variable()")

test_that("Assignment: add_median_variable()", {
  
  # Expect object
  expect_true(exists("add_median_variable"), info = "No object 'add_median_variable' exists.")
  
  # Expect class
  expect_is(add_median_variable, class = "function",
            info = "Object 'add_median_variable' is not a function.")

  # Expect arguments
  expect_function_arguments(add_median_variable, expected = c("df", "j"),
                            info = "The function arguments are not named correctly.")

  # Expect self contained
  expect_function_self_contained(add_median_variable, 
                                 info = "'add_median_variable' contains variables not defined in the function")

  # Test cases (arguments)
  data("ChickWeight")
  df1 <- ChickWeight
  j1 <- 1

  # Expect to run
  expect_silent(add_median_variable(df = df1, j = j1))

  # Run functions
  test_res1 <- add_median_variable(df = df1, j = j1)

  ## Expect results
  # Expect class
  expect_is(test_res1, "data.frame", 
            info = "'add_median_variable()' do not return a correct object.")

  # Expect dimensions
  expect_true(all(dim(test_res1) == c(578, 5)), 
              info = "'add_median_variable()' do not return an object with correct dimensions.")

  # Expect names
  expect_true(all(names(test_res1) %in% c("weight","Time","Chick","Diet", "compared_to_median")), 
              info = "'add_median_variable()' do not return an object with correct variable names.")
  
  # Expect results
  expect_true(all(table(test_res1$compared_to_median) == c(286, 7 ,285)), 
              info = "'add_median_variable()' returns erroneous values.")
  expect_true(all(unique(test_res1$compared_to_median) == c("Smaller","Greater","Median")), 
              info = "'add_median_variable()' returns erroneous values.")
})
