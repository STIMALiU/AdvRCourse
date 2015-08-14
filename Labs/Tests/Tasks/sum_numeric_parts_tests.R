### Assignment : sum_numeric_parts() ###

context("sum_numeric_parts()")

test_that("Assignment: sum_numeric_parts()", {
  
  # Expect object
  expect_true(exists("sum_numeric_parts"), info = "No object 'sum_numeric_parts' exists.")
  
  # Expect class
  expect_is(sum_numeric_parts, class = "function",
            info = "Object 'sum_numeric_parts' is not a function.")

  # Expect arguments
  expect_function_arguments(sum_numeric_parts, expected = c("x"),
                            info = "The function arguments are not named correctly.")

  # Expect self contained
  expect_self_contained(sum_numeric_parts, 
                        info = "'sum_numeric_parts' contains variables not defined in the function")

  # Test cases (arguments)
  x1 <- list(1:10, 3:5, matrix(1:4, ncol=2))


  # Expect to run
  expect_that(sum_numeric_parts(x = x1), 
              condition = not(throws_error()), 
              info = "'sum_numeric_parts()' throws an error.")

  # Run functions
  test_res1 <- sum_numeric_parts(x = x1)

  ## Expect results
  # Expect class
  expect_is(test_res1, "numeric", 
            info = "'sum_numeric_parts()' do not return a correct object.")

  # Expect dimensions
  expect_true(length(test_res1) == 1, 
              info = "'sum_numeric_parts()' do not return an object with correct dimensions.")

  # Expect names

  # Expect results
  expect_true(test_res1 == 77, 
              info = "'sum_numeric_parts()' returns erroneous values.")
})
