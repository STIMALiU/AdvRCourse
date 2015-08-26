### Assignment : calculate_elements() ###

context("calculate_elements()")

test_that("Assignment: calculate_elements()", {
  
  # Expect object
  expect_true(exists("calculate_elements"), info = "No object 'calculate_elements' exists.")
  
  # Expect class
  expect_is(calculate_elements, class = "function",
            info = "Object 'calculate_elements' is not a function.")

  # Expect arguments
  expect_function_arguments(calculate_elements, expected = c("A"),
                            info = "The function arguments are not named correctly.")

  # Expect self contained
  expect_self_contained(calculate_elements, 
                        info = "'calculate_elements' contains variables not defined in the function")

  # Test cases (arguments)
  x1 <- matrix(1:6, ncol=2)
  x2 <- matrix(1:2, ncol=2)

  # Expect to run
  expect_that(calculate_elements(A = x1), condition = not(throws_error()), 
              info = "'calculate_elements()' throws an error.")
  expect_that(calculate_elements(A = x2), condition = not(throws_error()), 
              info = "'calculate_elements()' throws an error.")
  
  # Run functions
  test_res1 <- calculate_elements(A = x1)
  test_res2 <- calculate_elements(A = x2)
    
  ## Expect results
  # Expect class
  expect_true(is.numeric(test_res1) | is.integer(test_res1), 
            info = "'calculate_elements()' do not return a correct object.")
  expect_true(is.numeric(test_res2) | is.integer(test_res2), 
            info = "'calculate_elements()' do not return a correct object.")
  
  # Expect dimensions
  expect_true(length(test_res1) == 1, 
              info = "'calculate_elements()' do not return an object with correct dimensions.")
  expect_true(length(test_res2) == 1, 
              info = "'calculate_elements()' do not return an object with correct dimensions.")

  # Expect names
  
  # Expect results
  expect_true(test_res1 == 6, 
              info = "'calculate_elements()' returns erroneous values.")
  expect_true(test_res2 == 2, 
              info = "'calculate_elements()' returns erroneous values.")
})
