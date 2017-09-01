### Assignment : row_to_zero() ###

context("row_to_zero()")

test_that("Assignment: row_to_zero()", {
  
  # Expect object
  expect_true(exists("row_to_zero"), info = "No object 'row_to_zero' exists.")
  
  # Expect class
  expect_is(row_to_zero, class = "function",
            info = "Object 'row_to_zero' is not a function.")

  # Expect arguments
  expect_function_arguments(row_to_zero, expected = c("A", "i"),
                            info = "The function arguments are not named correctly.")

  # Expect self contained
  expect_function_self_contained(row_to_zero, 
                                 info = "'row_to_zero' contains variables not defined in the function")

  # Test cases (arguments)
  x1 <- matrix(1:6, ncol=2) 
  i1 <- 3
  x2 <- matrix(1:9, ncol=1)
  i2 <- 1

  # Expect to run
  expect_silent(row_to_zero(A = x1, i = i1))
  expect_silent(row_to_zero(A = x2, i = i2))
  
  # Run functions
  test_res1 <- row_to_zero(A = x1, i = i1)
  test_res2 <- row_to_zero(A = x2, i = i2)
    
  ## Expect results
  # Expect class
  expect_is(test_res1, "matrix", 
            info = "'row_to_zero()' do not return a correct object.")
  expect_is(test_res2, "matrix", 
            info = "'row_to_zero()' do not return a correct object.")
  
  # Expect dimensions
  expect_true(all(dim(test_res1) == c(3,2)), 
              info = "'row_to_zero()' do not return an object with correct dimensions.")
  expect_true(all(dim(test_res2) == c(9,1)), 
              info = "'row_to_zero()' do not return an object with correct dimensions.")

  # Expect names
  
  # Expect results
  expect_true(sum(test_res1) == 12, 
              info = "'row_to_zero()' returns erroneous values.")
  expect_true(sum(test_res2) == 44, 
              info = "'row_to_zero()' returns erroneous values.")
})
