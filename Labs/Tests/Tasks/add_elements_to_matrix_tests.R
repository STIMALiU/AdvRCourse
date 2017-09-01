### Assignment : add_elements_to_matrix() ###

context("add_elements_to_matrix()")

test_that("Assignment: add_elements_to_matrix()", {
  
  # Expect object
  expect_true(exists("add_elements_to_matrix"), info = "No object 'add_elements_to_matrix' exists.")
  
  # Expect class
  expect_is(add_elements_to_matrix, class = "function",
            info = "Object 'add_elements_to_matrix' is not a function.")

  # Expect arguments
  expect_function_arguments(add_elements_to_matrix, expected = c("A", "x", "i", "j"),
                            info = "The function arguments are not named correctly.")

  # Expect self contained
  expect_function_self_contained(add_elements_to_matrix, 
                                 info = "'add_elements_to_matrix' contains variables not defined in the function")

  # Test cases (arguments)
  A1 <- matrix(1:9, ncol=3) 
  i1 <- 1:3
  j1 <- 2
  x1 <- 4
  A2 <- matrix(1:12, ncol=3) 
  i2 <- 2:3
  j2 <- 1:2
  x2 <- -3

  # Expect to run
  expect_silent(add_elements_to_matrix(A = A1, i = i1, j = j1, x = x1))
  expect_silent(add_elements_to_matrix(A = A2, i = i2, j = j2, x = x2))
  
  # Run functions
  test_res1 <- add_elements_to_matrix(A = A1, i = i1, j = j1, x = x1)
  test_res2 <- add_elements_to_matrix(A = A2, i = i2, j = j2, x = x2)
    
  ## Expect results
  # Expect class
  expect_is(test_res1, "matrix", 
            info = "'add_elements_to_matrix()' do not return a correct object.")
  expect_is(test_res2, "matrix", 
            info = "'add_elements_to_matrix()' do not return a correct object.")
  
  # Expect dimensions
  expect_true(all(dim(test_res1) == c(3,3)), 
              info = "'add_elements_to_matrix()' do not return an object with correct dimensions.")
  expect_true(all(dim(test_res2) == c(4,3)), 
              info = "'add_elements_to_matrix()' do not return an object with correct dimensions.")

  # Expect names
  
  # Expect results
  expect_true(sum(test_res1) == 57, 
              info = "'add_elements_to_matrix()' returns erroneous values.")
  expect_true(sum(test_res2) == 66, 
              info = "'add_elements_to_matrix()' returns erroneous values.")
})
