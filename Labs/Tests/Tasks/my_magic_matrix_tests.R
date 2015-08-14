### Assignment : my_magic_matrix() ###

context("my_magic_matrix()")

test_that("Assignment: my_magic_matrix()", {
  
  # Expect object
  expect_true(exists("my_magic_matrix"), info = "No object 'my_magic_matrix' exists.")
  
  # Expect class
  expect_is(my_magic_matrix, class = "function",
            info = "Object 'my_magic_matrix' is not a function.")

  # Expect arguments
#  expect_function_arguments(my_magic_matrix, expected = c("x", "leq"),
#                            info = "The function arguments are not named correctly.")

  # Expect self contained
  expect_self_contained(my_magic_matrix, 
                        info = "'my_magic_matrix' contains variables not defined in the function")

  # Test cases (arguments)

  # Expect to run
  expect_that(my_magic_matrix(), condition = not(throws_error()), 
              info = "'my_magic_matrix()' throws an error.")
  expect_that(my_magic_matrix(x = x2, leq = leq2), condition = not(throws_error()), 
              info = "'my_magic_matrix()' throws an error.")
  
  # Run functions
  test_res1 <- my_magic_matrix()
    
  ## Expect results
  # Expect class
  expect_is(test_res1, "matrix", 
            info = "'my_magic_matrix()' do not return a correct object.")
  
  # Expect dimensions
  expect_true(all(dim(test_res1) == c(3,3)), 
              info = "'my_magic_matrix()' do not return an object with correct dimensions.")

  # Expect names
  
  # Expect results
  expect_true(all(as.vector(test_res1) == c(4,3,8,9,5,1,2,7,6)), 
              info = "'my_magic_matrix()' returns erroneous values.")

})
