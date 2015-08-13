### Assignment : my_num_vector() ###

context("my_num_vector()")

test_that("Assignment: my_num_vector()", {
  
  # Expect object
  expect_true(exists("my_num_vector"), info = "No object 'my_num_vector' exists.")
  
  # Expect class
  expect_is(my_num_vector, class = "function",
            info = "Object 'my_num_vector' is not a function.")

  # Expect arguments
  # expect_function_arguments(my_num_vector, 
  #                           info = "The function arguments are not named correctly.")

  # Expect self contained
  #expect_self_contained(my_num_vector, 
  #                      info = "'my_num_vector' contains variables not defined in the function")

  # Test cases (arguments)
  

  # Expect to run
  expect_that(my_num_vector(), condition = not(throws_error()), 
              info = "'my_num_vector()' throws an error.")
  
  # Run functions
  test_res1 <- my_num_vector()
    
  ## Expect results
  # Expect class
  expect_is(test_res1, "numeric", 
            info = "'my_num_vector()' do not return a numeric vector.")
  
  # Expect dimensions
  expect_true(length(test_res1) == 4, 
              info = "'my_num_vector()' do not return a vector of length 4.")

  # Expect names
  
  # Expect results
  expect_true(all(round(test_res1, 7) == c(1.0413927, 0.8090170, 2.8496539, 0.2105263)), 
              info = "'my_num_vector()' returns erroneous values.")

})
