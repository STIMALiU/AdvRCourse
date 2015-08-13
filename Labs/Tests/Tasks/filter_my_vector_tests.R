### Assignment : filter_my_vector() ###

context("filter_my_vector()")

test_that("Assignment: filter_my_vector()", {
  
  # Expect object
  expect_true(exists("filter_my_vector"), info = "No object 'filter_my_vector' exists.")
  
  # Expect class
  expect_is(filter_my_vector, class = "function",
            info = "Object 'filter_my_vector' is not a function.")

  # Expect arguments
  expect_function_arguments(filter_my_vector, expected = c("x", "leq"),
                            info = "The function arguments are not named correctly.")

  # Expect self contained
  expect_self_contained(filter_my_vector, 
                        info = "'filter_my_vector' contains variables not defined in the function")

  # Test cases (arguments)
  x1 <- c(2, 9, 2, 4, 102)
  leq1 <- 4
  x2 <- as.numeric(7:15)
  leq2 <- 8

  # Expect to run
  expect_that(filter_my_vector(x = x1, leq = leq1), condition = not(throws_error()), 
              info = "'filter_my_vector()' throws an error.")
  expect_that(filter_my_vector(x = x2, leq = leq2), condition = not(throws_error()), 
              info = "'filter_my_vector()' throws an error.")
  
  # Run functions
  test_res1 <- filter_my_vector(x = x1, leq = leq1)
  test_res2 <- filter_my_vector(x = x2, leq = leq2)
    
  ## Expect results
  # Expect class
  expect_is(test_res1, "numeric", 
            info = "'filter_my_vector()' do not return a numeric vector.")
  expect_is(test_res2, "numeric", 
            info = "'filter_my_vector()' do not return a numeric vector.")
  
  # Expect dimensions
  expect_true(length(test_res1) == 5, 
              info = "'filter_my_vector()' do not return an object with correct dimensions.")
  expect_true(length(test_res2) == 9, 
              info = "'filter_my_vector()' do not return an object with correct dimensions.")

  # Expect names
  
  # Expect results
  expect_true(all(is.na(test_res1[c(2,4)])), 
              info = "'filter_my_vector()' returns erroneous values.")
  expect_true(all(test_res1[c(1,3)] == c(2,2)), 
              info = "'filter_my_vector()' returns erroneous values.")
  expect_true(all(is.na(test_res2[2])), 
              info = "'filter_my_vector()' returns erroneous values.")
  expect_true(all(test_res2[1] == 7), 
              info = "'filter_my_vector()' returns erroneous values.")

})
