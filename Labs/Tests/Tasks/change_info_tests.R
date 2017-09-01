### Assignment : change_info() ###

context("change_info()")

test_that("Assignment: change_info()", {
  
  # Expect object
  expect_true(exists("change_info"), info = "No object 'change_info' exists.")
  
  # Expect class
  expect_is(change_info, class = "function",
            info = "Object 'change_info' is not a function.")

  # Expect arguments
  expect_function_arguments(change_info, expected = c("x", "text"),
                            info = "The function arguments are not named correctly.")

  # Expect self contained
  expect_function_self_contained(change_info, 
                                 info = "'change_info' contains variables not defined in the function")

  # Test cases (arguments)
  x1 <- list(info="testinfo1")
  text1 <- "new info"
  x2 <- list(1:10, info="testinfo2")
  text2 <- "new info"
  
  # Expect to run
  expect_silent(change_info(x = x1, text = text1))
  expect_silent(change_info(x = x2, text = text2))

  # Run functions
  test_res1 <- change_info(x = x1, text = text1)
  test_res2 <- change_info(x = x2, text = text2)
    
  ## Expect results
  # Expect class
  expect_is(test_res1, "list", 
            info = "'change_info()' do not return a correct object.")
  expect_is(test_res2, "list", 
            info = "'change_info()' do not return a correct object.")
  
  # Expect dimensions
  expect_true(length(test_res1) == 1, 
              info = "'change_info()' do not return an object with correct dimensions.")
  expect_true(length(test_res2) == 2, 
              info = "'change_info()' do not return an object with correct dimensions.")
  expect_true(length(unlist(test_res1)) == 1, 
              info = "'change_info()' do not return an object with correct dimensions of elements.")
  expect_true(length(unlist(test_res2)) == 11, 
              info = "'change_info()' do not return an object with correct dimensions of elements.")
  
  # Expect names
  expect_true(all(names(test_res1) %in% c("", "info")), 
              info = "'change_info()' do not return an object with correct names")
  expect_true(all(names(test_res2) %in% c("", "info")), 
              info = "'change_info()' do not return an object with correct names")
  
  # Expect results
  expect_true(test_res1$info == text1, 
              info = "'change_info()' returns erroneous values (in info, remember R is case sensitive).")
  expect_true(test_res2$info == text2, 
              info = "'change_info()' returns erroneous values (in info, remember R is case sensitive).")
})
