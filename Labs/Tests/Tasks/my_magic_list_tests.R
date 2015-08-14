### Assignment : my_magic_list() ###

context("my_magic_list()")

test_that("Assignment: my_magic_list()", {
  
  # Expect object
  expect_true(exists("my_magic_list"), info = "No object 'my_magic_list' exists.")
  
  # Expect class
  expect_is(my_magic_list, class = "function",
            info = "Object 'my_magic_list' is not a function.")

  # Expect arguments

  # Expect self contained
  expect_self_contained(my_magic_list, 
                        info = "'my_magic_list' contains variables not defined in the function")

  # Test cases (arguments)

  # Expect to run
  expect_that(my_magic_list(), 
              condition = not(throws_error()), 
              info = "'my_magic_list()' throws an error.")

  # Run functions
  test_res1 <- my_magic_list()
    
  ## Expect results
  # Expect class
  expect_is(test_res1, "list", 
            info = "'my_magic_list()' do not return a correct object.")
  
  # Expect dimensions
  expect_true(length(test_res1) == 3, 
              info = "'my_magic_list()' do not return an object with correct dimensions.")
  expect_true(length(unlist(test_res1)) == 14, 
              info = "'my_magic_list()' do not return an object with correct dimensions of elements.")
  
  # Expect names
  expect_true(all(names(test_res1) %in% c("", "info")), 
              info = "'my_magic_list()' do not return an object with correct names")
  
  # Expect results
  listsum <- sum(suppressWarnings(as.numeric(unlist(test_res1))), na.rm = TRUE)
  expect_true(round(listsum,2) == 49.91, 
              info = "'my_magic_list()' returns erroneous values.")
  expect_true(test_res1$info == "my own list", 
              info = "'my_magic_list()' returns erroneous values (in info, remember R is case sensitive).")
})
