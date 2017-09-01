### Assignment : my_data.frame() ###

context("my_data.frame()")

test_that("Assignment: my_data.frame()", {
  
  # Expect object
  expect_true(exists("my_data.frame"), info = "No object 'my_data.frame' exists.")
  
  # Expect class
  expect_is(my_data.frame, class = "function",
            info = "Object 'my_data.frame' is not a function.")

  # Expect arguments

  # Expect self contained
  expect_function_self_contained(my_data.frame, 
                                 info = "'my_data.frame' contains variables not defined in the function")

  # Test cases (arguments)

  # Expect to run
  expect_silent(my_data.frame())

  # Run functions
  test_res1 <- my_data.frame()

  ## Expect results
  # Expect class
  expect_is(test_res1, "data.frame", 
            info = "'my_data.frame()' do not return a correct object.")

  # Expect dimensions
  expect_true(all(dim(test_res1) == c(3,4)), 
              info = "'my_data.frame()' do not return an object with correct dimensions.")

  # Expect names
  expect_true(all(names(test_res1) %in% c("id","name","income","rich")), 
              info = "'my_data.frame()' do not return an object with correct variable names.")
  
  # Expect results
  expect_true(sum(test_res1$id) == 6, 
              info = "'my_data.frame()' returns erroneous values (id).")
  expect_true(sum(test_res1$income) == 22.51, 
              info = "'my_data.frame()' returns erroneous values (income).")
  expect_true(sum(test_res1$rich) == 1, 
              info = "'my_data.frame()' returns erroneous values (rich).")
})
