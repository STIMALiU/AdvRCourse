### Assignment : add_note() ###

context("add_note()")

test_that("Assignment: add_note()", {
  
  # Expect object
  expect_true(exists("add_note"), info = "No object 'add_note' exists.")
  
  # Expect class
  expect_is(add_note, class = "function",
            info = "Object 'add_note' is not a function.")

  # Expect arguments
  expect_function_arguments(add_note, expected = c("x", "note"),
                            info = "The function arguments are not named correctly.")

  # Expect self contained
  expect_function_self_contained(add_note, 
                                 info = "'add_note' contains variables not defined in the function")

  # Test cases (arguments)
  x1 <- list(info="testinfo1")
  note1 <- "new info"

  # Expect to run
  expect_silent(add_note(x = x1, note = note1))

  # Run functions
  test_res1 <- add_note(x = x1, note = note1)

  ## Expect results
  # Expect class
  expect_is(test_res1, "list", 
            info = "'add_note()' do not return a correct object.")

  # Expect dimensions
  expect_true(length(test_res1) == 2, 
              info = "'add_note()' do not return an object with correct dimensions.")
  expect_true(length(unlist(test_res1)) == 2, 
              info = "'add_note()' do not return an object with correct dimensions of elements.")

  # Expect names
  expect_true(all(names(test_res1) %in% c("note", "info")), 
              info = "'add_note()' do not return an object with correct names")

  # Expect results
  expect_true(test_res1$note == note1, 
              info = "'add_note()' returns erroneous values.")
})
