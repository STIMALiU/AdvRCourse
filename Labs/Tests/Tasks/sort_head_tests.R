### Assignment : sort_head() ###

context("sort_head()")

test_that("Assignment: sort_head()", {
  
  # Expect object
  expect_true(exists("sort_head"), info = "No object 'sort_head' exists.")
  
  # Expect class
  expect_is(sort_head, class = "function",
            info = "Object 'sort_head' is not a function.")

  # Expect arguments
  expect_function_arguments(sort_head, expected = c("df", "var.name", "n"),
                            info = "The function arguments are not named correctly.")

  # Expect self contained
  expect_self_contained(sort_head, 
                        info = "'sort_head' contains variables not defined in the function")

  # Test cases (arguments)
  data("ChickWeight")
  df1 <- ChickWeight
  var.name1 <- "weight"
  n1 <- 3
  
  # Expect to run
  expect_that(sort_head(df = df1, var.name = var.name1, n = n1), 
              condition = not(throws_error()), 
              info = "'sort_head()' throws an error.")

  # Run functions
  test_res1 <- sort_head(df = df1, var.name = var.name1, n = n1)

  ## Expect results
  # Expect class
  expect_is(test_res1, "data.frame", 
            info = "'sort_head()' do not return a correct object.")

  # Expect dimensions
  expect_true(all(dim(test_res1) == c(3,4)), 
              info = "'sort_head()' do not return an object with correct dimensions.")

  # Expect names
  expect_true(all(names(test_res1) %in% c("weight","Time","Chick","Diet")), 
              info = "'sort_head()' do not return an object with correct variable names.")
  
  # Expect results
  expect_true(sum(test_res1$weight) == 1075, 
              info = "'sort_head()' returns erroneous values.")
  expect_true(sum(test_res1$Time) == 62, 
              info = "'sort_head()' returns erroneous values.")
})
