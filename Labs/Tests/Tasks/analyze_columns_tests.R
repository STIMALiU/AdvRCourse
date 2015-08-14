### Assignment : analyze_columns() ###

context("analyze_columns()")

test_that("Assignment: analyze_columns()", {
  
  # Expect object
  expect_true(exists("analyze_columns"), info = "No object 'analyze_columns' exists.")
  
  # Expect class
  expect_is(analyze_columns, class = "function",
            info = "Object 'analyze_columns' is not a function.")

  # Expect arguments
  expect_function_arguments(analyze_columns, expected = c("df", "j"),
                            info = "The function arguments are not named correctly.")

  # Expect self contained
  expect_self_contained(analyze_columns, 
                        info = "'analyze_columns' contains variables not defined in the function")

  # Test cases (arguments)
  data("ChickWeight")
  df1 <- ChickWeight
  j1 <- 1:2
  j2 <- 2:1

  # Expect to run
  expect_that(analyze_columns(df = df1, j = j1), 
              condition = not(throws_error()), 
              info = "'analyze_columns()' throws an error.")
  expect_that(analyze_columns(df = df1, j = j2), 
              condition = not(throws_error()), 
              info = "'analyze_columns()' throws an error.")

  # Run functions
  test_res1 <- analyze_columns(df = df1, j = j1)
  test_res2 <- analyze_columns(df = df1, j = j2)

  ## Expect results
  # Expect class
  expect_is(test_res1, "list", 
            info = "'analyze_columns()' do not return a correct object.")
  expect_is(test_res2, "list", 
            info = "'analyze_columns()' do not return a correct object.")
  expect_is(test_res1[[1]], "numeric", 
            info = "'analyze_columns()' do not return a correct object.")
  expect_is(test_res1[[2]], "numeric", 
            info = "'analyze_columns()' do not return a correct object.")
  expect_is(test_res1[[3]], "matrix", 
            info = "'analyze_columns()' do not return a correct object.")
  
  # Expect dimensions
  expect_true(length(test_res1) == 3, 
              info = "'analyze_columns()' do not return an object with correct dimensions.")

  # Expect names
  expect_true(all(names(test_res1) == c("weight", "Time",  "correlation_matrix")), 
              info = "'analyze_columns()' do not return an object with correct (ordered) variable names.")
  expect_true(all(names(test_res2) == c("Time", "weight", "correlation_matrix")), 
              info = "'analyze_columns()' do not return an object with correct (ordered) variable names.")  
  expect_true(all(names(test_res1[[1]]) %in% c("mean", "median", "sd")), 
              info = "'analyze_columns()' do not return an object with correct (ordered) variable names.")
  
  # Expect results
  expect_true(all(round(test_res1$weight) == c(122, 103, 71)), 
              info = "'analyze_columns()' returns erroneous values (mean/median/sd).")
  expect_true(all(round(test_res1$Time) == c(11, 10, 7)), 
              info = "'analyze_columns()' returns erroneous values (mean/median/sd).")
  expect_true(all(as.vector(round(test_res1$correlation_matrix,2)) == c(1.00, 0.84, 0.84, 1.00)), 
              info = "'analyze_columns()' returns erroneous values (correlation matrix).")
})
