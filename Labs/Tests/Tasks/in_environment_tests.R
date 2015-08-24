### Assignment : in_environment() ###

context("in_environment()")

test_that("Assignment: in_environment()", {
  
  # Expect object
  expect_true(exists("in_environment"), info = "No object 'in_environment' exists.")
  
  # Expect class
  expect_is(in_environment, class = "function",
            info = "Object 'in_environment' is not a function.")

  # Expect arguments
  exp_args <- c("env")
  expect_function_arguments(in_environment, expected = exp_args,
                            info = paste0("The function arguments are not named correctly (",  
                                          paste(paste0("'",exp_args,"'"),collapse = ", "),
                                          ")."))
  # Expect self contained
  expect_self_contained(in_environment, 
                        info = "'in_environment' contains variables not defined in the function (free variables)")
  
  # Expect not folowing code in function
#  not_allowed <- "for"
#  expect_that(in_environment, not(function_code(not_allowed)), 
#              info = paste0("'in_environment' contains the code '", not_allowed, "' that is not allowed."))

  # Test cases (arguments)
  testenv <- search()[length(search())]

  # Expect to run
  expect_that(in_environment(env=testenv), 
              condition = not(throws_error()), 
              info = "'in_environment()' throws an error.")
  
  # Expect assertions
  expect_error(in_environment(x="hello", n="world"), 
              info = "'in_environment()' do not throw an error with erroneous inputs.")

  # Run functions
  test_res <- list()
  test_res[[1]] <- in_environment(env = testenv)

  ## Expect results
  # Expect class
  for (i in seq_along(test_res)){
    expect_is(test_res[[i]], "character", 
              info = "'in_environment()' do not return a correct object.")
  }

  # Expect dimensions
  dim_true <- c(1000)
  for (i in seq_along(test_res)){
    expect_true(length(test_res[[i]]) >= dim_true[i], 
                info = "'in_environment()' do not return an object with correct dimensions.")
  }
  
  # Expect names
#   expect_true(all(names(test_res1) == c("weight", "Time",  "correlation_matrix")), 
#               info = "'in_environment()' do not return an object with correct (ordered) variable names.")
#   expect_true(all(names(test_res2) == c("Time", "weight", "correlation_matrix")), 
#               info = "'in_environment()' do not return an object with correct (ordered) variable names.")  
#   expect_true(all(names(test_res1[[1]]) %in% c("mean", "median", "sd")), 
#               info = "'in_environment()' do not return an object with correct (ordered) variable names.")
  
  # Expect results
  result_list1 <- list(c("-.POSIXt",":"))
  for (i in seq_along(test_res)){
    expect_true(all(test_res[[i]][3:4] == result_list1[[i]]), 
                info = "'in_environment()' returns erroneous results.")
  }
})
