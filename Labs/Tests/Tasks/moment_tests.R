### Assignment : moment() ###

context("moment()")

test_that("Assignment: moment()", {
  
  # Expect object
  expect_true(exists("moment"), info = "No object 'moment' exists.")
  
  # Expect class
  expect_is(moment, class = "function",
            info = "Object 'moment' is not a function.")

  # Expect arguments
  exp_args <- c("i")
  expect_function_arguments(moment, expected = exp_args,
                            info = paste0("The function arguments are not named correctly (",  
                                          paste(paste0("'",exp_args,"'"),collapse = ", "),
                                          ")."))
  # Expect self contained
  expect_function_self_contained(moment, 
                        info = "'moment' contains variables not defined in the function (free variables)")
  
  # Expect not folowing code in function
#  not_allowed <- "for"
#  expect_that(moment, not(function_code(not_allowed)), 
#              info = paste0("'moment' contains the code '", not_allowed, "' that is not allowed."))

  # Test cases (arguments)
  mom <- list(1,2,4)

  # Expect to run
  for(i in seq_along(mom)){
    expect_silent(suppressWarnings(suppressMessages(moment(i=mom[[i]]))))
  }

  # Expect assertions
  expect_error(moment(i="hello world"), 
              info = "'moment()' do not throw an error with erroneous inputs.")

  # Run functions
  test_res <- list() 
  for(i in seq_along(mom)){
    test_res[[i]] <- moment(i = mom[[i]])
  }

  ## Expect results
  # Expect class
  for (i in seq_along(test_res)){
    expect_is(test_res[[i]], "function", 
              info = "'moment()' do not return a correct object.")
  }

  # Expect dimensions
#  for (i in seq_along(test_res)){
#    expect_true(length(test_res[[i]]) == 4, 
#                info = "'moment()' do not return an object with correct dimensions.")
#  }
  
  # Expect names
#   expect_true(all(names(test_res1) == c("weight", "Time",  "correlation_matrix")), 
#               info = "'moment()' do not return an object with correct (ordered) variable names.")
#   expect_true(all(names(test_res2) == c("Time", "weight", "correlation_matrix")), 
#               info = "'moment()' do not return an object with correct (ordered) variable names.")  
#   expect_true(all(names(test_res1[[1]]) %in% c("mean", "median", "sd")), 
#               info = "'moment()' do not return an object with correct (ordered) variable names.")
  
  # Expect results
  result_list1 <- list(0, 833, 1249583)
  for (i in seq_along(test_res)){
    expect_true(round(test_res[[i]](1:100)) == result_list1[[i]], 
                info = "'moment()' returns erroneous results.")
  }
})
