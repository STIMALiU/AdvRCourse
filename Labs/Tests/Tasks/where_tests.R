### Assignment : where() ###

context("where()")

test_that("Assignment: where()", {
  
  # Expect object
  expect_true(exists("where"), info = "No object 'where' exists.")
  
  # Expect class
  expect_is(where, class = "function",
            info = "Object 'where' is not a function.")

  # Expect arguments
  exp_args <- c("fun")
  expect_function_arguments(where, expected = exp_args,
                            info = paste0("The function arguments are not named correctly (",  
                                          paste(paste0("'",exp_args,"'"),collapse = ", "),
                                          ")."))
  # Expect self contained
  expect_function_self_contained(where, 
                        info = "'where' contains variables not defined in the function (free variables)")
  
  # Expect not folowing code in function
#  not_allowed <- "for"
#  expect_that(where, not(function_code(not_allowed)), 
#              info = paste0("'where' contains the code '", not_allowed, "' that is not allowed."))

  # Test cases (arguments)
  testfun <- c("read.table", "lm", "blahblah")

  # Expect to run
  for(i in seq_along(testfun)){
    expect_silent(suppressWarnings(suppressMessages(where(fun=testfun[i]))))
  }

  # Expect assertions
  expect_error(where(fun=1), 
              info = "'where()' do not throw an error with erroneous inputs.")

  # Run functions
  test_res <- list() 
  for(i in seq_along(testfun)){
    test_res[[i]] <- where(fun = testfun[i])
  }

  ## Expect results
  # Expect class
  for (i in seq_along(test_res)){
    expect_is(test_res[[i]], "character", 
              info = "'where()' do not return a correct object.")
  }

  # Expect dimensions
  for (i in seq_along(test_res)){
    expect_true(length(test_res[[i]]) == 1, 
                info = "'where()' do not return an object with correct dimensions.")
  }
  
  # Expect names
#   expect_true(all(names(test_res1) == c("weight", "Time",  "correlation_matrix")), 
#               info = "'where()' do not return an object with correct (ordered) variable names.")
#   expect_true(all(names(test_res2) == c("Time", "weight", "correlation_matrix")), 
#               info = "'where()' do not return an object with correct (ordered) variable names.")  
#   expect_true(all(names(test_res1[[1]]) %in% c("mean", "median", "sd")), 
#               info = "'where()' do not return an object with correct (ordered) variable names.")
  
  # Expect results
  result_list1 <- list("package:utils", "package:stats", "blahblah not found!")
  for (i in seq_along(test_res)){
    expect_true(test_res[[i]] == result_list1[[i]], 
                info = "'where()' returns erroneous results.")
  }
})
