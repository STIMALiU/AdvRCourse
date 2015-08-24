### Assignment : trial_division_factorization() ###

context("trial_division_factorization()")

test_that("Assignment: trial_division_factorization()", {
  
  # Expect object
  expect_true(exists("trial_division_factorization"), info = "No object 'trial_division_factorization' exists.")
  
  # Expect class
  expect_is(trial_division_factorization, class = "function",
            info = "Object 'trial_division_factorization' is not a function.")

  # Expect arguments
  exp_args <- c("x")
  expect_function_arguments(trial_division_factorization, expected = exp_args,
                            info = paste0("The function arguments are not named correctly (",  
                                          paste(paste0("'",exp_args,"'"),collapse = ", "),
                                          ")."))
  # Expect self contained
  expect_self_contained(trial_division_factorization, 
                        info = "'trial_division_factorization' contains variables not defined in the function (free variables)")
  
  # Expect not folowing code in function
#   not_allowed <- c("for")
#   for (not_ok in not_allowed){
#     expect_that(trial_division_factorization, not(function_code(not_ok)), 
#                 info = paste0("'trial_division_factorization' contains the code '", not_ok, "' that is not allowed."))
#   }
  
  # Test cases (arguments)
  x <- list(2^5, 3*5*7*11*13*17*23*29)

  # Expect to run
  for(i in seq_along(x)){
    expect_that(trial_division_factorization(x=x[[i]]), 
                condition = not(throws_error()), 
                info = "'trial_division_factorization()' throws an error.")
  }

  # Expect assertions
#  expect_error(trial_division_factorization(x=1:4, n=c(4,5)),
#              info = "'trial_division_factorization()' do not throw an error with erroneous inputs.")

  # Run functions
  test_res <- list()
  for(i in seq_along(x)){
    test_res[[i]] <- trial_division_factorization(x=x[[i]])
  }
  
  ## Expect results
  # Expect class
  for (i in seq_along(test_res)){
    expect_is(test_res[[i]], "numeric", 
              info = "'trial_division_factorization()' do not return a correct object.")
  }

  # Expect dimensions
  dim_res <- list(5,8)
  for (i in seq_along(test_res)){
    expect_true(all(length(test_res[[i]]) == dim_res[[i]]), 
                info = "'trial_division_factorization()' do not return an object with correct dimensions.")
  }
  
  # Expect names
#   row_res <- list(c("7", "8"), c("56", "57"))
#   col_res <- list(c("7", "8"), c("56", "57"))
#   for (i in seq_along(test_res)){
#     expect_true(all(rownames(test_res[[i]])[1:2] == row_res[[i]]), 
#                 info = "'trial_division_factorization()' do not return an object with correct (ordered) variable names.")
#     expect_true(all(colnames(test_res[[i]])[1:2] == col_res[[i]]), 
#                 info = "'trial_division_factorization()' do not return an object with correct (ordered) variable names.")
#   }

  # Expect results
  result_list1 <- list(c(2,2,2,2,2), c(3, 5, 7, 11, 13, 17, 23, 29))
  for (i in seq_along(test_res)){
    expect_true(all(test_res[[i]] == result_list1[[i]]), 
                info = "'trial_division_factorization()' returns erroneous results.")
  }
})
