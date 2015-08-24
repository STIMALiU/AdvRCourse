### Assignment : repeat_find_cumsum() ###

context("repeat_find_cumsum()")

test_that("Assignment: repeat_find_cumsum()", {
  
  # Expect object
  expect_true(exists("repeat_find_cumsum"), info = "No object 'repeat_find_cumsum' exists.")
  
  # Expect class
  expect_is(repeat_find_cumsum, class = "function",
            info = "Object 'repeat_find_cumsum' is not a function.")

  # Expect arguments
  exp_args <- c("x", "find_sum")
  expect_function_arguments(repeat_find_cumsum, expected = exp_args,
                            info = paste0("The function arguments are not named correctly (",  
                                          paste(paste0("'",exp_args,"'"),collapse = ", "),
                                          ")."))
  # Expect self contained
  expect_self_contained(repeat_find_cumsum, 
                        info = "'repeat_find_cumsum' contains variables not defined in the function (free variables)")
  
  # Expect not folowing code in function
  not_allowed <- c("for", "while")
  for (not_ok in not_allowed){
    expect_that(repeat_find_cumsum, not(function_code(not_ok)), 
                info = paste0("'repeat_find_cumsum' contains the code '", not_ok, "' that is not allowed."))
  }
  
  
  # Test cases (arguments)
  x <- list(100:150, 10:20)
  find_sum <- list(2000, 166)

  # Expect to run
  expect_that(repeat_find_cumsum(x=x[[1]], find_sum=find_sum[[1]]), 
              condition = not(throws_error()), 
              info = "'repeat_find_cumsum()' throws an error.")
  expect_that(repeat_find_cumsum(x=x[[2]], find_sum=find_sum[[2]]), 
              condition = not(throws_error()), 
              info = "'repeat_find_cumsum()' throws an error.")
  
  # Expect assertions
  expect_error(repeat_find_cumsum(x=1:4, n=c(4,5)),
              info = "'repeat_find_cumsum()' do not throw an error with erroneous inputs.")

  # Run functions
  test_res <- list()
  test_res[[1]] <- repeat_find_cumsum(x=x[[1]], find_sum=find_sum[[1]])
  test_res[[2]] <- repeat_find_cumsum(x=x[[2]], find_sum=find_sum[[2]])
  
  ## Expect results
  # Expect class
  for (i in seq_along(test_res)){
    expect_is(test_res[[i]], "numeric", 
              info = "'repeat_find_cumsum()' do not return a correct object.")
  }

  # Expect dimensions
  dim_res <- list(1,1)
  for (i in seq_along(test_res)){
    expect_true(all(length(test_res[[i]]) == dim_res[[i]]), 
                info = "'repeat_find_cumsum()' do not return an object with correct dimensions.")
  }
  
  # Expect names
#   row_res <- list(c("7", "8"), c("56", "57"))
#   col_res <- list(c("7", "8"), c("56", "57"))
#   for (i in seq_along(test_res)){
#     expect_true(all(rownames(test_res[[i]])[1:2] == row_res[[i]]), 
#                 info = "'repeat_find_cumsum()' do not return an object with correct (ordered) variable names.")
#     expect_true(all(colnames(test_res[[i]])[1:2] == col_res[[i]]), 
#                 info = "'repeat_find_cumsum()' do not return an object with correct (ordered) variable names.")
#   }

  # Expect results
  result_list1 <- list(2071, 165)
  for (i in seq_along(test_res)){
    expect_true(test_res[[i]] == result_list1[[i]], 
                info = "'repeat_find_cumsum()' returns erroneous results.")
  }
})
