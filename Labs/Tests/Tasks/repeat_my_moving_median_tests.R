### Assignment : repeat_my_moving_median() ###

context("repeat_my_moving_median()")

test_that("Assignment: repeat_my_moving_median()", {
  
  # Expect object
  expect_true(exists("repeat_my_moving_median"), info = "No object 'repeat_my_moving_median' exists.")
  
  # Expect class
  expect_is(repeat_my_moving_median, class = "function",
            info = "Object 'repeat_my_moving_median' is not a function.")

  # Expect arguments
  exp_args <- c("x", "n", "...")
  expect_function_arguments(repeat_my_moving_median, expected = exp_args,
                            info = paste0("The function arguments are not named correctly (",  
                                          paste(paste0("'",exp_args,"'"),collapse = ", "),
                                          ")."))
  # Expect self contained
  expect_self_contained(repeat_my_moving_median, 
                        info = "'repeat_my_moving_median' contains variables not defined in the function (free variables)")
  
  # Expect not folowing code in function
  not_allowed <- "for"
  expect_that(repeat_my_moving_median, not(function_code(not_allowed)), 
              info = paste0("'repeat_my_moving_median' contains the code '", not_allowed, "' that is not allowed."))

  # Test cases (arguments)
  set.seed(42)
  x <- rnorm(100)
  x[1:2]<-NA
  n <- c(2, 5)

  # Expect to run
  expect_that(repeat_my_moving_median(x=x, n=n[1]), 
              condition = not(throws_error()), 
              info = "'repeat_my_moving_median()' throws an error.")
  expect_that(repeat_my_moving_median(x=x, n=n[2]), 
              condition = not(throws_error()), 
              info = "'repeat_my_moving_median()' throws an error.")
  expect_that(repeat_my_moving_median(x=x, n=n[1], na.rm=TRUE), 
              condition = not(throws_error()), 
              info = "'repeat_my_moving_median()' throws an error.")

  
  # Expect assertions
  expect_error(repeat_my_moving_median(x="hello", n="world"), 
              info = "'repeat_my_moving_median()' do not throw an error with erroneous inputs.")

  # Run functions
  test_res <- list()
  test_res[[1]] <- repeat_my_moving_median(x=x, n=n[1])
  test_res[[2]] <- repeat_my_moving_median(x=x, n=n[2])
  test_res[[3]] <- repeat_my_moving_median(x=x, n=n[1], na.rm=TRUE)

  ## Expect results
  # Expect class
  for (i in seq_along(test_res)){
    expect_is(test_res[[i]], "numeric", 
              info = "'repeat_my_moving_median()' do not return a correct object.")
  }

  # Expect dimensions
  dim_true <- c(98,95,98)
  for (i in seq_along(test_res)){
    expect_true(length(test_res[[i]]) == dim_true[i], 
                info = "'repeat_my_moving_median()' do not return an object with correct dimensions.")
  }
  
  # Expect names
#   expect_true(all(names(test_res1) == c("weight", "Time",  "correlation_matrix")), 
#               info = "'repeat_my_moving_median()' do not return an object with correct (ordered) variable names.")
#   expect_true(all(names(test_res2) == c("Time", "weight", "correlation_matrix")), 
#               info = "'repeat_my_moving_median()' do not return an object with correct (ordered) variable names.")  
#   expect_true(all(names(test_res1[[1]]) %in% c("mean", "median", "sd")), 
#               info = "'repeat_my_moving_median()' do not return an object with correct (ordered) variable names.")
  
  # Expect results
  result_list1 <- list(c(0.404,0.404),
                      c(0.384,0.519),
                      c(0.404,0.404))
  result_list2 <- list(2,2,0)
  for (i in seq_along(test_res)){
    expect_true(all(round(test_res[[i]][3:4],3) == result_list1[[i]]), 
                info = "'repeat_my_moving_median()' returns erroneous results.")
    expect_true(sum(is.na(test_res[[i]][1:4])) == result_list2[[i]], 
                info = "'repeat_my_moving_median()' returns erroneous results.")
  }
})
