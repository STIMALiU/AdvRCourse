### Assignment : cov() ###

context("cov()")

test_that("Assignment: cov()", {
  
  # Expect object
  expect_true(exists("cov"), info = "No object 'cov' exists.")
  
  # Expect class
  expect_is(cov, class = "function",
            info = "Object 'cov' is not a function.")

  # Expect arguments
  exp_args <- c("X")
  expect_function_arguments(cov, expected = exp_args,
                            info = paste0("The function arguments are not named correctly (",  
                                          paste(paste0("'",exp_args,"'"),collapse = ", "),
                                          ")."))
  # Expect self contained
  expect_self_contained(cov, 
                        info = "'cov' contains variables not defined in the function (free variables)")
  
  # Expect not folowing code in function
  not_allowed <- "for"
  expect_that(cov, not(function_code(not_allowed)), 
              info = paste0("'cov' contains the code '", not_allowed, "' that is not allowed."))

  # Test cases (arguments)
  data(iris)
  X <- list(iris[,1:4])

  # Expect to run
  for(i in seq_along(X)){
    expect_that(cov(X=X[[i]]), 
                condition = not(throws_error()), 
                info = "'cov()' throws an error.")
  }

  # Expect assertions
  expect_error(cov(X=1), 
              info = "'cov()' do not throw an error with erroneous inputs.")

  # Run functions
  test_res <- list() 
  for(i in seq_along(X)){
    test_res[[i]] <- cov(X = X[[i]])
  }

  ## Expect results
  # Expect class
  for (i in seq_along(test_res)){
    expect_is(test_res[[i]], "numeric", 
              info = "'cov()' do not return a correct object.")
  }

  # Expect dimensions
  for (i in seq_along(test_res)){
    expect_true(length(test_res[[i]]) == 4, 
                info = "'cov()' do not return an object with correct dimensions.")
  }
  
  # Expect names
#   expect_true(all(names(test_res1) == c("weight", "Time",  "correlation_matrix")), 
#               info = "'cov()' do not return an object with correct (ordered) variable names.")
#   expect_true(all(names(test_res2) == c("Time", "weight", "correlation_matrix")), 
#               info = "'cov()' do not return an object with correct (ordered) variable names.")  
#   expect_true(all(names(test_res1[[1]]) %in% c("mean", "median", "sd")), 
#               info = "'cov()' do not return an object with correct (ordered) variable names.")
  
  # Expect results
  result_list1 <- list(c(0.14, 0.14, 0.47, 0.64))
  for (i in seq_along(test_res)){
    expect_true(all(round(test_res[[i]],2) == result_list1[[i]]), 
                info = "'cov()' returns erroneous results.")
  }
})
