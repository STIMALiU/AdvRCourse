### Assignment : mcmc_counter_factory() ###

context("mcmc_counter_factory()")

test_that("Assignment: mcmc_counter_factory()", {
  
  # Expect object
  expect_true(exists("mcmc_counter_factory"), info = "No object 'mcmc_counter_factory' exists.")
  
  # Expect class
  expect_is(mcmc_counter_factory, class = "function",
            info = "Object 'mcmc_counter_factory' is not a function.")

  # Expect arguments
  exp_args <- c("burnin", "thin")
  expect_function_arguments(mcmc_counter_factory, expected = exp_args,
                            info = paste0("The function arguments are not named correctly (",  
                                          paste(paste0("'",exp_args,"'"),collapse = ", "),
                                          ")."))
  # Expect self contained
  expect_function_self_contained(mcmc_counter_factory, 
                        info = "'mcmc_counter_factory' contains variables not defined in the function (free variables)")
  
  # Expect not folowing code in function
#  not_allowed <- "for"
#  expect_that(mcmc_counter_factory, not(function_code(not_allowed)), 
#              info = paste0("'mcmc_counter_factory' contains the code '", not_allowed, "' that is not allowed."))

  # Test cases (arguments)
  burnin <- list(5, 1)
  thin <- list(1, 2)

  # Expect to run
  for(i in seq_along(burnin)){
    expect_silent(suppressWarnings(suppressMessages(mcmc_counter_factory(burnin = burnin[[i]], thin = thin[[i]]))))
  }

  # Expect assertions
  expect_error(mcmc_counter_factory(burnin=-1, thin=1), 
              info = "'mcmc_counter_factory()' do not throw an error with erroneous inputs.")

  # Run functions
  test_res <- list() 
  for(i in seq_along(burnin)){
    test_res[[i]] <- mcmc_counter_factory(burnin = burnin[[i]], thin = thin[[i]])
  }

  ## Expect results
  # Expect class
  for (i in seq_along(test_res)){
    expect_is(test_res[[i]], "function", 
              info = "'mcmc_counter_factory()' do not return a correct object.")
  }

  # Expect dimensions
#  for (i in seq_along(test_res)){
#    expect_true(length(test_res[[i]]) == 4, 
#                info = "'mcmc_counter_factory()' do not return an object with correct dimensions.")
#  }
  
  # Expect names
#   expect_true(all(names(test_res1) == c("weight", "Time",  "correlation_matrix")), 
#               info = "'mcmc_counter_factory()' do not return an object with correct (ordered) variable names.")
#   expect_true(all(names(test_res2) == c("Time", "weight", "correlation_matrix")), 
#               info = "'mcmc_counter_factory()' do not return an object with correct (ordered) variable names.")  
#   expect_true(all(names(test_res1[[1]]) %in% c("mean", "median", "sd")), 
#               info = "'mcmc_counter_factory()' do not return an object with correct (ordered) variable names.")
  
  # Expect results
  result_list1 <- list(unlist(list(11, TRUE, 6)), unlist(list(11, TRUE, 5)))
  for (i in seq_along(test_res)){ # i <- 2
    for(j in 1:10){
      test_res[[i]]()
    }
    expect_true(all(unlist(test_res[[i]]()) == result_list1[[i]]), 
                info = "'mcmc_counter_factory()' returns erroneous results.")
  }
})
