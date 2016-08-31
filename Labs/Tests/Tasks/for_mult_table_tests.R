### Assignment : for_mult_table() ###

context("for_mult_table()")

test_that("Assignment: for_mult_table()", {
  
  # Expect object
  expect_true(exists("for_mult_table"), info = "No object 'for_mult_table' exists.")
  
  # Expect class
  expect_is(for_mult_table, class = "function",
            info = "Object 'for_mult_table' is not a function.")

  # Expect arguments
  exp_args <- c("from", "to")
  expect_function_arguments(for_mult_table, expected = exp_args,
                            info = paste0("The function arguments are not named correctly (",  
                                          paste(paste0("'",exp_args,"'"),collapse = ", "),
                                          ")."))
  # Expect self contained
  expect_function_self_contained(for_mult_table, 
                        info = "'for_mult_table' contains variables not defined in the function (free variables)")
  
  # Expect not folowing code in function
  #not_allowed <- "while"
  #expect_that(for_mult_table, not(function_code(not_allowed)), 
  #            info = paste0("'for_mult_table' contains the code '", not_allowed, "' that is not allowed."))

  # Test cases (arguments)
  from <- c(7, 56)
  to <- c(8, 58)
  
  # Expect to run
  expect_silent(suppressWarnings(suppressMessages(for_mult_table(from = from[1], to=to[1]))))
  expect_silent(suppressWarnings(suppressMessages(for_mult_table(from = from[2], to=to[2]))))

  
  # Expect assertions
  expect_error(for_mult_table(from=1:3, to="world"),
              info = "'for_mult_table()' do not throw an error with erroneous inputs.")

  # Run functions
  test_res <- list()
  test_res[[1]] <- for_mult_table(from = from[1], to=to[1])
  test_res[[2]] <- for_mult_table(from = from[2], to=to[2])

  ## Expect results
  # Expect class
  for (i in seq_along(test_res)){
    expect_is(test_res[[i]], "matrix", 
              info = "'for_mult_table()' do not return a correct object.")
  }

  # Expect dimensions
  dim_res <- list(c(2,2),c(3,3))
  for (i in seq_along(test_res)){
    expect_true(all(dim(test_res[[i]]) == dim_res[[i]]), 
                info = "'for_mult_table()' do not return an object with correct dimensions.")
  }
  
  # Expect names
  row_res <- list(c("7", "8"), c("56", "57"))
  col_res <- list(c("7", "8"), c("56", "57"))
  for (i in seq_along(test_res)){
    expect_true(all(rownames(test_res[[i]])[1:2] == row_res[[i]]), 
                info = "'for_mult_table()' do not return an object with correct (ordered) variable names.")
    expect_true(all(colnames(test_res[[i]])[1:2] == col_res[[i]]), 
                info = "'for_mult_table()' do not return an object with correct (ordered) variable names.")
  }

  # Expect results
  result_list1 <- list(c(49,56,56),
                       c(3136,3192,3248))
  for (i in seq_along(test_res)){
    expect_true(all(as.vector(test_res[[i]])[1:3] == result_list1[[i]]), 
                info = "'for_mult_table()' returns erroneous results.")
  }
})
