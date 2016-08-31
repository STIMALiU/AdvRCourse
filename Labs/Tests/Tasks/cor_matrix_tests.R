### Assignment : cor_matrix() ###

context("cor_matrix()")

test_that("Assignment: cor_matrix()", {
  
  # Expect object
  expect_true(exists("cor_matrix"), info = "No object 'cor_matrix' exists.")
  
  # Expect class
  expect_is(cor_matrix, class = "function",
            info = "Object 'cor_matrix' is not a function.")

  # Expect arguments
  exp_args <- c("X")
  expect_function_arguments(cor_matrix, expected = exp_args,
                            info = paste0("The function arguments are not named correctly (",  
                                          paste(paste0("'",exp_args,"'"),collapse = ", "),
                                          ")."))
  # Expect self contained
  expect_function_self_contained(cor_matrix, 
                        info = "'cor_matrix' contains variables not defined in the function (free variables)")
  
  # Expect not folowing code in function
  #not_allowed <- c("var\\(", "sd\\(", "cor\\(")
  #for (not_ok in not_allowed){
  #  expect_that(cor_matrix, not(function_code(not_ok)), 
  #              info = paste0("'cor_matrix' contains the code '", not_ok, "' that is not allowed."))
  #}
  
  # Test cases (arguments)
  data(iris)
  X <- iris[2:4]
  
  # Expect to run
  expect_silent(suppressWarnings(suppressMessages(cor_matrix(X=X))))
  
  
  # Expect assertions
  expect_error(cor_matrix(X=as.matrix(X)),
              info = "'cor_matrix()' do not throw an error with erroneous inputs.")

  # Run functions
  test_res <- list()
  test_res[[1]] <- cor_matrix(X = X)

  ## Expect results
  # Expect class
  for (i in seq_along(test_res)){
    expect_is(test_res[[i]], "matrix", 
              info = "'cor_matrix()' do not return a correct object.")
  }

  # Expect dimensions
  dim_res <- list(c(3,3))
  for (i in seq_along(test_res)){
    expect_true(all(dim(test_res[[i]]) == dim_res[[i]]), 
                info = "'cor_matrix()' do not return an object with correct dimensions.")
  }
  
  # Expect names
#   row_res <- list(c("7", "8"), c("56", "57"))
#   col_res <- list(c("7", "8"), c("56", "57"))
#   for (i in seq_along(test_res)){
#     expect_true(all(rownames(test_res[[i]])[1:2] == row_res[[i]]), 
#                 info = "'cor_matrix()' do not return an object with correct (ordered) variable names.")
#     expect_true(all(colnames(test_res[[i]])[1:2] == col_res[[i]]), 
#                 info = "'cor_matrix()' do not return an object with correct (ordered) variable names.")
#   }

  # Expect results
  result_list1 <- list(c(1.00,-0.43,-0.37))
  for (i in seq_along(test_res)){
    expect_true(all(round(as.vector(test_res[[i]])[1:3],2) == result_list1[[i]]), 
                info = "'cor_matrix()' returns erroneous results.")
  }
})
