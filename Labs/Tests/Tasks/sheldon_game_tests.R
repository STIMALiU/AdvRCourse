### Assignment : sheldon_game() ###

context("sheldon_game()")

test_that("Assignment: sheldon_game()", {
  
  # Expect object
  expect_true(exists("sheldon_game"), info = "No object 'sheldon_game' exists.")
  
  # Expect class
  expect_is(sheldon_game, class = "function",
            info = "Object 'sheldon_game' is not a function.")

  # Expect arguments
  exp_args <- c("player1", "player2")
  expect_function_arguments(sheldon_game, expected = exp_args,
                            info = paste0("The function arguments are not named correctly (",  
                                          paste(paste0("'",exp_args,"'"),collapse = ", "),
                                          ")."))
  # Expect self contained
  expect_function_self_contained(sheldon_game, 
                        info = "'sheldon_game' contains variables not defined in the function (free variables)")
  
  # Expect not folowing code in function
  #not_allowed <- "%*%"
  #expect_that(sheldon_game, not(function_code(not_allowed)), 
  #            info = paste0("'sheldon_game' contains the code '", not_allowed, "' that is not allowed."))

  # Test cases (arguments)
  player1 <- c("rock", "spock")
  player2 <- c("lizard", "spock")

  # Expect to run
  expect_silent(suppressWarnings(suppressMessages(sheldon_game(player1 = player1[1], player2 = player2[1]))))
  expect_silent(suppressWarnings(suppressMessages(sheldon_game(player1 = player1[2], player2 = player2[2]))))
  
  # Expect assertions
  expect_error(sheldon_game(player1 = 1, player2 = 3), 
              info = "'sheldon_game()' do not throw an error with erroneous inputs.")

  # Run functions
  test_res1 <- sheldon_game(player1 = player1[1], player2 = player2[1])
  test_res2 <- sheldon_game(player1 = player1[2], player2 = player2[2])

  ## Expect results
  # Expect class
  expect_is(test_res1, "character", 
            info = "'sheldon_game()' do not return a correct object.")
  expect_is(test_res2, "character", 
            info = "'sheldon_game()' do not return a correct object.")

  # Expect dimensions
  expect_true(length(test_res1) == 1, 
              info = "'sheldon_game()' do not return an object with correct dimensions.")

  # Expect names
#   expect_true(all(names(test_res1) == c("weight", "Time",  "correlation_matrix")), 
#               info = "'sheldon_game()' do not return an object with correct (ordered) variable names.")
#   expect_true(all(names(test_res2) == c("Time", "weight", "correlation_matrix")), 
#               info = "'sheldon_game()' do not return an object with correct (ordered) variable names.")  
#   expect_true(all(names(test_res1[[1]]) %in% c("mean", "median", "sd")), 
#               info = "'sheldon_game()' do not return an object with correct (ordered) variable names.")
  
  # Expect results
  expect_true(tolower(test_res1) == "player 1 wins!", 
              info = "'sheldon_game()' returns erroneous results.")
  expect_true(tolower(test_res2) == "draw!", 
              info = "'sheldon_game()' returns erroneous results.")
})
