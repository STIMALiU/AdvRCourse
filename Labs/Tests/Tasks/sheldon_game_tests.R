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
  player1 <- c("rock", "spock", "paper", "scissors", "lizard")
  player2 <- c("lizard", "spock", "paper", "scissors", "rock")

  # Expect to run
  expect_silent(suppressWarnings(suppressMessages(sheldon_game(player1 = player1[1], player2 = player2[1]))))
  expect_silent(suppressWarnings(suppressMessages(sheldon_game(player1 = player1[2], player2 = player2[2]))))
  expect_silent(suppressWarnings(suppressMessages(sheldon_game(player1 = player1[3], player2 = player2[3]))))
  expect_silent(suppressWarnings(suppressMessages(sheldon_game(player1 = player1[4], player2 = player2[4]))))
  expect_silent(suppressWarnings(suppressMessages(sheldon_game(player1 = player1[5], player2 = player2[5]))))

  # Expect assertions
  expect_error(sheldon_game(player1 = 1, player2 = 3), 
              info = "'sheldon_game()' do not throw an error with erroneous inputs.")

  # Run functions
  test_res1 <- sheldon_game(player1 = player1[1], player2 = player2[1])
  test_res2 <- sheldon_game(player1 = player1[2], player2 = player2[2])
  test_res3 <- sheldon_game(player1 = player1[3], player2 = player2[3])
  test_res4 <- sheldon_game(player1 = player1[4], player2 = player2[4])
  test_res5 <- sheldon_game(player1 = player1[5], player2 = player2[5])
  test_res6 <- sheldon_game(player1 = player1[1], player2 = player2[5])
  test_res7 <- sheldon_game(player1 = player1[2], player2 = player2[1])
  test_res8 <- sheldon_game(player1 = player1[2], player2 = player2[3])
  test_res9 <- sheldon_game(player1 = player1[3], player2 = player2[2])
  test_res10 <- sheldon_game(player1 = player1[4], player2 = player2[5])

  ## Expect results
  # Expect class
  expect_is(test_res1, "character", 
            info = "'sheldon_game()' do not return a correct object.")
  expect_is(test_res2, "character", 
            info = "'sheldon_game()' do not return a correct object.")

  # Expect dimensions
  expect_true(length(test_res1) == 1, 
              info = "'sheldon_game()' do not return an object with correct dimensions.")


  # Expect results
  expect_true(tolower(test_res1) == "player 1 wins!", 
              info = "'sheldon_game()' returns erroneous results. Player 1 should win with 'rock' against 'lizard'.")
  expect_true(tolower(test_res2) == "draw!", 
              info = "'sheldon_game()' returns erroneous results. Should be draw with 'spock' against 'spock'.")
  expect_true(tolower(test_res3) == "draw!", 
              info = "'sheldon_game()' returns erroneous results. Should be draw with 'paper' against 'paper'.")
  expect_true(tolower(test_res4) == "draw!", 
              info = "'sheldon_game()' returns erroneous results. Should be draw with 'scissors' against 'scissors'.")
  expect_true(tolower(test_res5) == "player 2 wins!", 
              info = "'sheldon_game()' returns erroneous results. Player 2 should win with 'rock' against 'lizard'.")
  expect_true(tolower(test_res6) == "draw!", 
              info = "'sheldon_game()' returns erroneous results. Should be draw with 'rock' against 'rock'.")
  expect_true(tolower(test_res7) == "player 2 wins!", 
              info = "'sheldon_game()' returns erroneous results. Player 2 should win with 'spock' against 'lizard'.")
  expect_true(tolower(test_res8) == "player 2 wins!", 
              info = "'sheldon_game()' returns erroneous results. Player 2 should win with 'paper' against 'spock'.")
  expect_true(tolower(test_res9) == "player 1 wins!", 
              info = "'sheldon_game()' returns erroneous results. Player 1 should win with 'paper' against 'spock'.")
  expect_true(tolower(test_res10) == "player 2 wins!", 
              info = "'sheldon_game()' returns erroneous results. Player 2 should win with 'rock' against 'scissors'.")
})
