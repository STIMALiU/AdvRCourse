### Assignment structure ###

context("Mandatoy objects")

test_that("Mandatory objects", {
  expect_true(exists("name"), info = "'name' is missing.")
  expect_true(exists("liuid"), info = "'liuid' is missing")

  expect_is(name, "character", info = "'name' should be a character vector.")
  expect_is(liuid, "character", info = "'liuid' should be a character vector.")
  
  expect_true(nchar(name) > 0, info = "Write your name in 'name'")
  expect_true(nchar(liuid) > 0, info = "Write your name in 'liuid'")
})

