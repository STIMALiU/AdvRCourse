### Assignment : approx_e ###

context("approx_e()")

test_that("Kontroll av approx_e.", {
  expect_that(exists("approx_e"), expect_true(),
              info = "Object approx_e() is missing")
  expect_that(approx_e, is_a("function"),
              info = "approx_e is not a function.")
  expect_self_contained(object = approx_e,
                        "approx_e() contains undefined variables.")
  expect_that(all(names(formals(approx_e)) %in% c("N")), condition=expect_true(),
              info = "Arguments in approx_e are missing or misspelled.")
  expect_equal(is.numeric(approx_e(2)), TRUE,
               info = "approx_e() do not return a numeric scalar.")
  expect_equal(approx_e(2), 2.5,
              info = "approx_e() returns an erroneous result.")
  expect_equal(approx_e(4), 2.708333, tolerance=0.0001,
               info = "approx_e() returns an erroneous result.")
  expect_equal(approx_e(100), exp(1), 
               info = "approx_e() returns an erroneous result.")
})

