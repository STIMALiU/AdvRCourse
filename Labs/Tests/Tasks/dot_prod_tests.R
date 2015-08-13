### Assignment : dot_prod ###

context("dot_prod()")

test_that("dot_prod()", {
  expect_that(exists("dot_prod"), is_true(), 
              info = "Object dot_prod() is missing.")
  expect_that(dot_prod, is_a("function"),
              info = "dot_prod is not a function.")
  expect_self_contained(object = dot_prod,
                        "dot_prod() contains undefined variables.")
  expect_that(all(names(formals(dot_prod)) %in% c("a", "b")), condition=is_true(),
              info = "The argument names are incorrect.")
  expect_that(is.numeric(dot_prod(a=1:3, b=4:6)), condition=is_true(),
              info = "dot_prod() do not return a numeric value.")
  expect_equal(length(dot_prod(a=1:3, b=4:6)), 1,
              info = "dot_prod() do not return a scalar.")
  expect_equal(dot_prod(a=1:3, b=4:6), 32,
               info = "dot_prod() do not return a correct result.")
  expect_equal(dot_prod(a=5:10, b=20:25), 1030,
               info = "dot_prod() do not return a correct result.")
  
})

