test_that("as_numeric_without_warnings() works", {
  x <- c(1, 2, "1", "03", "a", NA)
  y <- c(1, 2,  1,    3,   NA, NA)
  expect_silent(as_numeric_without_warnings(x))
  expect_equal( as_numeric_without_warnings(x), y)
})
