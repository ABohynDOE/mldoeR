test_that("num2char works", {
  expect_equal(num2char(15), "abcd")
})

test_that("num2vec works", {
  expect_equal(num2vec(7), c(1, 1, 1))
  expect_equal(num2vec(13, 5), c(1, 0, 1, 1, 0))
})

test_that("char2num works", {
  # Normal test
  expect_equal(char2num("abcd"), 15)
  # Test with wrong generator
  expect_equal(char2num("AaCCCcb"), 7)
})

test_that("Generators functions only take x > 1", {
  # No negative number input
  expect_error(num2char(-1), "x must be a positive integer")
  expect_error(num2vec(-1), "x must be a positive integer")
  # No empty string or non-string
  expect_error(char2num(""), "s must be a non-empty string")
  expect_error(char2num(15), "s must be a string")
})
