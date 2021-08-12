test_that("Generator char to num works", {
  expect_equal(num2char(15), "abcd")
})

test_that("Generator num to vector works", {
  expect_equal(num2vec(7), c(1,1,1))
  expect_equal(num2vec(13,5), c(1,0,1,1,0))
})

test_that("Generators function do not work with x < 1", {
  expect_error(num2char(-1), "x must be a positive integer")
  expect_error(num2vec(-1), "x must be a positive integer")
  expect_error(num2vec(-1, 4), "x must be a positive integer")
})
