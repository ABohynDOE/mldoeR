test_that("B matrix dimension", {
  expect_equal(dim(bMat(4)), c(4, 15))
})
test_that("B matrix only works for k > 2", {
  expect_error(bMat(1))
})
