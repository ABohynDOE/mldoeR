test_that("Search-table has correct size and type", {
  # Define k, p and R
  k = 4
  p = 2
  R = 3
  st <- searchtable(k,p,R)
  # Test type of the search-table
  expect_type(st, "list")
  # Test the size
  ncols = p+1
  nrows = 2^k-1-k
  expect_equal(dim(st), c(nrows,ncols))
})
