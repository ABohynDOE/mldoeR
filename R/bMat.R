#' Full interaction matrix
#'
#' \code{bMat} returns the matrix of all the interactions between k basic factors
#'
#' @param k An integer. Number of basic factors.
#' @return A k by \eqn{2^{k}-1} binary matrix of all interactions between basic factors.
#' @export
bMat <- function(k) {
  int_vec <- c(1:(2^k - 1))
  full_int_mat <- sapply(int_vec, function(x) {
    as.integer(intToBits(x))[1:k]
  })
  colnames(full_int_mat) <- int_vec
  rownames(full_int_mat) <- letters[1:k]
  return(full_int_mat)
}
