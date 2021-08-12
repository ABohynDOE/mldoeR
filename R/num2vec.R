#' Vector representation of a column number
#'
#' @description
#' `num2vec` represents the numbers presented as arguments as column vectors from
#' the design matrix.
#'
#' @param x column number to represent.
#' @param k (Optional) number of basic factors to consider. Default is ceil(log2(x)).
#'
#' @details If a numeric vector is supplied to the function, then a matrix is returned.
#' The cplumn size of the matrix corresponds to the size of the vector.
#' @export
num2vec <- function(x, k){
  # Checking value of x
  if (x < 1) {
    stop("x must be a positive integer")
  }
  # Default k value if missing
  if(missing(k)){
    n = ceiling(log2(max(x)))
  } else {
    n = k
  }
  b <- binaryLogic::as.binary(x, n = n, littleEndian = TRUE)
  return(sapply(b,function(x) as.numeric(as.logical(x))))
}


