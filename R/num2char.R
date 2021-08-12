#' @title
#' Convert a column number to a word representation
#'
#' @description
#' \code{num2char} represents a column number as its corresponding generator.
#'
#' @details
#' Column number are always positive, non-zero integers.
#' Basic factors are power of two, while added factors are decomposed in powers of two and
#' their corresponding generator is build from it.
#' A generator corresponds to the basic factors used to create the levels of the added factor.
#' All basic factors are represented by independent lowercase letters.
#'
#' @param x Number of the interaction
#'
#' @return A character of length larger or equal to one, corresponding to a generator.
#'     For any number n, the length of the character will always be ceil(log2(n)).
#' @examples
#' num2char(4)
#' num2char(13)
#' @export
num2char <- function(x) {
  # Checking value of x
  if (x < 1) {
    stop("x must be a positive integer")
  }
  b <- binaryLogic::as.binary(x, n = 0, littleEndian = TRUE)
  l <- letters[1:length(b)]
  return(paste(l[b], collapse = ""))
}
