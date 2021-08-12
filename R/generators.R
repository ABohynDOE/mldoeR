#' @title
#' Convert a column number to a word representation
#'
#' @description
#' `num2char` represents a column number as its corresponding generator.
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
num2vec <- function(x, k) {
  # Checking value of x
  if (x < 1) {
    stop("x must be a positive integer")
  }
  # Default k value if missing
  if (missing(k)) {
    n <- ceiling(log2(max(x)))
  } else {
    n <- k
  }
  b <- binaryLogic::as.binary(x, n = n, littleEndian = TRUE)
  return(sapply(b, function(x) as.numeric(as.logical(x))))
}


#' Number representation of a generator, written as a string
#'
#' @description
#' `char2num` takes a non-empty string, representing a generator, and converts it
#' to the equivalent generator number. In the string, each letter represents a basic factor.
#'
#' @details
#' All letters are converted to lower case to avoid confusion with upper case letters,
#' representing four-level factors.
#' The order of the letters does not matter.
#' Any letter is only counted once in the word.
#' Thus, `char2num("abcd")` and `char2num("abbcddd")` will output the same result.
#'
#' @param s a non-empty string representing a generator
#'
#' @return the corresponding generator number
#' @export
#'
#' @examples
#' char2num("abd")
#' char2num("Addb")
char2num <- function(s) {
  # Check that s is a string
  if (!is.character(s)) {
    stop("s must be a string")
  }
  # Check that s is non-empty
  else if (nchar(s) == 0) {
    stop("s must be a non-empty string")
  }
  # Convert to lower case
  s <- tolower(s)
  # Convert to ASCII number and remove 61 (ASCII value of a)
  num_lst <- unique(utf8ToInt(s))
  # Retrieve 97, apply as power to 2 and sum
  n <- sum(sapply(num_lst, function(x) 2^(x - 97)))
  return(n)
}
