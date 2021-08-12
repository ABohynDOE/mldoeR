#' Creates interaction data about all possible interactions between k factors
#'
#' @param k number of basic factors
#' @description Interaction data includes: Number, Character equivalent, Length
interaction.data <- function(k){
  # Full interaction matrix
  full_int_mat = bMat(k)
  # Letters of the basic factors
  letters_vec = letters[1:k]
  # Interaction vector for chr and num
  int_chr = vector(length = 2^k-1)
  int_num = c(1:(2^k-1))
  for (i in int_num){
    col_vec = as.logical(t(full_int_mat[,i]))
    int_chr[i] = paste0(letters_vec[col_vec], collapse = "")
  }
  # Length vector
  int_len = as.vector(colSums(full_int_mat))
  # Join into a tibble
  int_tibble = tibble::tibble(
    num = int_num,
    gen = int_chr,
    len = int_len
  )
  return(int_tibble)
}

#' Create the search-table for $2^{n-p}$ designs of resolution r
#'
#' @param n number of two-level factors
#' @param p number of added factors
#' @param r minimal resolution of the designs
#' @return A 2^k-k-1 by p data.frame that contains the search-table
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
searchTable <- function(n,p,r){
  # Basic factors
  k = n-p
  # Interactions
  int.data <- interaction.data(k)
  # Generators
  gen.table <- int.data%>%
    dplyr::filter(.data$len >= (r-1))%>%
    dplyr::select(.data$gen)
  # Create the added factors
  for (i in 1:p+1){
    gen.table <- gen.table%>%
      dplyr::mutate(paste0(.data$gen,intToUtf8(96+k+i)))
    colnames(gen.table)[i] = intToUtf8(96+k+i)
  }
  return(gen.table)
}
