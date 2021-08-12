#' mSearchTable
#'
#' Create a search-table for mixed-level designs
#'
#' @param m Number of four-level factors
#' @param k Number of basic factors
#' @param p Number of added factors
#' @param r Resolution
#'
#' @return A tibble containing the search-table.
#' @export
#'
#' @examples
#' # Search-table for 4^1 2^6 designs in 32 runs
#' mSearchTable(1,5,3,3)
mSearchTable <- function(m,k,p,r){
  # Compute all generators
  gen <- c(1:(2^k-1))
  # Compute char equivalent
  generators <- sapply(gen,num2char)
  # Compute their matrix form
  gen_mat <- matrix(nrow = 2^k-1, ncol = k)
  for (i in gen) {
    gen_mat[i,] = binaryLogic::as.binary(i,littleEndian = TRUE, n = k)
  }
  # Original order
  order <- rowSums(gen_mat)
  # Modify according to four-level factors
  mod_order <- rowSums(gen_mat)
  type <- rep(0,(2^k-1))
  for (i in 1:m) {
      pf_order <- rowSums(gen_mat[,(2*i-1):(2*i)])
      mod_order <- mod_order - pf_order + as.logical(pf_order)
      type <- type + as.logical(pf_order)
  }
  # Sort by order and type
  int_df <- tibble::tibble(gen, generators, mod_order, type)
  st <- int_df[order(int_df$mod_order, int_df$type),]
  st <- dplyr::filter(st, mod_order > r-2)
  # Create the added factors
  for (i in 1:(p+1)) {
    st <- dplyr::mutate(st, paste0(generators,intToUtf8(96+k+i)))
    colnames(st)[ncol(st)] = intToUtf8(96+k+i)
  }
  # Return simply the ST
  return(dplyr::select(st,-mod_order, -type, -gen))
}
