#' Search-table for mixed-level designs
#'
#' Create a search-table for regular four-and-two-level designs.
#'
#' @details
#' Pseudo-factors are always composed of a pair of basic factors and their interaction.
#'
#' @param m Number of four-level factors
#' @param k Number of basic factors
#' @param p Number of added factors
#' @param R Resolution
#'
#' @return A tibble containing the search-table.
#' @export
#'
#' @examples
#' # Search-table for 4^1 2^6 designs in 32 runs
#' mixed_searchtable(1, 5, 3, 3)
mixed_searchtable <- function(m, k, p, R) {
  # Compute all generators
  gen <- c(1:(2^k - 1))
  # Compute char equivalent
  generators <- sapply(gen, num2char)
  # Compute their matrix form
  gen_mat <- matrix(nrow = 2^k - 1, ncol = k)
  for (i in gen) {
    gen_mat[i, ] <- binaryLogic::as.binary(i, littleEndian = TRUE, n = k)
  }
  # Original order
  order <- rowSums(gen_mat)
  # Modify according to four-level factors
  mod_order <- rowSums(gen_mat)
  type <- rep(0, (2^k - 1))
  for (i in 1:m) {
    pf_order <- rowSums(gen_mat[, (2 * i - 1):(2 * i)])
    mod_order <- mod_order - pf_order + as.logical(pf_order)
    type <- type + as.logical(pf_order)
  }
  # Sort by order and type
  int_df <- tibble::tibble(gen, generators, mod_order, type)
  st <- int_df[order(int_df$mod_order, int_df$type), ]
  st <- dplyr::filter(st, mod_order > R - 2)
  # Create the added factors
  for (i in 1:(p + 1)) {
    st <- dplyr::mutate(st, paste0(generators, intToUtf8(96 + k + i)))
    colnames(st)[ncol(st)] <- intToUtf8(96 + k + i)
  }
  # Return simply the ST
  return(dplyr::select(st, -mod_order, -type, -gen))
}

#' Search-table for two-level designs
#'
#' @param k Number of basic factors.
#' @param p Number of added factors
#' @param R Resolution
#'
#' @return A tibble containing the search-table.
#' @export
searchtable <- function(k, p, R) {
  # Total number of factors
  n <- k + p
  # Full interaction matrix
  full_int_mat <- bMat(k)
  # Letters of the basic factors
  letters_vec <- letters[1:k]
  # Interaction vector for chr and num
  int_chr <- vector(length = 2^k - 1)
  int_num <- c(1:(2^k - 1))
  for (i in int_num) {
    col_vec <- as.logical(t(full_int_mat[, i]))
    int_chr[i] <- paste0(letters_vec[col_vec], collapse = "")
  }
  # Length vector
  int_len <- as.vector(colSums(full_int_mat))
  # Join into a tibble
  int.data <- tibble::tibble(
    num = int_num,
    gen = int_chr,
    len = int_len
  )
  # Generators
  gen.table <- int.data %>%
    dplyr::filter(.data$len >= (R - 1)) %>%
    dplyr::select(.data$gen)
  # Create the added factors
  for (i in 1:p + 1) {
    gen.table <- gen.table %>%
      dplyr::mutate(paste0(.data$gen, intToUtf8(96 + k + i)))
    colnames(gen.table)[i] <- intToUtf8(96 + k + i)
  }
  return(gen.table)
}
