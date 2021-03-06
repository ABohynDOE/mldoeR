# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' @title Hamming distance between two vectors
#'
#' @description \code{hammingDist(a,b)} finds the hamming distance between two vectors \code{a} and \code{b}.
#'
#' @details The hamming distance is the number of elements that differs between
#'   two arrays.
#' @param a first vector
#' @param b second vector
#' @return Hamming distance between \code{a} and \code{b}
hammingDist <- function(a, b) {
    .Call(`_mldoeR_hammingDist`, a, b)
}

#' Distance distribution of a matrix
#'
#' @description
#' \code{distanceDist(x)} computes the distribution of the hamming distances between
#' all pairs of rows (even identical) in matrix \code{x}.
#'
#' @details
#' For a matrix \eqn{\mathbf{X}} of size \eqn{(n \times m)}, the distance distribution is
#' a row-vector \eqn{B} of length \eqn{n}, where \eqn{B_i} is the number of pairs of rows
#' with hamming distance \eqn{i}.
#' Since pairs of identical rows are considered, \eqn{B_0} will always be \eqn{n}.
#'
#' @param x input matrix
#' @return a vector of the distance distribution
distanceDist <- function(x) {
    .Call(`_mldoeR_distanceDist`, x)
}

