#include <Rcpp.h>
using namespace Rcpp;

//' @title Hamming distance between two vectors
//'
//' @description \code{hammingDist(a,b)} finds the hamming distance between two vectors \code{a} and \code{b}.
//'
//' @details The hamming distance is the number of elements that differs between
//'   two arrays.
//' @param a first vector
//' @param b second vector
//' @return Hamming distance between \code{a} and \code{b}
// [[Rcpp::export]]
int hammingDist(NumericVector a, NumericVector b){
  // Check that a and b have same size
  if (a.size() != b.size()){
    throw "Both vectors must have the same size";
  }
  int dist =0;
  for (int i = 0; i < a.size(); i++){
    dist += (a[i] != b[i]);
  }
  return dist;
}



//' Distance distribution of a matrix
//'
//' @description
//' \code{distanceDist(x)} computes the distribution of the hamming distances between
//' all pairs of rows (even identical) in matrix \code{x}.
//'
//' @details
//' For a matrix \eqn{\mathbf{X}} of size \eqn{(n \times m)}, the distance distribution is
//' a row-vector \eqn{B} of length \eqn{n}, where \eqn{B_i} is the number of pairs of rows
//' with hamming distance \eqn{i}.
//' Since pairs of identical rows are considered, \eqn{B_0} will always be \eqn{n}.
//'
//' @param x input matrix
//' @return A vector of the distance distribution
// [[Rcpp::export]]
NumericVector distanceDist(NumericMatrix x) {
  int nrow = x.nrow();
  int ncol = x.ncol();
  NumericVector distVec(ncol+1);
  for (int i = 0; i < nrow; i++){
    for (int j = i; j < nrow; j++){
      double h = hammingDist(x(i,_),x(j,_));
      distVec[h]++;
    }
  }
  return distVec;
}


/*** R
M = magic::magic(8)
M = rbind(M,M,M,M)
distanceDist(M)
*/
