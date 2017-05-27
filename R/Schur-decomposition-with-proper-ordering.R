#' Schur decomposition with stable eigenvalues in the upper-left block)
#'
#' @param A dim(A[1]) = dim(A)[2], n_by_n matrix
#'
#' @return Schur decomposition of this matrix with stable eigenvalues in the upper-left block
#' @export
#'

proper_order_sch <- function(A) {
  Asch <- Matrix::Schur(A) 
  Asch <- QZ::qz.dtrsen(Asch$T, Asch$Q, abs(Asch$EValues) <= 1)
  return(Asch)
}