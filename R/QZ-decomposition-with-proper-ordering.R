#' Schur decomposition with stable eigenvalues in the upper-left block)
#'
#' @param A dim(A[1]) = dim(A)[2], n_by_n matrix
#'
#' @return Schur decomposition of this matrix with stable eigenvalues in the upper-left block
#' @export
#'

proper_order_QZ <- function(A, E) {
  AE_QZ <- QZ::qz(A, E)
  ord <- abs (AE_QZ$ALPHA / AE_QZ$BETA) <= 1
  AE_QZ <- QZ::qz.dtgsen(AE_QZ$S, AE_QZ$T, AE_QZ$Q, AE_QZ$Z, select = ord)
  return(AE_QZ)
}