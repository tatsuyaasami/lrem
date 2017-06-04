#' Schur decomposition with stable eigenvalues in the upper-left block)
#'
#' @param A dim(A[1]) = dim(A)[2], n_by_n matrix
#' @param E dim(A[1]) = dim(A)[2], n_by_n matrix
#'
#' @return Schur decomposition of this matrix with stable eigenvalues in the upper-left block
#' @export
#'

proper_order_QZ <- function(A, E) {
  AE_QZ1 <- QZ::qz(A, E)
  ord <- abs(AE_QZ1$ALPHA / AE_QZ1$BETA) <= 1
  AE_QZ2 <- QZ::qz.dtgsen(AE_QZ1$S, AE_QZ1$T, AE_QZ1$Q, AE_QZ1$Z, select = ord)
  AE_QZ3 <- list(ALPHA = AE_QZ2$BETA, BETA = AE_QZ2$ALPHA, S = AE_QZ2$T, T = AE_QZ2$S, Q = AE_QZ2$Q, Z = AE_QZ2$Z)
  return(AE_QZ3)
}