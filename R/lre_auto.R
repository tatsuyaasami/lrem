#' Solve a linear autonomous system with Klein method
#'
#' @param A dim(A[1]) = dim(A)[2], n_by_n matrix
#' @param E dim(E[1]) = dim(E)[2], n_by_n matrix
#' @param x0 x0 in R, Initial value of predetermined vector
#'
#' @return Functions of predetermined vector at t on un-predetermined vector at t and predetermined vector at t+1
#'
#' @export 
#'
lre_auto <- function(A, E = NULL, x0) {
  if (is.null(E)) {
    lre_auto_bk(A, x0)
    # The contents of hw07's lre_auto
  } else {
    lre_auto_klein(A, E, x0)
    # Solution here using QZ decomposition!
  }
}