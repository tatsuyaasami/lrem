#' Obtain determinant even when input is scalar 
#'
#' @param A dim(A[1]) = dim(A)[2], n_by_n matrix
#' @param E det(A) = 0 is OK, n_by_n matrix
#' @param x0 x0 in R, Initial value of predetermined vector
#'
#' @return Functions of predetermined vector at t on un-predetermined vector at t and predetermined vector at t+1
#'
#' @export
#'
lre_auto_bk <- function(A, x0) {
  # The contents of hw07's lre_auto
}

lre_auto_klein <- function(A, E, x0) {
  # Solution here using QZ decomposition!
}

lre_auto <- function(A, E = NULL, x0) {
  if (is.null(E)) {
    lre_auto_bk(A, x0)
  } else {
    lre_auto_klein(A, E, x0)
  }
}