#' Obtain determinant even when input is scalar 
#'
#' @param A there exists n in N such that length(A) = n double, n_by_n matrix or scalar
#'
#' @return Determinant if A is matrix, otherwise; itself   
#'
#' @export
#'
modified_det <- function (A){
  if (length(A) == 1) {
    A
  }else{
    det(A)
  }
}