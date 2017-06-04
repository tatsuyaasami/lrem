#' Solve a linear rational expectation (LRE) system with autoregressive (AR) inputs 
#'
#' @param A dim(A)[1] = dim(A)[2], n_by_n matrix
#' @param E dim(E)[1] = dim(E)[2], n_by_n matrix
#' @param B dim(B)[1] = or (neq) dim(B)[2], n_by_m matrix
#' @param Phi dim(Phi)[1] = dim(Phi)[2], m_by_m matrix
#' @param x0 x0 in R, Initial value of predetermined vector including inital shock
#' @param u0 u0 in R, Initial value of AR inputs (vector or scalar)
#'
#' @return Functions of predetermined vector at t on un-predetermined vector at t and predetermined vector at t+1 
#'
#' @export
#'
lre_ar <- function (A, E, B, Phi, x0 = NULL, u0) {
  n <- dim(A)[1]
  m <- dim(Phi)[1]
  Onm <- matrix(0, nrow = n, ncol = m)
  Omn <- matrix(0, nrow = m, ncol = n)
  Imm <- diag(m)
  E1 <- rbind(Imm, Onm)
  E2 <- rbind(Omn, E)
  EE <- cbind(E1, E2)
  A1 <- rbind(Phi, B)
  A2 <- rbind(Omn, A)
  AA <- cbind(A1, A2)
  xu0 <- function (x0 = NULL, u0) {
    if (is.null(x0)) {
      matrix(u0, nrow = 1, byrow = TRUE)
    } else {
      matrix(cbind(u0, x0), nrow = 1, byrow = TRUE)
    }
  } 
  xu0 <- t(xu0(x0, u0))
   
    return(list(g = lre_auto(AA, EE, xu0)$g, h = lre_auto(AA, EE, xu0)$h)) # order is like (x, u)
  
}