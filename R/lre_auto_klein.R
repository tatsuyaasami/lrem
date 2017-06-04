#' Solve a linear autonomous system with Klein method
#'
#' @param A dim(A[1]) = dim(A)[2], n_by_n matrix
#' @param E det(A) = 0 is OK, n_by_n matrix
#' @param x0 x0 in R, Initial value of predetermined vector
#'
#' @return Functions of predetermined vector at t on un-predetermined vector at t and predetermined vector at t+1
#'
#' @export
#'
lre_auto_klein <- function (A, E, x0) {
  n <- dim(A)[2]      # Matrix A is n %*% n.
 n1 <- length(x0)     # The number of predetermined variables
 n2 <- n - length(x0) # The number of un-predetermined variables
pre <- 1:n1
npr <- (n1 + 1):(n1 + n2)

# The number of stable eigenvalues
 eigen_AE <- proper_order_QZ(A,E)$BETA / proper_order_QZ(A,E)$ALPHA
 ns <- length(eigen_AE[abs(eigen_AE)<= 1])
# The number of unstable eigenvalues
 nu <- n - ns

stb <- 1 : ns
ust <- (ns + 1) : (ns + nu)
 
Sss <- proper_order_QZ(A, E)$S[stb, stb]
Tss <- proper_order_QZ(A, E)$T[stb, stb]
Z1s <- proper_order_QZ(A, E)$Z[pre, stb]
Z2s <- proper_order_QZ(A, E)$Z[npr, stb]

if (n1 > ns){
  cat("multiple solutions (n1 > ns)")
}
if (n1 < ns){
  cat("no solution  (n1 < ns)")
}else{
  if (all.equal(modified_det(Z1s),0) == TRUE) {
    cat("multiple solutions (square but singular)")
  }else{ 
    # Function on x^2_t of x^1_t 
    g <- function (x1){
     Z2s %*%  solve(Z1s) %*% x1
    }
    
    # Function on x^1_{t+1} of x^1_t
    h <- function (x1){
     Z1s %*% solve(Sss) %*% Tss %*% solve(Z1s) %*% x1
    }
  return(list(g = g, h = h))
  }
 }
}