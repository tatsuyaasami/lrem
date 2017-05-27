#' Solve a linear autonomous system with Blanchard and Kahn method
#'
#' @param A dim(A[1]) = dim(A)[2], n_by_n matrix
#' @param x0 x0 in R, Initial value of predetermined vector
#'
#' @return Functions of predetermined vector at t on un-predetermined vector at t and predetermined vector at t+1
#'
#' @export
#'
lre_auto_bk <- function (A, x0) {
  n <- dim(A)[2]      # Matrix A is n %*% n.
 n1 <- length(x0)     # The number of predetermined variables
 n2 <- n - length(x0) # The number of un-predetermined variables
pre <- 1:n1
npr <- (n1 + 1):(n1 + n2)

# The number of stable eigenvalues
 ns <- length(proper_order_sch(A)$EValues[abs(proper_order_sch(A)$EValues) <= 1])
# The number of unstable eigenvalues
 nu <- n - ns

stb <- 1 : ns
ust <- (ns + 1) : (ns + nu)
 
Q1s <- proper_order_sch(A)$Q[pre, stb]
Q2s <- proper_order_sch(A)$Q[npr, stb]

if (n1 > ns){
  cat("multiple solutions (n1 > ns)")
}
if (n1 < ns){
  cat("no solution  (n1 < ns)")
} else{
  if (all.equal(modified_det(Q1s),0) == TRUE) {
    cat("multiple solutions (square but singular)")
  }else{ 
    # Function on x^2_t of x^1_t 
    g <- function (x1){
     Q2s %*%  solve(Q1s) %*% x1 
    }
    
    # Function on x^1_{t+1} of x^1_t
    h <- function (x1){
     A[pre, pre] %*% x1 + A[pre, npr] %*% g(x1)
    }
  return <- list(g, h)
  }
 }
}