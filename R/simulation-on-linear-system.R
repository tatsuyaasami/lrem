#' Simulate a linear system with given transition
#'
#' @param g function, Function of predetermined vector at t on un-predetermined vector at t
#' @param h function, Function of predetermined vector at t on predetermined vector at t+1
#' @param x0 x0 in R, Initial value of predetermined vector
#' @param t t in N, Length of simulation
#'
#' @return Path of all vectors
#'
#' @export
#'
simulate <- function (g, h, x0, t) {
 n1 <- length(x0)     # The number of predetermined variables
 n2 <- length(g(x0))  # The number of un-predetermined variables
pre <- 1:n1
npr <- (n1 + 1):(n1 + n2)

out <- matrix(0, t, n1 + n2) # Zero matrix for simulation output
out[1, pre] <- x0 # Substituting initial Condition into out[1, pre]
out[1, npr] <- g(x0) 
 for (i in 1:(t - 1)) {
  out[i + 1, pre] <- h(out[i, pre]) 
  out[i + 1, npr] <- g(out[i + 1, pre])
 }
out

}
