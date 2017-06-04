#' Simulate a linear system with given transition
#'
#' @param g function, Function of predetermined vector at t on un-predetermined vector at t
#' @param h function, Function of predetermined vector at t on predetermined vector at t+1
#' @param x0 x0 in R, Initial value of predetermined vector
#' @param u0 vector or matrix, each row e[k, ] corresponds to shock at t = k
#' @param t t in N, Length of simulation
#'
#' @return Path of all vectors
#'
#' @export
#' 

simulate <- function(g, h, x0 = NULL, u0 = NULL, t) {
  
simulate_without_shock <- function (g, h, x0, t) {
 n1 <- length(x0)     # The number of predetermined variables
 n2 <- length(g(x0))  # The number of un-predetermined variables
pre <- 1:n1
npr <- (n1 + 1):(n1 + n2)
 x0 <- t(x0)
out <- matrix(0, t, n1 + n2) # Zero matrix for simulation output
out[1, pre] <- x0 # Substituting initial Condition into out[1, pre]
out[1, npr] <- g(x0) 
i0 <- x0 

for (i in 1:(t - 1)) {
  i1 <- h(i0)
  out[i + 1, pre] <- i1 
  out[i + 1, npr] <- g(i1)
  i0 <- i1
 }
out

}

simulate_ar <- function (g, h, x0 = NULL, u0, t) {
  xu0 <- function (x0 = NULL, u0) {
    if (is.null(x0)) {
      matrix(u0, nrow = 1, byrow = TRUE)
    } else {
      matrix(cbind(u0, x0), nrow = 1, byrow = TRUE)
    }
  } 
  xu0 <- t(xu0(x0, u0))
  
  n1 <- length(xu0)    # The number of predetermined variables
  n2 <- length(g(xu0))     # The number of un-predetermined variables
 pre <- 1:n1
 npr <- (n1 + 1):(n1 + n2)
  
  out <- matrix(0, t, n1 + n2) # Zero matrix for simulation output
  out[1, pre] <- xu0 # Substituting initial Condition into out[1, pre]
  out[1, npr] <- g(xu0)
  i0 <- xu0 
  for (i in 1:(t - 1)) {
    i1 <- h(i0)
    out[i + 1, pre] <- i1 
    out[i + 1, npr] <- g(i1)
    i0 <- i1
  }
  out
  
}

  if (is.null(u0)) {
    return(simulate_without_shock(g, h, x0, t))
  } else {
    return(simulate_ar(g, h, x0, u0, t)) 
  }

} 



