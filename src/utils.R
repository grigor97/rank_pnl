set.seed(12)

simulate_ltm_gaussian <- function(n, m, beta) {
  exponent <- function(a, pow) (abs(a)^pow)*sign(a)
  
  # noise <- rnorm(n)
  noise = rexp(n)
  X <- matrix(rnorm(n*m), n, m)
  
  Z <- X %*% beta + noise
  Y <- exponent(Z, 1/3)
  
  res <- list("X"=X, "Y"=Y, "beta"=beta, "noise"=noise, "Z"=Z)
  return(res)
}

