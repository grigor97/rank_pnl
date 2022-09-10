library(EnvStats)

bd <- function(x, y) {
  list("est_Z"=cbind(x$est_Z, y$est_Z), "Z"=cbind(x$Z, y$Z),
       "est_beta"=cbind(x$est_beta, y$est_beta), "beta"=cbind(x$beta, y$beta),
       "est_noise"=cbind(x$est_noise, y$est_noise), "noise"=cbind(x$noise, y$noise),
       "Y"=cbind(x$Y, y$Y)
  )
}

simulate_ltm <- function(n, m, beta, name_noise, name_h) {
  exponent <- function(a, pow) (abs(a)^pow)*sign(a)
  noise <- NA
  h_inv <- NA
  if(name_noise == "gaussian" & name_h == "cube") {
    noise <- rnorm(n)
    h_inv <- function(z) {
      exponent(z, 1/3)
    }
  } else if(name_noise == "gaussian" & name_h == "log") {
    noise <- rnorm(n)
    h_inv <- function(z) {
      exp(z) - 1
    }
  } else if(name_noise == "evd" & name_h == "cube") {
    noise <- rgevd(n)
    h_inv <- function(z) {
      exponent(z, 1/3)
    }
  } else if(name_noise == "evd" & name_h == "log") {
    noise <- rgevd(n)
    h_inv <- function(z) {
      exp(z) - 1
    }
  } 
  
  X <- matrix(rnorm(n*m), n, m)
  Z <- X %*% beta + noise
  Y <- h_inv(Z)
  res <- list("X"=X, "Y"=Y, "beta"=beta, "noise"=noise, "Z"=Z)
  
  return(res)
}
