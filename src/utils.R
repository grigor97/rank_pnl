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

gen.directed.erdos.renyi.graph <- function(d) {
  if(d == 2) {
    A <- matrix(0, d, d)
    A[1, 2] <- 1
    return(A)
  }
  
  edge_prb <- 2/(d-1)
  A <- matrix(rbinom(d^2, 1, edge_prb), d, d)
  A <- A*upper.tri(A)
  return(A)
}

simulate.mult.pnl.erdos.renyi <- function(n, d, name_noise, name_h) {
  exponent <- function(a, pow) (abs(a)^pow)*sign(a)
  noise_f <- NA
  h_inv <- NA
  if(name_noise == "gaussian" & name_h == "cube") {
    noise_f <- function(n) {
      rnorm(n)
    }
    h_inv <- function(z) {
      exponent(z, 1/3)
    }
  } else if(name_noise == "gaussian" & name_h == "log") {
    noise_f <- function(n) {
      rnorm(n)
    }
    h_inv <- function(z) {
      exp(z) - 1
    }
  } else if(name_noise == "evd" & name_h == "cube") {
    noise_f <- function(n) {
      rgevd(n)
    }
    h_inv <- function(z) {
      exponent(z, 1/3)
    }
  } else if(name_noise == "evd" & name_h == "log") {
    noise_f <- function(n) {
      rgevd(n)
    }
    h_inv <- function(z) {
      exp(z) - 1
    }
  } 
  
  g <- function(x) {
    beta <- runif(2*dim(x)[2], -10, 10)
    val <- cbind(x, x^2) %*% beta
    return(val)
  }
  
  A <- gen.directed.erdos.renyi.graph(d)
  
  X <- matrix(0, n, d)
  for(j in 1:d) {
    parents <- which(A[, j] != 0, arr.ind=T)
    if(length(parents) == 0) {
      X[, j] <- noise_f(n)
    } else {
      noise <- noise_f(n)
      z <- g(cbind(X[, parents])) + noise
      y <- h_inv(z)
      X[, j] <- y
    }
  }
  
  return(list("A"=A, "X"=X, "noise"=name_noise, "h"=name_h))
}
