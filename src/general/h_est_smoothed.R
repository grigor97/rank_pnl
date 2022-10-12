# function h estimation in RankS method in the paper
h.est.smoothed <- function(est_beta, Y, X, y_0=0) {
  S_beta <- function(potential_val, y, est_beta, Y, X, y_0) {
    res <- 0
    m_X <- X %*% est_beta
    n <- nrow(X)
    for(i in 1:n) {
      for(j in 1:n) {
        if(i == j) {
          next
        }
        d_iy <- 1*(Y[i] >= y)
        d_jy_0 <- 1*(Y[j] >= y_0)
        res <- res + (d_iy - d_jy_0)*pnorm(sqrt(n)*(m_X[i] - m_X[j] - potential_val))
      }
    }

    return(-res + 0.001*potential_val**2)
  }
  
  grad_S_beta <- function(potential_val, y, est_beta, Y, X, y_0) {
    grad <- 0
    m_X <- X %*% est_beta
    n <- nrow(X)
    for(i in 1:n) {
      for(j in 1:n) {
        if(i == j) {
          next
        }
        d_iy <- 1*(Y[i] >= y)
        d_jy_0 <- 1*(Y[j] >= y_0)
        grad <- grad - (d_iy - d_jy_0)*sqrt(n)*dnorm(sqrt(n)*(m_X[i] - m_X[j] - potential_val))
      }
    }
    
    return(-grad + 0.002*potential_val)
  }
  
  dummy <- function(y) {
    optim_res <- optim(0, fn=S_beta, gr=grad_S_beta, method = "BFGS", 
                       # control = list(trace=T, REPORT=1),
                       y=y, est_beta=est_beta, Y=Y, X=X, y_0=y_0)
    return(optim_res$par)
  }
  
  est_h_Y <- sapply(Y, dummy)
  
  return(est_h_Y)
}
