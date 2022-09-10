# PRL version of smoothed h estimation!
# h(y_0) = 0 for identifiabilty and beta[-1] = 1
h.est.prl.smoothed <- function(est_beta, Y, X, y_0=0) {
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
        print("-------")
        print(m_X[i])
        print(m_X[j])
        print(potential_val)
        print(pnorm(sqrt(n)*(m_X[i] - m_X[j] - potential_val)))
        print(log(pnorm(sqrt(n)*(m_X[i] - m_X[j] - potential_val))))
        print("----------")
        res <- res + (d_iy - d_jy_0)*log(pnorm(sqrt(n)*(m_X[i] - m_X[j] - potential_val)))
      }
    }
    
    return(-res + 0.001*potential_val**2)
  }
  
  dummy <- function(y) {
    optim_res <- optim(0, fn=S_beta, method = "BFGS", 
                       control = list(trace=T, REPORT=1),
                       y=y, est_beta=est_beta, Y=Y, X=X, y_0=y_0)
    return(optim_res$par)
  }
  
  est_h_Y <- sapply(Y, dummy)
  
  return(est_h_Y)
}
