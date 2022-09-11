# from https://www.sciencedirect.com/science/article/abs/pii/S0167947312002861
beta.est.smoothed <- function(Y, X, gt_beta=NA) {
  m <- ncol(X)
  n <- nrow(X)
  ord <- order(Y)
  Y <- Y[ord]
  X <- X - matrix(rep(colMeans(X), n), n, m, byrow = T)
  if(m > 1) {
    X <- X[ord, ]
  } else {
    X <- matrix(X[ord], n, 1)
  }
  
  prl <- function(beta, Y, X) {
    n <- nrow(X)
    lik <- 0
    m_X <- X %*% beta
    for(i in 1:(n-1)) {
      for(j in (i+1):n) {
        # note here we take h = 1/sqrt(n)
        lik <- lik + pnorm(sqrt(n)*(m_X[j] - m_X[i]))
      }
    }
    
    return(-lik)
  }
  
  grad_prl <- function(beta, Y, X) {
    n <- nrow(X)
    grad <- 0
    m_X <- X %*% beta
    for(i in 1:(n-1)) {
      for(j in (i+1):n) {
        # note here we take h = 1/sqrt(n)
        grad <- grad + sqrt(n)*dnorm(sqrt(n)*(m_X[j] - m_X[i]))*(X[j, ] - X[i, ])
      }
    }
    
    return(-grad)
  }
  
  coefs <- rep(0, m)
  res <- optim(coefs, fn=prl, gr=grad_prl, method = "BFGS", 
               control = list(trace=T, REPORT=1),
               Y=Y, X=X)
  
  prl_lik <- res$value
  gt_lik <- NA
  if(!is.na(gt_beta)) {
    gt_lik <- prl(gt_beta, Y, X)
  }
  
  est_beta <- res$par/res$par[length(res$par)]
  return(list("est_beta"=est_beta, "est_obj"=prl_lik, 
              "gt_beta"=gt_beta, "gt_obj"=gt_lik))
}
