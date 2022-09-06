simulate_ltm_gaussian <- function(n, m, beta) {
  exponent <- function(a, pow) (abs(a)^pow)*sign(a)
  
  noise <- rnorm(n)
  # noise = rexp(n)
  X <- matrix(rnorm(n*m), n, m)
  
  Z <- X %*% beta + noise
  Y <- exponent(Z, 1/3)
  
  res <- list("X"=X, "Y"=Y, "beta"=beta, "noise"=noise, "Z"=Z)
  return(res)
}

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
  
  # grad_prl <- function(beta, Y, X) {
  #   n <- nrow(X)
  #   grad <- 0
  #   m_X <- X %*% beta
  #   for(i in 1:(n-1)) {
  #     for(j in (i+1):n) {
  #       grad <- grad + dnorm((m_X[j] - m_X[i])/sqrt(2*n))*(X[j, ]- X[i, ])/(pnorm((m_X[j] - m_X[i])/sqrt(2*n))*sqrt(2*n))
  #     }
  #   }
  #   
  #   return(-grad)
  # }
  
  coefs <- rep(0, m)
  res <- optim(coefs, fn=prl, method = "BFGS", 
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
  
  dummy <- function(y) {
    optim_res <- optim(0, fn=S_beta, method = "BFGS", 
                       # control = list(trace=T, REPORT=1),
                       y=y, est_beta=est_beta, Y=Y, X=X, y_0=y_0)
    return(optim_res$par)
  }
  
  est_h_Y <- sapply(Y, dummy)
  
  return(est_h_Y)
}

library(rjson)
library(doParallel)
no_cores <- detectCores()
cl <- makeCluster(no_cores-1)
registerDoParallel(cl) 

n <- 10
num_datasets <- 7

run.ltm.est <- function(n, m=3, beta=c(10, 5, 1)) {
  data = simulate_ltm_gaussian(n, m, beta)
  res_beta_est <- beta.est.smoothed(data$Y, data$X)
  est_Z <- h.est.smoothed(res_beta_est$est_beta, data$Y, data$X)
  est_noise <- est_Z - data$X %*% res_beta_est$est_beta
  return(list("est_Z"=matrix(est_Z, n, 1), "Z"=data$Z, 
              "est_beta"=matrix(res_beta_est$est_beta, length(data$beta), 1), "beta"=matrix(data$beta, length(data$beta), 1),
              "est_noise"=matrix(est_noise, n, 1), "noise"=matrix(data$noise, n, 1)))
}

bd <- function(x, y) {
  list("est_Z"=cbind(x$est_Z, y$est_Z), "Z"=cbind(x$Z, y$Z),
       "est_beta"=cbind(x$est_beta, y$est_beta), "beta"=cbind(x$beta, y$beta),
       "est_noise"=cbind(x$est_noise, y$est_noise), "noise"=cbind(x$noise, y$noise)
       )
}

res <- foreach(i=1:num_datasets, .combine=bd) %dopar% {
  run.ltm.est(n)
}

json_data <- toJSON(res)
write(json_data, "results.json")

getwd()

res

dim(res$est_Z)




stopCluster(cl)


