source("utils.R") 
source("gaussian/beta_est_prl_gaussian.R")
source("gaussian/h_est_gaussian.R")

source("general/beta_est_smoothed.R")
source("general/h_est_smoothed.R")

data = simulate_ltm_gaussian(200, 3, c(10, 5, 1))
# data$X
# res = beta.est.prl.gaussian(data$Y, data$X, data$beta)
# est_Z = h.est.rank.reg.gaussian(data$Y, data$X, res$est_beta)

res = beta.est.smoothed(data$Y, data$X, data$beta)
res

est_h <- h.est.smoothed(res$est_beta, data$Y, data$X)

est_h - data$Z

est_h

S_beta <- function(potential_val, y, est_beta, Y, X, y_0) {
  res <- 0
  m_X <- X %*% est_beta
  n <- nrow(X)
  for(i in 1:n) {
    for(j in 1:n) {
      if(i == j) {
        next
      }
      d_iy <- 1*(Y[i] <= y)
      d_jy_0 <- 1*(Y[j] <= y_0)
      res <- res + (d_iy - d_jy_0)*pnorm(sqrt(n)*(m_X[i] - m_X[j] - potential_val))
    }
  }
  
  return(res + 0.001*potential_val**2)
}

i <- 7
data$Y[i]
data$Z[i]
est_h[i]
S_beta(data$Z[i], data$Y[i], res$est_beta, data$Y, data$X, 0)
S_beta(est_h[i], data$Y[i], res$est_beta, data$Y, data$X, 0)

optim_res <- optim(0, fn=S_beta, method = "BFGS", 
                   # control = list(trace=T, REPORT=1),
                   y=data$Y[i], est_beta=res$est_beta, Y=data$Y, X=data$X, y_0=0)
optim_res$par
data$Z[i]

S_beta(data$Z[i], data$Y[i], res$est_beta, data$Y, data$X, 0)
S_beta(optim_res$par, data$Y[i], res$est_beta, data$Y, data$X, 0)
