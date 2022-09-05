source("utils.R") 
source("gaussian/beta_est_prl_gaussian.R")
source("gaussian/h_est_gaussian.R")

source("general/beta_est_smoothed.R")
source("general/h_est_smoothed.R")

data = simulate_ltm_gaussian(100, 3, c(10, 5, 1))
# data$X
# res = beta.est.prl.gaussian(data$Y, data$X, data$beta)
# est_Z = h.est.rank.reg.gaussian(data$Y, data$X, res$est_beta)

res = beta.est.smoothed(data$Y, data$X, data$beta)
res

est_h <- h.est.smoothed(res$est_beta, data$Y, data$X)

est_h - data$Z

est_h
