source("utils.R") 
source("gaussian/beta_est_prl_gaussian.R")
source("gaussian/h_est_gaussian.R")

source("general/beta_est_smoothed.R")
source("general/h_est_smoothed.R")

library(rjson)
library(doParallel)
no_cores <- detectCores()
cl <- makeCluster(no_cores-1)
registerDoParallel(cl) 

n <- 10
num_datasets <- 7

# data = simulate_ltm_gaussian(20, 3, c(10, 5, 1))
# # data$X
# # res = beta.est.prl.gaussian(data$Y, data$X, data$beta)
# # est_Z = h.est.rank.reg.gaussian(data$Y, data$X, res$est_beta)
# # est_Z - data$Z
# 
# res = beta.est.smoothed(data$Y, data$X, data$beta)
# res
# 
# est_h <- h.est.smoothed(res$est_beta, data$Y, data$X)
# 
# est_h - data$Z
# res$est_beta - data$beta
# est_h

# save_result <- function(est_betas, betas) {
#   res <- list("betas"=betas, "est_betas"=est_betas,
#               "num_datasets"=nrow(est_betas), "num_betas"=length(betas), 
#               "n"=n, "m"=m)
#   json_data <- toJSON(res)
#   
#   write(json_data, file_name)
#   
#   return(res)
# }

get.noise <- function(n, m=3, beta=c(10, 5, 1)) {
  data = simulate_ltm_gaussian(n, m, beta)
  res_beta_est <- beta.est.smoothed(data$Y, data$X, data$beta)
  est_Z <- h.est.smoothed(res_beta_est$est_beta, data$Y, data$X)
  return(est_Z)
}



res <- foreach(i=1:num_datasets, .combine="rbind", .packages = c("EnvStats")) %dopar% {
  get.noise()
}

res

stopCluster(cl)

# i <- 7
# data$Y[i]
# data$Z[i]
# est_h[i]
# S_beta(data$Z[i], data$Y[i], res$est_beta, data$Y, data$X, 0)
# S_beta(est_h[i], data$Y[i], res$est_beta, data$Y, data$X, 0)

