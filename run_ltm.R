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
name_noise <- "gaussian" # "gaussian", "evd"
name_h <- "cube" # "cube", "log"
name_alg <- "smoothed" # "smoothed", "prlg"

file_name <- paste("../res/results", name_noise, name_h, 
                   name_alg, n, num_datasets, ".json", sep = "_")


run.ltm.est <- function(n, m=3, beta=c(10, 5, 1)) {
  data = simulate_ltm(n, m, beta, name_noise, name_h)
  
  res_beta_est <- NA
  est_Z <- NA
  
  if(name_alg == "smoothed") {
    res_beta_est <- beta.est.smoothed(data$Y, data$X)
    est_Z <- h.est.smoothed(res_beta_est$est_beta, data$Y, data$X)
  } else if(name_alg == "prlg") {
    res_beta_est <- beta.est.prl.gaussian(data$Y, data$X)
    est_Z <- h.est.rank.reg.gaussian(res_beta_est$est_beta, data$Y, data$X)
  }
  
  est_noise <- est_Z - data$X %*% res_beta_est$est_beta
  
  return(list("est_Z"=matrix(est_Z, n, 1), "Z"=data$Z, 
              "est_beta"=matrix(res_beta_est$est_beta, length(data$beta), 1), "beta"=matrix(data$beta, length(data$beta), 1),
              "est_noise"=matrix(est_noise, n, 1), "noise"=matrix(data$noise, n, 1),
              "Y"=data$Y)
  )
}

res <- foreach(i=1:num_datasets, .combine=bd) %dopar% {
  run.ltm.est(n)
}

res
json_data <- toJSON(res)
write(json_data, file_name)

getwd()

stopCluster(cl)

