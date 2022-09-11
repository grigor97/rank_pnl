source("utils.R") 
source("gaussian/beta_est_prl_gaussian.R")
source("gaussian/h_est_gaussian.R")

source("general/beta_est_smoothed.R")
source("general/h_est_smoothed.R")

source("general/beta_est_prl_smoothed.R")
source("general/h_est_prl_smoothed.R")

# data = simulate_ltm(10, 3, c(10, 5, 1), "evd", "cube")
# res_beta_est <- beta.est.smoothed(data$Y, data$X)
# res_beta_est
# est_Z <- h.est.smoothed(res_beta_est$est_beta, data$Y, data$X)
# est_Z

library(rjson)
library(doParallel)

no_cores <- detectCores()
cl <- makeCluster(no_cores-1)
registerDoParallel(cl) 


args <- commandArgs(trailingOnly = TRUE)
print(args)
n <- strtoi(args[1])
num_datasets <- strtoi(args[2])
name_noise <- args[3]
name_h <- args[4]
name_alg <- args[5]
print(n)
print(num_datasets)
print(name_noise)
print(name_h)
print(name_alg)
# n <- 100
# num_datasets <- 7
# name_noise <- "gaussian" # "gaussian", "evd"
# name_h <- "cube" # "cube", "log"
# name_alg <- "prl_smoothed" # "smoothed", "prlg", "prl_smoothed"

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
  } else if(name_alg == "prl_smoothed") {
    res_beta_est <- beta.est.prl.smoothed(data$Y, data$X)
    est_Z <- h.est.prl.smoothed(res_beta_est$est_beta, data$Y, data$X)
  } else {
    print("no such model ...")
    return()
  }
  
  est_noise <- est_Z - data$X %*% res_beta_est$est_beta
  
  return(list("est_Z"=matrix(est_Z, n, 1), "Z"=data$Z, 
              "est_beta"=matrix(res_beta_est$est_beta, length(data$beta), 1), "beta"=matrix(data$beta, length(data$beta), 1),
              "est_noise"=matrix(est_noise, n, 1), "noise"=matrix(data$noise, n, 1),
              "Y"=data$Y)
  )
}

res <- foreach(i=1:num_datasets, .combine=bd) %dopar% {
  .GlobalEnv$cdf.z <- cdf.z
  run.ltm.est(n)
}

res
json_data <- toJSON(res)
file_name <- paste("../res/results", name_noise, name_h, 
                   name_alg, n, num_datasets, gsub(" ", "", Sys.time(), fixed = TRUE), ".json", sep = "_")
write(json_data, file_name)

getwd()

stopCluster(cl)
