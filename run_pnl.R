source("../codeANM/code/startups/startupLINGAM.R", chdir = TRUE)
source("../codeANM/code/startups/startupICML.R", chdir = TRUE)
pars <- list(regr.method = train_linear, regr.pars = list(), 
             indtest.method = indtestHsic, indtest.pars = list())


source("utils.R")

source("gaussian/beta_est_prl_gaussian.R")
source("gaussian/h_est_gaussian.R")

source("general/beta_est_smoothed.R")
source("general/h_est_smoothed.R")

source("general/beta_est_prl_smoothed.R")
source("general/h_est_prl_smoothed.R")

source("pnl/est_pnl_causal_order.R")

library(rjson)
library(doParallel)

# name_alg = "prlg"
# name_noise <- "gaussian"
# name_h <- "cube"
# n <- 100
# d <- 4
# num_datasets <- 7

args <- commandArgs(trailingOnly = TRUE)
print(args)
n <- strtoi(args[1])
d <- strtoi(args[2])
num_datasets <- strtoi(args[3])
name_noise <- args[4]
name_h <- args[5]
name_alg <- args[6]
# print(n)
# print(d)
# print(num_datasets)
# print(name_noise)
# print(name_h)
# print(name_alg)

beta.est.alg <- NA
h.est.alg <- NA
if(name_alg == "smoothed") {
  beta.est.alg <- beta.est.smoothed
  h.est.alg <- h.est.smoothed
} else if(name_alg == "prlg") {
  beta.est.alg <- beta.est.prl.gaussian
  h.est.alg <- h.est.rank.reg.gaussian
} else if(name_alg == "prl_smoothed") {
  beta.est.alg <- beta.est.prl.smoothed
  h.est.alg <- h.est.prl.smoothed
} else {
  print("no such model ...")
  return()
}

run_pnl <- function() {
  data = simulate.mult.pnl.erdos.renyi(n, d, name_noise, name_h)
  
  res <- est.pnl.causal.order(data$X, beta.est.alg, h.est.alg, output = FALSE)
  resRESIT <- ICML(data$X, model = train_linear, indtest = indtestHsic, output = FALSE)
  resLINGAM <- lingamWrap(data$X)
  
  # print(res)
  # print("---")
  # print(resRESIT)
  # print("---")
  # print(resLINGAM)
  # print("---")
  # print(data$A)
  
  wrong <- 0
  wrong_RESIT <- 0
  wrong_LINGAM <- 0
  for(i in 1:(d-1)) {
    for(j in (i+1):d) {
      if(data$A[res[j], res[i]] == 1) {
        wrong = wrong + 1
      }
      
      if(data$A[resRESIT[j], resRESIT[i]] == 1) {
        wrong_RESIT = wrong_RESIT + 1
      }
      
      if(data$A[resLINGAM[j], resLINGAM[i]] == 1) {
        wrong_LINGAM = wrong_LINGAM + 1
      }
    }
  }
  
  wrongs <- c(wrong, wrong_RESIT, wrong_LINGAM)
  wrongs
}

# run_pnl()

no_cores <- detectCores()
cl <- makeCluster(no_cores-1)
registerDoParallel(cl) 

res <- foreach(i=1:num_datasets, .combine="rbind", .packages = c("dHSIC", "EnvStats")) %dopar% {
  .GlobalEnv$cdf.z <- cdf.z
  
  run_pnl()
}

res

json_data <- toJSON(list("res"=res, "n"=n, "d"=d, "num_datasets"=num_datasets, 
                         "name_noise"=name_noise, "name_h"=name_h, "name_alg"=name_alg))
file_name <- paste("../res/pnl_results", name_noise, name_h, 
                   name_alg, n, d, num_datasets, ".json", sep = "_")
write(json_data, file_name)

stopCluster(cl)
