source("src/utils.R")
source("src/general/beta_est_smoothed.R")
source("src/general/h_est_smoothed.R")
source("src/pnl/est_pnl_causal_order.R")

set.seed(12345)
name_noise <- "evd" # noise distribution Gumbel
name_h <- "cube" # function h cubic
n <- 30 # sample size 
d <- 4 # number of nodes in causal graph

data = simulate.mult.pnl.erdos.renyi(n, d, name_noise, name_h)

estimated_causal_order <- est.pnl.causal.order(data$X, beta.est.smoothed, h.est.smoothed, output = TRUE)

print("estimated causal order")
estimated_causal_order

print("true causal graph: A[i, j] = 1 implies that there is an edge from node i to node j")
data$A

