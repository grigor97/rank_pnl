source("src/utils.R")
source("src/gaussian/beta_est_prl_gaussian.R")
source("src/gaussian/h_est_gaussian.R")
source("src/pnl/est_pnl_causal_order.R")

set.seed(42)
name_alg = "prlg" # same as RankG in the paper
name_noise <- "gaussian" # noise distribution Gaussian
name_h <- "cube" # function h cubic
n <- 100 # sample size 
d <- 4 # number of nodes in causal graph

data = simulate.mult.pnl.erdos.renyi(n, d, name_noise, name_h)

estimated_causal_order <- est.pnl.causal.order(data$X, beta.est.prl.gaussian, h.est.rank.reg.gaussian, output = TRUE)

print("estimated causal order")
estimated_causal_order

print("true causal graph: A[i, j] = 1 implies that there is an edge from node i to node j")
data$A
