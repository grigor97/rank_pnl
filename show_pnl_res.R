library(rjson)
library(matrixStats)

library(stringr) 

load_prlg_res <- function(dir_path, file_name) {
  f_path = paste(dir_path, file_name, sep="")
  pnl_res <- fromJSON(file=f_path)
  
  wrongs <- matrix(pnl_res$res, pnl_res$num_datasets, 3)
  wrongs
}

load_smoothed_res <- function(dir_path, file_name) {
  f_path = paste(dir_path, file_name, sep="")
  pnl_res <- fromJSON(file=f_path)
  
  wrongs <- matrix(pnl_res$res[1:pnl_res$num_datasets], pnl_res$num_datasets, 1)
  wrongs
}

load_abpnl_res <- function(dir_path, file_name) {
  f_path = paste(dir_path, file_name, sep="")
  data <- read.csv(f_path)
  unname(data.matrix(data))
}

load_all <- function(dir_path, prlg_p, smth_p, abpnl_p) {
  prlg_rl <- load_prlg_res(dir_path, prlg_p)
  smth <- load_smoothed_res(dir_path, smth_p)
  abpnl <- load_abpnl_res(dir_path, abpnl_p)
  
  res <- cbind(prlg_rl, smth, abpnl)
  colnames(res) <- c("prlg", "resit", "lingam", "smoothed", "abpnl")
  
  res
}

load_4 <- function(dir_path, prlg_p, abpnl_p) {
  prlg_rl <- load_prlg_res(dir_path, prlg_p)
  abpnl <- load_abpnl_res(dir_path, abpnl_p)
  
  res <- cbind(prlg_rl, abpnl)
  colnames(res) <- c("prlg", "resit", "lingam", "abpnl")
  
  res
}

get_true_name <- function(result_generic_name, noise, method, d, n) {
  result_name <- str_replace(result_generic_name, "nnn", as.character(n))
  result_name <- str_replace(result_name, "ddd", as.character(d))
  result_name <- str_replace(result_name, "method", method)
  result_name <- str_replace(result_name, "noise", noise)
  result_name
}

get_smoothed_all_results <- function(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name) {
  res_means <- matrix(nrow=0, ncol=length(col_names))
  res_stds <- matrix(nrow=0, ncol=length(col_names))
  for(n in ns) {
    smoothed_name <- get_true_name(result_generic_name, noise, "smoothed", d, n)
    prlg_name <- get_true_name(result_generic_name, noise, "prlg", d, n)
    abpnl_name <- get_true_name(abpnl_generic_name, noise, "", d, n)
    
    smoothed_res <- load_smoothed_res(dir_path, smoothed_name)
    prlg_res <- load_prlg_res(dir_path, prlg_name)
    abpnl_res <- load_abpnl_res(dir_path, abpnl_name)
    
    res <- c(smoothed_res, prlg_res, abpnl_res)
    
    col_means <- c(colMeans(smoothed_res), colMeans(prlg_res)[1], colMeans(abpnl_res), colMeans(prlg_res)[2])
    col_stds <- c(colSds(smoothed_res), colSds(prlg_res)[1], colSds(abpnl_res), colSds(prlg_res)[2])
    
    res_means <- rbind(res_means, col_means)
    res_stds <- rbind(res_stds, col_stds)
  }
  
  rownames(res_means) <- ns
  rownames(res_stds) <- ns
  
  
  colnames(res_means) <- col_names
  colnames(res_stds) <- col_names
  
  # res_means
  # res_stds
  list("means"=res_means, "stds"=res_stds)
}


get_prlg_all_results <- function(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name) {
  res_means <- matrix(nrow=0, ncol=length(col_names))
  res_stds <- matrix(nrow=0, ncol=length(col_names))
  for(n in ns) {
    prlg_name <- get_true_name(result_generic_name, noise, "prlg", d, n)
    abpnl_name <- get_true_name(abpnl_generic_name, noise, "", d, n)
    
    prlg_res <- load_prlg_res(dir_path, prlg_name)
    abpnl_res <- load_abpnl_res(dir_path, abpnl_name)
    
    res <- c(prlg_res, abpnl_res)
    
    col_means <- c(colMeans(prlg_res)[1], colMeans(abpnl_res), colMeans(prlg_res)[2])
    col_stds <- c(colSds(prlg_res)[1], colSds(abpnl_res), colSds(prlg_res)[2])
    
    res_means <- rbind(res_means, col_means)
    res_stds <- rbind(res_stds, col_stds)
  }
  
  rownames(res_means) <- ns
  rownames(res_stds) <- ns
  
  
  colnames(res_means) <- col_names
  colnames(res_stds) <- col_names
  
  # res_means
  # res_stds
  list("means"=res_means, "stds"=res_stds)
}


print_latex_format <- function(ns, res_gaussian, res_evd) {
  for(i in 1:nrow(res_gaussian$means)) {
    rr <- as.character(ns[i])
    for(j in 1:ncol(res_gaussian$means)) {
      rr <- paste(rr, "&", res_gaussian$means[i, j], "$\\pm$", round(res_gaussian$stds[i, j], 2))
    }
    for(j in 1:ncol(res_evd$means)) {
      rr <- paste(rr, "&", res_evd$means[i, j], "$\\pm$", round(res_evd$stds[i, j], 2))
    }
    rr <- paste(rr, "\\", "\\", "\n", sep="")
    cat(rr)
  }
}

dir_path <- "../final_res/pnl_beta_10/"
result_generic_name <- "pnl_results_noise_cube_method_nnn_ddd_100_.json"
abpnl_generic_name <- "abpnl_results_noise_cube_abpnlnnn_ddd100.csv"

d <- 4
# ns <- c(100, seq(500, by=500, len=4))
ns <- seq(100, by=50, len=5)
col_names <- c("smoothed", "prlg", "abpnl", "resit")
noise <- "evd"
res_evd <- get_smoothed_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)

noise <- "gaussian"
res_gaussian <- get_smoothed_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)

res_gaussian$means
res_evd$means

res_gaussian$stds
res_evd$stds

print_latex_format(ns, res_gaussian, res_evd)



# no smoothed here
d <- 4
ns <- c(100, seq(500, by=500, len=4))
col_names <- c("prlg", "abpnl", "resit")
noise <- "evd"
res_evd <- get_prlg_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)

noise <- "gaussian"
res_gaussian <- get_prlg_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)

res_gaussian$means
res_evd$means

res_gaussian$stds
res_evd$stds

print_latex_format(ns, res_gaussian, res_evd)

d <- 7
noise <- "evd"
res_evd <- get_prlg_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)

noise <- "gaussian"
res_gaussian <- get_prlg_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)

res_gaussian$means
res_evd$means

res_gaussian$stds
res_evd$stds

print_latex_format(ns, res_gaussian, res_evd)
