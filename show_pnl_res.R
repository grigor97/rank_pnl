library(rjson)
library(matrixStats)

library(stringr) 
library(ggplot2)
require(gridExtra)
library(scales)

load_prlg_res <- function(dir_path, file_name) {
  f_path = paste(dir_path, file_name, sep="")
  pnl_res <- fromJSON(file=f_path)
  
  wrongs <- matrix(pnl_res$res, pnl_res$num_datasets, 3)
  wrongs
}


# ddd <- load_prlg_res("../post_final_res/pnl_beta_100/",
#               "pnl_results_gaussian_cube_prlg_2000_4_100_.json")
# colMeans(ddd)
# ddd

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


result_generic_name <- "pnl_results_noise_cube_method_nnn_ddd_100_.json"
abpnl_generic_name <- "abpnl_results_noise_cube_abpnlnnn_ddd100.csv"


# 4 nodes smoothed
d <- 4
ns <- seq(100, by=50, len=5)
col_names <- c("smoothed", "prlg", "abpnl", "resit")

dir_path <- "../post_final_res/pnl_beta_100/"
noise <- "evd"
res_evd_sm_100 <- get_smoothed_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)
noise <- "gaussian"
res_gaussian_sm_100 <- get_smoothed_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)

dir_path <- "../post_final_res/pnl_beta_10/"
noise <- "evd"
res_evd_sm_10 <- get_smoothed_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)
noise <- "gaussian"
res_gaussian_sm_10 <- get_smoothed_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)

# print_latex_format(ns, res_gaussian, res_evd)
res_gaussian_sm_10

df_gaussian_sm_100 <- data.frame(res_gaussian_sm_100$means)
df_gaussian_sm_100["sample_size"] <- ns
df_evd_sm_100 <- data.frame(res_evd_sm_100$means)
df_evd_sm_100["sample_size"] <- ns

df_gaussian_sm_10 <- data.frame(res_gaussian_sm_10$means)
df_gaussian_sm_10["sample_size"] <- ns
df_evd_sm_10 <- data.frame(res_evd_sm_10$means)
df_evd_sm_10["sample_size"] <- ns


# 4 nodes prlg
d <- 4

ns <- c(100, seq(500, by=500, len=4))
col_names <- c("prlg", "abpnl", "resit")

dir_path <- "../post_final_res/pnl_beta_100/"
noise <- "evd"
res_evd_prlg_100 <- get_prlg_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)

noise <- "gaussian"
res_gaussian_prlg_100 <- get_prlg_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)
# res_gaussian_prlg_100

dir_path <- "../post_final_res/pnl_beta_10/"
noise <- "evd"
res_evd_prlg_10 <- get_prlg_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)
noise <- "gaussian"
res_gaussian_prlg_10 <- get_prlg_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)

# print_latex_format(ns, res_gaussian, res_evd)


df_gaussian_prlg_100 <- data.frame(res_gaussian_prlg_100$means)
df_gaussian_prlg_100["sample_size"] <- ns
df_evd_prlg_100 <- data.frame(res_evd_prlg_100$means)
df_evd_prlg_100["sample_size"] <- ns

df_gaussian_prlg_10 <- data.frame(res_gaussian_prlg_10$means)
df_gaussian_prlg_10["sample_size"] <- ns
df_evd_prlg_10 <- data.frame(res_evd_prlg_10$means)
df_evd_prlg_10["sample_size"] <- ns



# 7 nodes prlg
d <- 7

ns <- c(100, seq(500, by=500, len=4))
col_names <- c("prlg", "abpnl", "resit")

dir_path <- "../post_final_res/pnl_beta_100/"
noise <- "evd"
res_evd_prlg_100_7 <- get_prlg_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)

noise <- "gaussian"
res_gaussian_prlg_100_7 <- get_prlg_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)


dir_path <- "../post_final_res/pnl_beta_10/"
noise <- "evd"
res_evd_prlg_10_7 <- get_prlg_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)
noise <- "gaussian"
res_gaussian_prlg_10_7 <- get_prlg_all_results(dir_path, col_names, ns, d, noise, result_generic_name, abpnl_generic_name)


df_gaussian_prlg_100_7 <- data.frame(res_gaussian_prlg_100_7$means)
df_gaussian_prlg_100_7["sample_size"] <- ns
df_evd_prlg_100_7 <- data.frame(res_evd_prlg_100_7$means)
df_evd_prlg_100_7["sample_size"] <- ns

df_gaussian_prlg_10_7 <- data.frame(res_gaussian_prlg_10_7$means)
df_gaussian_prlg_10_7["sample_size"] <- ns
df_evd_prlg_10_7 <- data.frame(res_evd_prlg_10_7$means)
df_evd_prlg_10_7["sample_size"] <- ns


# mean estimates

df_gaussian_sm_100
df_evd_sm_100
df_gaussian_sm_10
df_evd_sm_10


df_gaussian_prlg_100
df_evd_prlg_100
df_gaussian_prlg_10
df_evd_prlg_10


df_gaussian_prlg_100_7
df_evd_prlg_100_7
df_gaussian_prlg_10_7
df_evd_prlg_10_7

# end mean estimates

df_gaussian_sm_100["noise"] <- "Gaussian"
df_evd_sm_100["noise"] <- "Gumbel"
df_gaussian_sm_10["noise"] <- "Gaussian"
df_evd_sm_10["noise"] <- "Gumbel"

df_gaussian_sm_100["beta"] <- 100
df_evd_sm_100["beta"] <- 100
df_gaussian_sm_10["beta"] <- 10
df_evd_sm_10["beta"] <- 10

df_sm <- rbind(df_gaussian_sm_100, df_evd_sm_100, df_gaussian_sm_10, df_evd_sm_10)
rownames(df_sm) <- NULL
df_sm$beta <- factor(df_sm$beta)
df_sm$noise <- factor(df_sm$noise, levels = c("Gaussian", "Gumbel"))
df_sm

library("RColorBrewer")
# colorsss <- hue_pal()(4)
cls <-  brewer.pal(7, "Set1")

ggplot(data=df_sm) +
  geom_line(aes(x=sample_size, y=smoothed, linetype=beta, color="RankS")) +
  geom_point(aes(x=sample_size, y=smoothed, color="RankS")) +
  geom_line(aes(x=sample_size, y=prlg, linetype=beta, color="RankG")) +
  geom_point(aes(x=sample_size, y=prlg, color="RankG")) +
  geom_line(aes(x=sample_size, y=abpnl, linetype=beta, color="AbPNL")) +
  geom_point(aes(x=sample_size, y=abpnl, color="AbPNL")) +
  geom_line(aes(x=sample_size, y=resit, linetype=beta, color="RESIT")) +
  geom_point(aes(x=sample_size, y=resit, color="RESIT")) +
  facet_wrap(vars(noise)) + 
  scale_color_manual(values=cls[c(4, 3, 7, 2)], 
                     breaks=c("RankS", "RankG", "AbPNL", "RESIT")) +
  scale_linetype_manual(values=c("dashed", "solid"), 
                        breaks=c(10, 100),
                        labels=c("weak", "strong"),
                        name="Signal") +
  labs(color='Methods', title="", x="sample size", y="error") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right") +
  guides(color=guide_legend(order = 1), linetype=guide_legend(order = 2))





df_gaussian_prlg_100
df_evd_prlg_100
df_gaussian_prlg_10
df_evd_prlg_10


df_gaussian_prlg_100_7
df_evd_prlg_100_7
df_gaussian_prlg_10_7
df_evd_prlg_10_7


df_gaussian_prlg_100["noise"] <- "Gaussian"
df_evd_prlg_100["noise"] <- "Gumbel"
df_gaussian_prlg_10["noise"] <- "Gaussian"
df_evd_prlg_10["noise"] <- "Gumbel"

df_gaussian_prlg_100["beta"] <- 100
df_evd_prlg_100["beta"] <- 100
df_gaussian_prlg_10["beta"] <- 10
df_evd_prlg_10["beta"] <- 10

df_gaussian_prlg_100["d"] <- "4 nodes"
df_evd_prlg_100["d"] <- "4 nodes"
df_gaussian_prlg_10["d"] <- "4 nodes"
df_evd_prlg_10["d"] <- "4 nodes"


df_gaussian_prlg_100_7["noise"] <- "Gaussian"
df_evd_prlg_100_7["noise"] <- "Gumbel"
df_gaussian_prlg_10_7["noise"] <- "Gaussian"
df_evd_prlg_10_7["noise"] <- "Gumbel"

df_gaussian_prlg_100_7["beta"] <- 100
df_evd_prlg_100_7["beta"] <- 100
df_gaussian_prlg_10_7["beta"] <- 10
df_evd_prlg_10_7["beta"] <- 10

df_gaussian_prlg_100_7["d"] <- "7 nodes"
df_evd_prlg_100_7["d"] <- "7 nodes"
df_gaussian_prlg_10_7["d"] <- "7 nodes"
df_evd_prlg_10_7["d"] <- "7 nodes"

df_prlg <- rbind(df_gaussian_prlg_100, 
                 df_evd_prlg_100, 
                 df_gaussian_prlg_10, 
                 df_evd_prlg_10,
                 df_gaussian_prlg_100_7, 
                 df_evd_prlg_100_7, 
                 df_gaussian_prlg_10_7, 
                 df_evd_prlg_10_7)
rownames(df_prlg) <- NULL
df_prlg$beta <- factor(df_prlg$beta)
df_prlg$d <- factor(df_prlg$d)
df_prlg$noise <- factor(df_prlg$noise, levels = c("Gaussian", "Gumbel"))
df_prlg


ggplot(data=df_prlg) +
  geom_line(aes(x=sample_size, y=prlg, linetype=beta, color="RankG")) +
  geom_point(aes(x=sample_size, y=prlg, color="RankG")) +
  geom_line(aes(x=sample_size, y=abpnl, linetype=beta, color="AbPNL")) +
  geom_point(aes(x=sample_size, y=abpnl, color="AbPNL")) +
  geom_line(aes(x=sample_size, y=resit, linetype=beta, color="RESIT")) +
  geom_point(aes(x=sample_size, y=resit, color="RESIT")) +
  facet_grid(d~noise) + 
  scale_color_manual(values=cls[c(3, 7, 2)], 
                     breaks=c("RankG", "AbPNL", "RESIT")) +
  scale_linetype_manual(values=c("dashed", "solid"), 
                        breaks=c(10, 100),
                        labels=c("weak", "strong"),
                        name="Signal") +
  labs(color='Methods', title="", x="sample size", y="error") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right") +
  guides(color=guide_legend(order = 1), linetype=guide_legend(order = 2))


df_prlg

df_gaussian_sm_10$sample_size
res_gaussian_sm_10
res_evd_sm_10

print_latex_format(df_gaussian_sm_10$sample_size, 
                   res_gaussian_sm_10, 
                   res_evd_sm_10)

print_latex_format(df_gaussian_sm_100$sample_size, 
                   res_gaussian_sm_100, 
                   res_evd_sm_100)

print_latex_format(df_gaussian_prlg_10_7$sample_size, 
                   res_gaussian_prlg_10_7, 
                   res_evd_prlg_10_7)

print_latex_format(df_gaussian_prlg_100_7$sample_size, 
                   res_gaussian_prlg_100_7, 
                   res_evd_prlg_100_7)


print_latex_format(df_gaussian_prlg_10$sample_size, 
                   res_gaussian_prlg_10, 
                   res_evd_prlg_10)

print_latex_format(df_gaussian_prlg_100$sample_size, 
                   res_gaussian_prlg_100, 
                   res_evd_prlg_100)




