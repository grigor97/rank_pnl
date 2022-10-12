library(ggplot2)
library(rjson)

library(latex2exp)

library(foreach)


get_rel_path <- function(dir_path, generic_name, noise, method, h, n) {
  result_name <- str_replace(generic_name, "nnn", as.character(n))
  result_name <- str_replace(result_name, "hhh", as.character(h))
  result_name <- str_replace(result_name, "method", method)
  result_name <- str_replace(result_name, "noise", noise)
  result_name <- paste(dir_path, result_name, sep="")
  result_name
}

get_df_from_ltm_res <- function(dir_path, generic_name, noise, method, h, n) {
  ltm_path <- get_rel_path(dir_path, generic_name, noise, method, h, n)
  
  ltm_res <- fromJSON(file=ltm_path)
  
  noise_name <- if (noise=="gaussian") "Gaussian" else "Gumbel"
  method_name = if (method == "prlg") "RankG" else "RankS"
  df_ltm_res <- data.frame(Y=ltm_res$Y, 
                           Z =ltm_res$Z, 
                           est_Z=ltm_res$est_Z,
                           noise=ltm_res$noise,
                           est_noise=ltm_res$est_noise,
                           beta_10=10,
                           est_beta_10=matrix(ltm_res$est_beta, 3)[1,],
                           beta_5=5,
                           est_beta_5=matrix(ltm_res$est_beta, 3)[2,],
                           method=method_name,
                           noise_name=noise_name,
                           h=h,
                           n=n)
  
  df_ltm_res <- df_ltm_res[1001:1500, ]
  
  df_ltm_res$noise_name <- factor(df_ltm_res$noise_name, levels = c("Gaussian", "Gumbel"))
  df_ltm_res$method <- factor(df_ltm_res$method, levels = c("RankG", "RankS"))
  df_ltm_res
}


dir_path <- "../final_res/ltm_res_beta/"
generic_name <- "results_noise_hhh_method_nnn_100_.json"

n <- 500 # 100, 500, 1000

h <- "cube" # "log", "cube"

# noise <- "gaussian" # "evd", "gaussian"
# method <- "smoothed" # "smoothed", "prlg"


noises <- c("evd", "gaussian")
methods <- c("smoothed", "prlg")

df_cube_500 <- foreach(noise = noises, .combine="rbind") %do% {
  foreach(method = methods, .combine="rbind") %do% {
  get_df_from_ltm_res(dir_path, generic_name, noise, method, h, n)
  }
}

head(df_cube_500)



ggplot(data=df_cube_500) + 
  geom_line(aes(x=Y, y=Z, color="true")) +
  geom_point(aes(x=Y, y=est_Z, color="estimated"), alpha=0.1) +
  scale_color_manual(values=c("red", "black"), 
                     breaks=c("true", "estimated")) +
  labs(color='', title="", x="y", y=TeX(r'($h(y)$)')) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "None") +
  facet_grid(method~noise_name) 


ggplot(data=df_cube_500) + 
  geom_line(aes(x=noise, y=noise, color="true")) +
  geom_point(aes(x=noise, y=est_noise, color="estimated"), alpha=0.1) +
  scale_color_manual(values=c("red", "black"), 
                     breaks=c("true", "estimated")) +
  labs(color='', title="", x="true noise", y="estimated noise") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "None") +
  facet_grid(method~noise_name)


ggplot(data=df_cube_500) + 
  geom_point(aes(x=1, y=beta_10, color="true")) +
  geom_boxplot(aes(x=1, y=est_beta_10, color="estimated"), alpha=0.1) +
  geom_point(aes(x=2, y=beta_5, color="true")) +
  geom_boxplot(aes(x=2, y=est_beta_5, color="estimated"), alpha=0.1) +
  ylim(c(3.2, 19)) +
  scale_color_manual(values=c("red", "black"),
                     breaks=c("true", "estimated")) +
  labs(color='', title="", x="", y=TeX(r'($\beta_0$)')) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "None") +
  facet_grid(method~noise_name) +
  scale_x_discrete(limits=c(1, 2), 
                   labels=c(TeX(r'($\beta_0^{(1)}$)'), TeX(r'($\beta_0^{(2)}$)')))


