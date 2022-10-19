# rank_pnl
Ranked-Based Causal Discovery for Post-Nonlinear Models

Official implementation of rank-based PNL causal discovery methods RankG and RankS  
  
To run the methods the following installations are required. Start `R` in the current directory and type
```
install.packages("EnvStats")
install.packages("dHSIC")
```
  
  
To run an example of RankG method run the following R script in the current directory
```
Rscript run_RankG.R
```
which generates PNL model with sample size 100 and runs RankG method and outputs the estimated causal order. 
  
  
  
  
To run an example of RankS method run the following R script in the current directory
```
Rscript run_RankS.R
```
which generates PNL model with sample size 30 and runs RankS method and outputs the estimated causal order. 
  
  


