library(dHSIC)

# calculates the HSIC measure
dependence.hsic <- function(U, V) {
  dhsic(U, V, kernel = "gaussian.fixed")$dHSIC
}

# estimates the causal order for PNL models
est.pnl.causal.order <- function(data, beta.est.alg, h.est.alg, 
                                 dependence.measure=dependence.hsic, 
                                 output=FALSE) {
  if(output) {
    print("start recovering causal order")
    print("nodes are")
    print(names(data))
  }
  
  order_of_nodes <- c()
  cur_nodes <- c(1:ncol(data))
  
  while(length(cur_nodes) > 1) {
    min_dep_val <- 1000
    last_node <- cur_nodes[1]
    for(node in cur_nodes) {
      rem_nodes <- cur_nodes[!cur_nodes %in% node]
      
      if(output) {
        print("----------------")
        print(paste(node, "versus"))
        print(rem_nodes)
      }
      
      Y <- as.matrix(data[, node])
      X <- as.matrix(data[, rem_nodes])
      XX <- cbind(X, X^2)
      
      res_beta_est <- beta.est.alg(Y, XX)
      res_h_est <- h.est.alg(res_beta_est$est_beta, Y, XX)
      res_est_noise <- res_h_est - (XX %*% res_beta_est$est_beta)
      
      dependence_val <- dependence.measure(X, res_est_noise)
      
      if(dependence_val < min_dep_val) {
        min_dep_val <- dependence_val
        last_node <- node
        
        if(output) {
          print("min dependence is updated")
        }
      }
      
    }
    
    order_of_nodes <- c(last_node, order_of_nodes)
    cur_nodes <- cur_nodes[!cur_nodes %in% last_node]
    if(output) {
      print("current nodes ")
      print(cur_nodes)
    }
  }
  
  order_of_nodes <- c(cur_nodes[1], order_of_nodes)
  if(output) {
    print(paste("order is", order_of_nodes))
    print("end of recovering the order")
  }
  return(order_of_nodes)
}