cdf.z <- function(y, subtract_values) {
  res = 0
  for(i in subtract_values) {
    res = res + pnorm(y - i)
  }
  res = res / length(subtract_values)
  return(res)
}

inverse <- function(f, lower, upper){
  function(y, arg){
    # if(f(lower, arg) - y > 0) {
    #   return(lower)
    # }
    # if (f(upper, arg) - y < 0) {
    #   return(upper)
    # }
    uniroot(function(x){f(x, arg) - y}, lower = lower, upper = upper, 
            extendInt="upX", tol=1e-3,)[1]$root
  }
}

inverse.cdf.z <- inverse(cdf.z, -100, 100)

# function h estimation in RankG method in the paper
h.est.rank.reg.gaussian <- function(est_beta, Y, X) {
  ranks_Y <- rank(Y)
  empirical_cdf_Y <- ranks_Y/(length(Y)+1)
  m_X <- X %*% est_beta
  
  f2_inv_y <- as.vector(sapply(empirical_cdf_Y, inverse.cdf.z, arg=m_X))
  return(f2_inv_y)
}