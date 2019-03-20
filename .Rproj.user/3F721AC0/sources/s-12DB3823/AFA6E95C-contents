

x <- sim_dgp1(100)

cummean <- function(x) cumsum(x) / seq_along(x) 

cusum <- function(x) {
  n <- NROW(x)
  minw <- floor((0.01 + 1.8 / sqrt(n)) * n)
  
  dx <- diff(x)
  
  mu <- cummean(dx)
  sigma <- sqrt(cumsum((dx - mu)^2)/(seq_along(dx) - 1))

  dx_adj <- c(sum(dx[1:minw]), dx[-c(1:minw)])
  
  stat <- 1/sigma[-c(1:(minw - 1))] * cumsum(dx_adj)
  
  # critical values
  # k_a = 4.6 according to Chu, Stinchcombe, and White (1996)
  # crit <- sqrt(4.6 + log(((minw + 1):n)/minw))
  
  return(stat)
    
}


mc_cusum <- function(n, nrep = 2000) { 
  minw <- floor((0.01 + 1.8 / sqrt(n)) * n)
  
  sim <- matrix(nrow = n - minw, ncol = nrep)
  
  for (i in 1:nrep) {
    rw <- cumsum(rnorm(n))
    sim[,i] <- cusum(rw)
  }
  
  return(sim %>% apply(1, quantile, probs = 0.95))
  
}


## Example

s1 <- sim_dgp2(100) %>% cusum()
s1_crit <- mc_cusum(n = 100)
plot.ts(cbind(s1, s1_crit), plot.type = "single")

