

# Bo <- sum(dx^2)/sum((x - x[1])^2)

kim <- function(x, tau) {
  
  # tau = 50
  n <- NROW(x)
  
  smpl <- seq(tau + 1, length.out = n - tau)
  
  
  num <-  sum((x[smpl] - x[tau])^2)*(n - tau)^(-2)
  den <- sum((x[-smpl] - x[1])^2)*tau^(-2)
  stat <- num/den
  
  return(stat)
}

kim_stat <- function(x) {
  n <- NROW(x)
  empty <- numeric()
  for (i in (0.1*n):(0.9*n)) {
    empty[i] <- kim(x, tau = i)
  }
  return(max(empty, na.rm = T))
}


sim <- numeric()
for (i in 1:2000) {
    wn <- cumsum(rnorm(326))
    sim[i] <- kim_stat(wn)
}

sim %>% quantile(probs = 0.99)
