
price <- sim_dgp2(100)
# x <- price
# Bo <- sum(dx^2)/sum((x - x[1])^2)

BT <- function(x, tau) {
  
  n <- NROW(x)
  dx <- diff(x)
  
  smpl <- seq(tau + 1, length.out = n - tau)
  
  sig <- sum(dx^2)/n # full sample variance estimator
  
  B <- sum((x[n] - x[smpl])^2)/(sig*(n - tau)^2)
  
  return(B)
}

BT_stat <- function(x) {
  n <- NROW(x)
  empty <- numeric()
  for (i in (0.1*n):(0.9*n)) {
    empty[i] <- BT(x, tau = i)
  }
  return(max(empty, na.rm = T))
}

library(parallel)
sim <- foreach(
  i = 1:nrep,
  .export = c("BT_stat", "BT"),
  .combine = "cbind"
) %dopar% {
  y <- cumsum(rnorm(n))
  BT_stat(y)
}


sim <- numeric()
for (i in 1:5000) {
  wn <- cumsum(rnorm(2000))
  sim[i] <- BT_stat(wn)
}

sim %>% quantile(probs = c(0.90,0.95,0.99))




