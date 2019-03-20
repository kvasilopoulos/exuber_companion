
price <- sim_dgp1(100)
x <- price
# Bo <- sum(dx^2)/sum((x - x[1])^2)

B_tau <- function(x, tau) {
  
  # tau = 10
  dx <- c(NA, diff(x))
  n <- NROW(x)
  
  smpl <- seq(tau + 1, length.out = n - tau)
  
  sig <- sum(dx[smpl]^2)/(n - tau)
  B <- sum((x[smpl] - x[tau])^2)/(sig*(n - tau)^2)
  
  return(B)
}

B_stat <- function(x) {
  n <- NROW(x)
  empty <- numeric()
  for (i in (0.1*n):(0.9*n)) {
    empty[i] <- B_tau(x, tau = i)
  }
  return(max(empty, na.rm = T))
}

sim <- numeric()
for (i in 1:2000) {
  wn <- cumsum(rnorm(326))
  sim[i] <- B_stat(wn)
}

sim %>% quantile(probs = 0.99)

