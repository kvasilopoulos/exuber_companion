# Test for an end-of-sample bubble in financial time series
# Astill, Harvey, Leybourne and Taylor 2017

library(exuber)
x <- sim_dgp1(100)


m <- 5
dx <- diff(x)
N <- NROW(dx)

sm_crit <- numeric()
for (j in 1:(N - m)) {
  for (t in (j + 1):(j + m)) {
    sm_crit[t] <- (t - j) * dx[t]
  }
}
sm_sum <- sum(sm_crit, na.rm = T)
sm_sum
