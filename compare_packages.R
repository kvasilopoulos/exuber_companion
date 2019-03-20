library(exuber)
library(psymonitor)
library(MultipleBubbles)
library(microbenchmark)

results <- 
  microbenchmark(
    x1 <- exuber::mc_cv(200, nrep = 2000),
    x2 <- psymonitor::cvPSYmc(200, nrep = 2000, multiplicity = FALSE, nCores = 3),
    x3 <- MultipleBubbles::gsadf(m = 2000, t = 200),
    times = 10L
  )

