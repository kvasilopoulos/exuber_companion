
library(tidyverse)

set.seed(123)
x <- sim_dgp1(100)
plot.ts(x)

bx <- 
  x %>% 
  radf() %>% 
  pluck("bsadf")

mc <- 
  mc_cv(100, opt_bsadf = "conservative") %>% 
  pluck("bsadf_cv") %>% 
  as.data.frame() %>% 
  select("95%")


rx <- rev(x)
plot.ts(rx)

brx <- 
  rx %>% 
  radf() %>% 
  pluck("bsadf")


plot.ts(cbind(bx, mc), plot.type = "single", col = c(1,2))

plot.ts(cbind(1-brx, mc), plot.type = "single", col = c(1,2))
