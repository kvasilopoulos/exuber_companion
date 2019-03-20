
library(exuber)
x <- sim_div(100)
dx <- diff(x)
lag_x <- x[-length(x)]
N <- NROW(dx)


# Model 1 -----------------------------------------------------------------

ssr_model1 <- numeric(length = N - 1)
for (t in 1:N) {
  dummy1 <- rep(0, N)
  dummy1[t:N] <- 1

  reg <- lag_x * dummy1

  mode1 <- lm(dx ~ dummy1 + reg)
  ssr_model1[t] <- anova(mode1)["Residuals", "Sum Sq"]
}
m1_tau1 <- which.min(ssr_model1)
ssr1 <- ssr_model1[m1_tau1]

bic1 <- N * log(ssr1 / N) + (2 + 1) * log(N)

# Model 2 -----------------------------------------------------------------

ssr_model2 <- matrix(NA, N - 2, N - 2)

for (t1 in 2:(N - 1)) {
  for (t2 in (t1 + 1):N) { 
      # print(c(t1,t2))
    
    dummy12 <- rep(0, N)
    dummy12[t1:t2] <- 1

    reg <- lag_x * dummy12

    model2 <- lm(dx ~ dummy12 + reg)
    ssr_model2[t1 - 1, t2 - 2] <- anova(model2)["Residuals", "Sum Sq"]
  }
}

m2_tau1 <- apply(ssr_model2, 1, min, na.rm = T) %>% which.min()
m2_tau2 <- apply(ssr_model2[-c(1,N),-c(1,N)], 2, min, na.rm = T) %>% which.min()
ssr2 <- ssr_model2[m2_tau1, m2_tau2]


bic2 <- N * log(ssr2 / N) + (2 + 2) * log(N)
c(m2_tau1, m2_tau2)

# Model 3 -----------------------------------------------------------------

ssr_model3 <- matrix(NA, N, N)
for (t1 in 1:N) {
  for (t2 in t1:N) {
    dummy12 <- rep(0, N)
    dummy12[t1:t2] <- 1

    dummy2 <- rep(0, N)
    dummy2[t2:N] <- 1

    reg12 <- lag_x * dummy12
    reg2 <- lag_x * dummy2

    model3 <- lm(dx ~ dummy12 + dummy2 + reg12 + reg2)
    ssr_model3[t1, t2] <- anova(model3)["Residuals", "Sum Sq"]
  }
}

m3_tau1 <- apply(ssr_model3[-1, ], 1, min, na.rm = T) %>% which.min() + 1 # exclude the 1st obs
m3_tau2 <- apply(ssr_model3[-1, ], 2, min, na.rm = T) %>% which.min() + 1
ssr3 <- ssr_model3[m3_tau1, m3_tau2]

bic3 <- N * log(ssr3 / N) + (4 + 2) * log(N)

# Model 4 -----------------------------------------------------------------



# Model Selection ---------------------------------------------------------

data.frame(
  "BIC1" = bic1,
  "BIC2" = bic2,
  "BIC3" = bic3
)
