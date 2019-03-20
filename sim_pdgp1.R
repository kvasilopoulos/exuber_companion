library(MASS)
#'
#'
#' @importFrom MASS mvrnorm
#'
sim_pdgp <- function(n, ns, nb, te = 0.4 * n, tf = 0.2 * n + te, r = 1.05,
                     corr = 0.25, aggregate = FALSE) {

  ymat <- matrix(NA, n, ns) # matrix with y series
  ymat[1, ] <- rep(5, ns) # initial values
  varr <- .2 # same variance for all N variables
  covv <- corr * sqrt(varr * varr) # covariance
  Sigma <- matrix(covv, ns, ns)
  diag(Sigma) <- varr
  mu <- rep(0, ns)
  emat <- mvrnorm(n = n, mu = mu, Sigma = Sigma)
  cserror <- apply(emat[(te + 1):n, ], 2, cumsum)

  for (j in 1:nb) {
    for (t in 2:n) {
      if (t < te) {
        ymat[t, j] <- ymat[t - 1, j] + emat[t, j]
      } else if (t >= te && t <= tf) {
        ymat[t, j] <- r * ymat[t - 1, j] + emat[t, j]
      } else {
        ymat[t, j] <- ymat[te - 1, j] + cserror[t - tf, j]
      }
    }

    if (nb < ns) {
      for (j in (nb + 1):ns) {
        ymat[, j] <- stats::filter(c(0, emat[-1, j]), 1, "rec", init = ymat[1, j])
      }
    }
  }

  x_agg <- rowMeans(ymat) # aggregate series

  if(aggregate) return(x_agg) else return(ymat)
}




