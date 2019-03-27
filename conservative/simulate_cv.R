setwd("D://exuber_companion")

library(exuber)


options(exuber.show_progress = FALSE)
mc_con <- vector("list", length = 2000)
for(i in 960:2000) {
  mc_con[[i]] <- exuber::mc_cv(i, opt_bsadf = "conservative")
  print(i)
}
saveRDS(mc_con, "mc_con.Rds")

# names(mc_con) <- c(paste0("_nan", 1:5), c(paste0("n", 6:nn)))
class(mc_con) <- c("list", "crit")

# ... ---------------------------------------------------------------------

usethis::use_data(crit, overwrite = TRUE, compress = "xz")

print.crit <- function(x, ...) {
  # we dont want to overwhelm the console
  print(tibble::enframe(x))
}
