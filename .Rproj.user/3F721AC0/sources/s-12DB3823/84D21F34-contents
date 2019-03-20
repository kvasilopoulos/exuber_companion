

download_cv <- function(overwrite = TRUE) {
  url <- "https://github.com/kvasilopoulos/latex-abstract/archive/master.zip"
  filename <- "cv.zip"
  download.file(url, destfile = filename)
  unzip(filename, overwrite = overwrite)
  content_raw <- unzip(filename, list = TRUE)
  content <- content_raw$Name
  file.remove(paste0("./", filename))
}

use_crit <- function() {
  load("crit.rda")
}


download_cv()
use_crit()

library(exuber)

sim_dgp1(100) %>% 
  radf() %>% 
  summary(cv = crit[[100]])
