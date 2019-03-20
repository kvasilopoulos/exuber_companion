load("~/R/exuber_companion/crit.rda")

x <- crit

library(tidyverse)


library(furrr)



dfize <- function(name, column) {
  x1 <- 
    x[6:2000] %>% 
    future_map(~ .x %>% 
          pluck(name) %>% 
          "["(, column) %>% 
          as.tibble()
    )
  x2 <- 
    x1 %>% 
    future_map(~.x %>% 
                 mutate(sort = 1:NROW(.x)) %>% 
                 select(sort, everything())) %>% 
    reduce(full_join, by = "sort")
  x3 <- 
    x2 %>% 
    select(-sort) %>% 
    set_names(names(x1))
  x3
}


bsadf_95 <- dfize("bsadf_cv", column = 2)
