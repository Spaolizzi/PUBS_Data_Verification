library(circular)
library(tidyverse)
n = 10000
mu = 0
kappa = 30
vonmises <- rvonmises(n, mu, kappa, control.circular=list())
plot(vonmises)
vonmises_reorient_df  <- data.frame(vonmises_orig = as.vector(vonmises)) %>% mutate(vonmises_altered = if_else(vonmises_orig > 180, vonmises_orig -360, vonmises_orig))
hist(as.vector(vonmises_reorient_df$vonmises_altered))
sd(vonmises_reorient_df$vonmises_altered)
hist(rnorm(n, mean = 0, sd = 18.75569))