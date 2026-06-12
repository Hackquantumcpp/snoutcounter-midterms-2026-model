library(tidyverse)
library(janitor)
library(rstan)
library(rstanarm)

options(mc.cores = parallel::detectCores(logical = FALSE))

data <- read_csv("transformed/all_2p_house_races_trainset.csv")

data <- data %>% mutate(
  dem_funds_2p_pct_sqrd = dem_funds_2p_pct**2
)

fit <- stan_glmer( dem_pct_2p ~ prev_lean + prev2_lean + generic_ballot_avg +
                     dem_funds_2p_pct_sqrd + dem_inc_dummy + rep_inc_dummy +
                     (1 | dem_cand) + (1 | rep_cand) + (1 | state),
                   family = gaussian(),
                   data = data,
                   prior = normal(0, 1, autoscale = TRUE),
                   adapt_delta = 0.99,
                   refresh = 100,
                   seed = 1010
)
print(fit)