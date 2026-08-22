library(tidyverse)
library(janitor)
library(rstan)
library(rstanarm)
library(bayesplot)
library(matrixStats)

options(mc.cores = parallel::detectCores(logical = FALSE))

data <- read_csv('transformed/war_data.csv')

data <- data %>% mutate(
  inc_dummy = dem_inc_dummy - rep_inc_dummy,
  lagged_pres_swing = dem_2p_prev - dem_2p_prev2,
  funds_pct_margin = if_else(dem_tot_funds + rep_tot_funds == 0, 0, (dem_tot_funds - rep_tot_funds) / (dem_tot_funds + rep_tot_funds)) * 100,
  over_under = dem_pct_2p - dem_2p_prev
)

war_model <- stan_glmer( over_under ~ inc_dummy + lagged_pres_swing + is_midterm:shave + funds_pct_margin +
                           (1 | dem_cand) + (1 | rep_cand) + # Our WAR metrics
                           (1 | state:year) + (1 | demo_cluster:year) +
                           (1 | year), ## Comment out if there is only one year in data
                         family = gaussian(),
                         data = data,
                         prior = student_t(location = 0, scale = 4, df = 5, autoscale = TRUE),
                         adapt_delta = 0.95,
                         refresh = 10,
                         iter = 1000*2,
                         seed = 1010
)
