library(tidyverse)
library(janitor)
library(rstan)
library(rstanarm)
library(bayesplot)
library(matrixStats)

options(mc.cores = parallel::detectCores(logical = FALSE))

data <- read_csv("transformed/all_2p_house+senate_races_trainset.csv")

data <- data %>% mutate(
  dem_funds_2p_pct_sqrd = dem_funds_2p_pct**2,
  effn = pmax(dem_effn, rep_effn),
  sqrt_effn = sqrt(effn),
  poll_margin = rep_poll_avg - dem_poll_avg, # Keep consistency in convention
  dem_pct_2p_offset = dem_pct_2p - 50,
  baseline = 2*pvi - generic_ballot_avg, # PVI is offset from 50%, not margin
  funds_pct_margin = if_else(dem_tot_funds + rep_tot_funds == 0, 0, (dem_tot_funds - rep_tot_funds) / (dem_tot_funds + rep_tot_funds)) * 100
)

# sb_elasticity <- read_csv("data/silver_bulletin_state_elasticity.csv")

# data <- data %>% left_join(sb_elasticity, join_by(state_po))

# data <- data %>% mutate(
#  prior_lean = pvi - (elasticity * generic_ballot_avg)
#)

set.seed(3700)

train_data <- bind_rows(data %>% filter(chamber == "House"), data %>% filter(chamber == "Senate") %>% 
                          sample_frac(0.67))

test_data <- anti_join(data, train_data, by=c("year", "state_po", "geography", "chamber"))

fit <- stan_glmer( dem_pct_2p_offset ~ 0 + baseline + sqrt_effn:baseline +
                     funds_pct_margin + inc_dummy +
                     polarization:funds_pct_margin + polarization:inc_dummy +
                     (1 | dem_cand) + (1 | rep_cand) +  (1 | demo_cluster:year) + (1 | year) +
                     (1 | state:year) + (1 | census_region:year) + (1 | chamber:year) + 
                     sqrt_effn:poll_margin + sqrt_effn:inc_dummy + sqrt_effn:funds_pct_margin + 
                     sqrt_effn:net_scandal_score + net_scandal_score,
                   family = gaussian(),
                   data = train_data,
                   prior = student_t(location = 0, scale = 4, df = 5, autoscale = TRUE),
                   adapt_delta = 0.95,
                   refresh = 10,
                   iter = 5000*2,
                   seed = 1010
)
print(fit)