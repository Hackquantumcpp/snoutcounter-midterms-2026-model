library(tidyverse)
library(janitor)
library(rstan)
library(rstanarm)
library(bayesplot)
library(matrixStats)

options(mc.cores = parallel::detectCores(logical = FALSE))

data <- read_csv("transformed/all_2p_house_races_trainset.csv")

data <- data %>% mutate(
  dem_funds_2p_pct_sqrd = dem_funds_2p_pct**2,
  effn = pmax(dem_effn, rep_effn),
  sqrt_effn = sqrt(effn),
  poll_margin = rep_poll_avg - dem_poll_avg # Keep consistency in convention
)

fit <- stan_glmer( dem_pct_2p ~ pvi + generic_ballot_avg + (generic_ballot_avg | state) +
                     dem_funds_2p_pct_sqrd + dem_inc_dummy + rep_inc_dummy +
                     cvap_hisp_pct + cvap_natam_pct + cvap_black_pct + cvap_aapi_pct +
                     (1 | dem_cand) + (1 | rep_cand) + (1 | year) + #+ (1 | state)
                     (1 | state:year) + (1 | census_region) + sqrt_effn:poll_margin + college +
                     dem_scandal_score + rep_scandal_score,
                   family = gaussian(),
                   data = data,
                   prior = normal(0, 2.5, autoscale = TRUE),
                   adapt_delta = 0.99,
                   refresh = 10,
                   iter = 5000*2,
                   seed = 1010
)
print(fit)
print(fixef(fit))
print(ranef(fit))

# Diagnostics
mcmc_trace(fit, pars = c("pvi", "dem_inc_dummy",
                         "rep_inc_dummy"))
mcmc_trace(fit, pars = c("generic_ballot_avg", 'dem_funds_2p_pct_sqrd',
                         'factor(year)2020', 'factor(year)2022'))
mcmc_trace(as.array(fit), regex_pars = 'Sigma')
mcmc_dens_overlay(fit, pars = c("pvi", "dem_inc_dummy",
                                "rep_inc_dummy")) + ylab('density')
mcmc_dens_overlay(fit, pars = c("generic_ballot_avg", 
                                "dem_funds_2p_pct_sqrd")) + ylab('density')
mcmc_dens_overlay(as.array(fit), regex_pars = 'Sigma') + ylab('density')
neff_ratio(fit, pars = c("pvi", "dem_inc_dummy", "rep_inc_dummy",
                         "generic_ballot_avg", "dem_funds_2p_pct_sqrd",
                         "cvap_hisp_pct", 
                         "cvap_natam_pct",
                         "cvap_black_pct", "cvap_aapi_pct",
                         "sqrt_effn:poll_margin", "college",
                         "dem_scandal_score", "rep_scandal_score"))
rhat(fit, pars = c("pvi", "dem_inc_dummy", "rep_inc_dummy",
                   "generic_ballot_avg", "dem_funds_2p_pct_sqrd",
                   "cvap_hisp_pct", 
                   "cvap_natam_pct",
                   "cvap_black_pct", "cvap_aapi_pct",
                   "sqrt_effn:poll_margin", "college",
                   "dem_scandal_score", "rep_scandal_score"))
neff_ratio(fit, pars = c("Sigma[dem_cand:(Intercept),(Intercept)]",
                         "Sigma[rep_cand:(Intercept),(Intercept)]",
                         "Sigma[year:(Intercept),(Intercept)]",
                         "Sigma[state:year:(Intercept),(Intercept)]",
                         "Sigma[state:(Intercept),(Intercept)]",
                         "Sigma[census_region:(Intercept),(Intercept)]"))
rhat(fit, pars = c("Sigma[dem_cand:(Intercept),(Intercept)]",
                   "Sigma[rep_cand:(Intercept),(Intercept)]",
                   "Sigma[year:(Intercept),(Intercept)]",
                   "Sigma[state:year:(Intercept),(Intercept)]",
                   "Sigma[state:(Intercept),(Intercept)]",
                   "Sigma[census_region:(Intercept),(Intercept)]"))

saveRDS(fit, "house_model.RDS")