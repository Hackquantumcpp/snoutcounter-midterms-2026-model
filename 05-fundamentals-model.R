library(tidyverse)
library(janitor)
library(rstan)
library(rstanarm)
library(bayesplot)

options(mc.cores = parallel::detectCores(logical = FALSE))

data <- read_csv("transformed/all_2p_house_races_trainset.csv")

data <- data %>% mutate(
  dem_funds_2p_pct_sqrd = dem_funds_2p_pct**2
)

fit <- stan_glmer( dem_pct_2p ~ pvi + generic_ballot_avg +
                     dem_funds_2p_pct_sqrd + dem_inc_dummy + rep_inc_dummy +
                     (1 | dem_cand) + (1 | rep_cand) + (1 | state) + factor(year),
                   family = gaussian(),
                   data = data,
                   prior = normal(0, 1, autoscale = TRUE),
                   adapt_delta = 0.99,
                   refresh = 100,
                   iter = 5000*2,
                   seed = 1010
)
print(fit)

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
                         "factor(year)2020", "factor(year)2022"))
rhat(fit, pars = c("pvi", "dem_inc_dummy", "rep_inc_dummy",
                   "generic_ballot_avg", "dem_funds_2p_pct_sqrd",
                   "factor(year)2020", "factor(year)2022"))

