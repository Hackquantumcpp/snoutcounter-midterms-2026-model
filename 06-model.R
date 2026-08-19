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
  poll_margin = rep_poll_avg - dem_poll_avg, # Keep consistency in convention
  dem_pct_2p_offset = dem_pct_2p - 50,
  dem_funds_2p_pct_offset = dem_funds_2p_pct - 50,
  baseline = 2*pvi - generic_ballot_avg,
  funds_pct_margin = if_else(dem_tot_funds + rep_tot_funds == 0, 0, (dem_tot_funds - rep_tot_funds) / (dem_tot_funds + rep_tot_funds)) * 100
)

fit <- stan_glmer( dem_pct_2p_offset ~ 0 + baseline +
                     funds_pct_margin + inc_dummy +
                     polarization:funds_pct_margin + polarization:inc_dummy +
                     (1 | dem_cand) + (1 | rep_cand) +  (1 | demo_cluster:year) + (1 | year) +
                     (1 | state:year) + (1 | census_region:year) + sqrt_effn:poll_margin +
                     dem_scandal_score + rep_scandal_score,
                   family = gaussian(),
                   data = data,
                   prior = student_t(location = 0, scale = 4, df = 5, autoscale = TRUE),
                   adapt_delta = 0.95,
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
neff_ratio(fit, pars = c("pvi", "prior_lean", "dem_inc_dummy", "rep_inc_dummy", "baseline",
                         "inc_dummy", "dem_funds_2p_pct_offset",
                         "generic_ballot_avg", "funds_pct_margin",
                         "funds_pct_margin:polarization", "inc_dummy:polarization",
                         "cvap_hisp_pct", 
                         "cvap_natam_pct",
                         "cvap_black_pct", "cvap_aapi_pct",
                         "sqrt_effn:poll_margin", "college",
                         "dem_scandal_score", "rep_scandal_score"))
rhat(fit, pars = c("pvi", "dem_inc_dummy", "rep_inc_dummy", "baseline",
                   "inc_dummy", "dem_funds_2p_pct_offset",
                   "generic_ballot_avg", "funds_pct_margin",
                   "funds_pct_margin:polarization", "inc_dummy:polarization",
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
                         "Sigma[census_region:year:(Intercept),(Intercept)]",
                         "Sigma[demo_cluster:year:(Intercept),(Intercept)]"))
rhat(fit, pars = c("Sigma[dem_cand:(Intercept),(Intercept)]",
                   "Sigma[rep_cand:(Intercept),(Intercept)]",
                   "Sigma[year:(Intercept),(Intercept)]",
                   "Sigma[state:year:(Intercept),(Intercept)]",
                   "Sigma[state:(Intercept),(Intercept)]",
                   "Sigma[census_region:(Intercept),(Intercept)]",
                   "Sigma[census_region:year:(Intercept),(Intercept)]",
                   "Sigma[demo_cluster:year:(Intercept),(Intercept)]"))


pp_check(fit, nreps = 100)

saveRDS(fit, "model/house_model.RDS")