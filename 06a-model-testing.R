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
  baseline = 2*pvi - generic_ballot_avg, # PVI is offset from 50%, not margin
  funds_pct_margin = if_else(dem_tot_funds + rep_tot_funds == 0, 0, (dem_tot_funds - rep_tot_funds) / (dem_tot_funds + rep_tot_funds)) * 100
)

# sb_elasticity <- read_csv("data/silver_bulletin_state_elasticity.csv")

# data <- data %>% left_join(sb_elasticity, join_by(state_po))

# data <- data %>% mutate(
#  prior_lean = pvi - (elasticity * generic_ballot_avg)
#)

set.seed(3300)

train_data <- data %>% sample_frac(0.67)

test_data <- anti_join(data, train_data, by=c("year", "state_po", "district"))

fit <- stan_glmer( dem_pct_2p_offset ~ 0 + baseline + sqrt_effn:baseline +
                     (1 | demo_cluster:year) +
                     funds_pct_margin + inc_dummy + polarization:funds_pct_margin + polarization:inc_dummy +
                     (1 | dem_cand) + (1 | rep_cand) + (1 | state:year) + (1 | year) +
                     #(1 | state) + #(1 | year) +
                      (1 | census_region:year) + sqrt_effn:poll_margin + 
                     net_scandal_score,
                   family = gaussian(),
                   data = train_data,
                   prior = student_t(location = 0, scale = 4, df = 5, autoscale = TRUE),
                   adapt_delta = 0.95,
                   refresh = 10,
                   iter = 5000*2,
                   seed = 1010
)
print(fit)

## Tested random slopes + intercepts model
## Little change to accuracy, but matters a lot in some districts
## Tentative decision: do away with

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
                         "cvap_hisp_pct", "baseline:sqrt_effn",
                         "cvap_natam_pct",
                         "cvap_black_pct", "cvap_aapi_pct",
                         "sqrt_effn:poll_margin", "college",
                         "dem_scandal_score", "rep_scandal_score", "net_scandal_score"))
rhat(fit, pars = c("pvi", "dem_inc_dummy", "rep_inc_dummy", "baseline",
                   "inc_dummy", "dem_funds_2p_pct_offset",
                   "generic_ballot_avg", "funds_pct_margin",
                   "funds_pct_margin:polarization", "inc_dummy:polarization",
                   "cvap_hisp_pct", "baseline:sqrt_effn",
                   "cvap_natam_pct",
                   "cvap_black_pct", "cvap_aapi_pct",
                   "sqrt_effn:poll_margin", "college",
                   "dem_scandal_score", "rep_scandal_score", "net_scandal_score"))
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

# Validation

y_hat <- posterior_predict(fit, newdata = test_data)

pp_check(fit, nreps = 100)

y_hat_mean <- colMeans(y_hat)

y_hat_sd <- colSds(y_hat)

y_act = test_data$dem_pct_2p_offset

mae <- mean(abs(y_hat_mean - y_act))

mde <- mean(y_hat_mean - y_act) # Directional

fund_chances <- apply(y_hat, 2, \(x) mean(x > 0) * 100)

test_data <- test_data %>% mutate(
  y_pred = y_hat_mean + 50,
  y_act = dem_pct_2p,
  poster_sd = y_hat_sd
) %>% mutate(
  abs_err = abs(y_pred - y_act),
  err = y_pred - y_act,
  fund_chances = fund_chances
) %>% mutate(
  z = err / poster_sd
)

comp_mae <- mean((test_data %>% filter((y_pred >= 45 & y_pred <= 55) | (y_act >= 45 & y_act <= 55)))$abs_err)

comp_mde <- mean((test_data %>% filter((y_pred >= 45 & y_pred <= 55) | (y_act >= 45 & y_act <= 55)))$err)

ggplot() + geom_point(mapping = aes(x = y_hat_mean + 50, y = y_act + 50)) +
  labs(
    x = "Predicted values",
    y = "Actual values",
    title = "Predicted vs actual"
  ) + xlim(40, 60) + ylim(35, 65)

# loo_intsloeps <- loo(fit)

# Backtesting (2024)

pre24 <- data %>% filter(year < 2024)

data_24 <- data %>% filter(year == 2024)

backtest_model <- stan_glmer( dem_pct_2p_offset ~ 0 + baseline + sqrt_effn:baseline +
                                (1 | demo_cluster:year) +
                                funds_pct_margin + inc_dummy + polarization:funds_pct_margin + polarization:inc_dummy +
                                (1 | dem_cand) + (1 | rep_cand) + (1 | state:year) + (1 | year) +
                                #(1 | state) + #(1 | year) +
                                (1 | census_region:year) + sqrt_effn:poll_margin + 
                                dem_scandal_score + rep_scandal_score,
                                     family = gaussian(),
                                     data = pre24,
                                     prior = normal(0, 4, autoscale = TRUE),
                                     adapt_delta = 0.95,
                                     refresh = 100,
                                     iter = 1000*2,
                                     seed = 1010
)
print(backtest_model)
print(fixef(backtest_model))
print(ranef(backtest_model))

mcmc_trace(backtest_model, pars = c("pvi", "generic_ballot_avg",
                                    "dem_inc_dummy", "rep_inc_dummy"))
mcmc_trace(backtest_model, pars = c("cvap_hisp_pct", "cvap_white_pct",
                                    "cvap_black_pct", "cvap_aapi_pct"))
mcmc_dens_overlay(backtest_model, pars = c("pvi", "generic_ballot_avg",
                                           "dem_inc_dummy", "rep_inc_dummy"))
mcmc_dens_overlay(backtest_model, pars = c("cvap_hisp_pct", "cvap_white_pct",
                                           "cvap_black_pct", "cvap_aapi_pct"))
mcmc_trace(as.array(backtest_model), regex_pars = "Sigma")
mcmc_dens_overlay(as.array(backtest_model), regex_pars = "Sigma")
neff_ratio(backtest_model, pars = c("pvi", "generic_ballot_avg",
                                    "dem_inc_dummy", "rep_inc_dummy",
                                    "dem_funds_2p_pct_sqrd", #"cvap_hisp_pct", 
                                    #"cvap_white_pct",
                                    #"cvap_black_pct", "cvap_aapi_pct",
                                    "sqrt_effn:poll_margin", #"college",
                                    "dem_scandal_score", "rep_scandal_score"))
rhat(backtest_model, pars = c("pvi", "generic_ballot_avg",
                              "dem_inc_dummy", "rep_inc_dummy",
                              "dem_funds_2p_pct_sqrd",
                              "sqrt_effn:poll_margin",
                              "dem_scandal_score", "rep_scandal_score"))
neff_ratio(backtest_model, pars = c("Sigma[dem_cand:(Intercept),(Intercept)]",
                                    "Sigma[rep_cand:(Intercept),(Intercept)]",
                                    "Sigma[year:(Intercept),(Intercept)]",
                                    "Sigma[state:year:(Intercept),(Intercept)]",
                                    "Sigma[state:(Intercept),(Intercept)]",
                                    "Sigma[census_region:(Intercept),(Intercept)]",
                                    "Sigma[demo_cluster:(Intercept),(Intercept)]"))
rhat(backtest_model, pars = c("Sigma[dem_cand:(Intercept),(Intercept)]",
                                    "Sigma[rep_cand:(Intercept),(Intercept)]",
                                    "Sigma[year:(Intercept),(Intercept)]",
                                    "Sigma[state:year:(Intercept),(Intercept)]",
                                    "Sigma[state:(Intercept),(Intercept)]",
                              "Sigma[census_region:(Intercept),(Intercept)]",
                              "Sigma[demo_cluster:(Intercept),(Intercept)]"))

poster_2024 <- posterior_predict(backtest_model, newdata = data_24)

pp_check(backtest_model, nreps = 100)

y_hat_2024 <- colMeans(poster_2024)

sd_yhat_2024 <- colSds(poster_2024)

fund_chances <- apply(poster_2024, 2, \(x) mean(x > 0) * 100)

tot_seats_sims <- apply(poster_2024, 1, \(x) sum(x > 0))

y_act_2024 <- data_24$dem_pct_2p_offset

mae <- mean(abs(y_hat_2024 - y_act_2024))

data_24 <- data_24 %>% mutate(
  y_pred = y_hat_2024 + 50,
  y_act = dem_pct_2p,
  y_pred_sd = sd_yhat_2024,
  fund_chances = fund_chances
) %>% mutate(
  abs_err = abs(y_pred - y_act),
  err = y_pred - y_act
) %>% mutate(
  z = err / y_pred_sd
) %>% mutate(
  in_95_ci = if_else(abs(z) < 2, TRUE, FALSE),
  in_68_ci = if_else(abs(z) < 1, TRUE, FALSE),
  index = row_number()
) %>% mutate(
  sims = lapply(index, function(index) poster_2024[, index])
)

ggplot() + geom_point(mapping = aes(x = y_hat_2024 + 50, y = y_act_2024 + 50)) +
  labs(
    x = "Predicted values",
    y = "Actual values",
    title = "Predicted vs actual (Backtesting, 2024)"
  ) + xlim(40, 60) + ylim(35, 65)

ggplot(data = data_24, mapping = aes(x = y_pred, y = fund_chances)) + geom_point() +
  labs(
    x = "Predicted values",
    y = "Predicted chances"
  )

ggplot() + geom_histogram(mapping = aes(x = tot_seats_sims), binwidth=1)

View(data_24 %>% filter((y_act < 55 & y_act > 45) | (y_pred < 55 & y_pred > 45)) %>% select(year, district, dem_cand, rep_cand, y_pred, y_act, abs_err, fund_chances))

data_24 <- data_24 %>% mutate(district_id = str_remove_all(district, "-"))

write_csv(data_24 %>% select(-sims), "backtesting/backtesting_res_fundamentals_2024.csv")

# Backtesting (2018)

post18 <- data %>% filter(year > 2018)

data_18 <- data %>% filter(year == 2018)

backtest_model <- stan_glmer( dem_pct_2p ~ pvi + generic_ballot_avg +
                                dem_funds_2p_pct_sqrd + dem_inc_dummy + rep_inc_dummy +
                                cvap_hisp_pct + cvap_white_pct + cvap_black_pct + cvap_aapi_pct +
                                (1 | dem_cand) + (1 | rep_cand) + (1 | state) + (1 | year) +
                                (1 | state:year) + (sqrt_effn:poll_margin),
                              family = gaussian(),
                              data = post18,
                              prior = normal(0, 2.5, autoscale = TRUE),
                              adapt_delta = 0.99,
                              refresh = 100,
                              iter = 5000*2,
                              seed = 1010
)
print(backtest_model)
print(fixef(backtest_model))
print(ranef(backtest_model))

mcmc_trace(backtest_model, pars = c("pvi", "generic_ballot_avg",
                                    "dem_inc_dummy", "rep_inc_dummy"))
mcmc_trace(backtest_model, pars = c("cvap_hisp_pct", "cvap_white_pct",
                                    "cvap_black_pct", "cvap_aapi_pct"))
mcmc_dens_overlay(backtest_model, pars = c("pvi", "generic_ballot_avg",
                                           "dem_inc_dummy", "rep_inc_dummy"))
mcmc_dens_overlay(backtest_model, pars = c("cvap_hisp_pct", "cvap_white_pct",
                                           "cvap_black_pct", "cvap_aapi_pct"))
mcmc_trace(as.array(backtest_model), regex_pars = "Sigma")
mcmc_dens_overlay(as.array(backtest_model), regex_pars = "Sigma")
neff_ratio(backtest_model, pars = c("pvi", "generic_ballot_avg",
                                    "dem_inc_dummy", "rep_inc_dummy",
                                    "dem_funds_2p_pct_sqrd", "cvap_hisp_pct", 
                                    "cvap_white_pct",
                                    "cvap_black_pct", "cvap_aapi_pct"))
rhat(backtest_model, pars = c("pvi", "generic_ballot_avg",
                              "dem_inc_dummy", "rep_inc_dummy",
                              "dem_funds_2p_pct_sqrd", "cvap_hisp_pct", "cvap_white_pct",
                              "cvap_black_pct", "cvap_aapi_pct"))
neff_ratio(backtest_model, pars = c("Sigma[dem_cand:(Intercept),(Intercept)]",
                                    "Sigma[rep_cand:(Intercept),(Intercept)]",
                                    "Sigma[year:(Intercept),(Intercept)]",
                                    "Sigma[state:year:(Intercept),(Intercept)]",
                                    "Sigma[state:(Intercept),(Intercept)]"))
rhat(backtest_model, pars = c("Sigma[dem_cand:(Intercept),(Intercept)]",
                              "Sigma[rep_cand:(Intercept),(Intercept)]",
                              "Sigma[year:(Intercept),(Intercept)]",
                              "Sigma[state:year:(Intercept),(Intercept)]",
                              "Sigma[state:(Intercept),(Intercept)]"))
set.seed(42)
poster_2018 <- posterior_predict(backtest_model, newdata = data_18)

pp_check(backtest_model, nreps = 100)

y_hat_2018 <- colMeans(poster_2018)

fund_chances <- apply(poster_2018, 2, \(x) mean(x > 50) * 100)

tot_seats_sims <- apply(poster_2018, 1, \(x) sum(x > 50))

y_act_2018 <- data_18$dem_pct_2p

mae <- mean(abs(y_hat_2018 - y_act_2018))

data_18 <- data_18 %>% mutate(
  y_pred = y_hat_2018,
  y_act = dem_pct_2p,
  fund_chances = fund_chances
) %>% mutate(
  abs_err = abs(y_pred - y_act),
  err = y_pred - y_act
)

ggplot() + geom_point(mapping = aes(x = y_hat_2018, y = y_act_2018)) +
  labs(
    x = "Predicted values",
    y = "Actual values",
    title = "Predicted vs actual (Backtesting, 2018)"
  ) + xlim(40, 60) + ylim(35, 65)

ggplot() + geom_histogram(mapping = aes(x = tot_seats_sims), binwidth=1)

data_18 <- data_18 %>% mutate(district_id = str_remove_all(district, "-"))

write_csv(data_18, "backtesting/backtesting_res_fundamentals_2018.csv")
