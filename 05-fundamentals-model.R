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

train_data <- data %>% sample_frac(0.67)

test_data <- anti_join(data, train_data, by=c("year", "state_po", "district"))

fit <- stan_glmer( dem_pct_2p ~ pvi + generic_ballot_avg +
                     dem_funds_2p_pct_sqrd + dem_inc_dummy + rep_inc_dummy +
                     (1 | dem_cand) + (1 | rep_cand) + (1 | state) + factor(year),
                   family = gaussian(),
                   data = train_data,
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

# Validation

y_hat <- posterior_predict(fit, newdata = test_data)

pp_check(fit, nreps = 50)

y_hat_mean <- colMeans(y_hat)

y_act = test_data$dem_pct_2p

mae <- mean(abs(y_hat_mean - y_act))

mde <- mean(y_hat_mean - y_act) # Directional

test_data <- test_data %>% mutate(
  y_pred = y_hat_mean,
  y_act = dem_pct_2p
) %>% mutate(
  abs_err = abs(y_pred - y_act)
)

ggplot() + geom_point(mapping = aes(x = y_hat_mean, y = y_act)) +
  labs(
    x = "Predicted values",
    y = "Actual values",
    title = "Predicted vs actual"
  ) + xlim(40, 60) + ylim(35, 65)


# Backtesting

pre24 <- data %>% filter(year < 2024)

data_24 <- data %>% filter(year == 2024)

backtest_model <- stan_glmer( dem_pct_2p ~ pvi + generic_ballot_avg +
                                       dem_funds_2p_pct_sqrd + dem_inc_dummy + rep_inc_dummy +
                                       (1 | dem_cand) + (1 | rep_cand) + (1 | state) + (1 | year),
                                     family = gaussian(),
                                     data = pre24,
                                     prior = normal(0, 1, autoscale = TRUE),
                                     adapt_delta = 0.99,
                                     refresh = 100,
                                     iter = 5000*2,
                                     seed = 1010
)
print(backtest_model)

poster_2024 <- posterior_predict(backtest_model, newdata = data_24)

pp_check(backtest_model, nreps = 100)

y_hat_2024 <- colMeans(poster_2024)

fund_chances <- apply(poster_2024, 2, \(x) mean(x > 50) * 100)

tot_seats_sims <- apply(poster_2024, 1, \(x) sum(x > 50))

y_act_2024 <- data_24$dem_pct_2p

mae <- mean(abs(y_hat_2024 - y_act_2024))

data_24 <- data_24 %>% mutate(
  y_pred = y_hat_2024,
  y_act = dem_pct_2p,
  fund_chances = fund_chances
) %>% mutate(
  abs_err = abs(y_pred - y_act)
)

ggplot() + geom_point(mapping = aes(x = y_hat_2024, y = y_act_2024)) +
  labs(
    x = "Predicted values",
    y = "Actual values",
    title = "Predicted vs actual (Backtesting, 2024)"
  ) + xlim(40, 60) + ylim(35, 65)

ggplot() + geom_histogram(mapping = aes(x = tot_seats_sims), binwidth=1)
