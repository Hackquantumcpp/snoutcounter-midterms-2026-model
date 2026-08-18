library(tidyverse)
library(janitor)
library(rstan)
library(rstanarm)
library(bayesplot)
library(matrixStats)

options(mc.cores = parallel::detectCores(logical = FALSE))

data <- read_csv("transformed/2026_house_prediction_dataset.csv")

data <- data %>% mutate(
  dem_funds_2p_pct_sqrd = dem_funds_2p_pct**2,
  effn = pmax(dem_effn, rep_effn),
  sqrt_effn = sqrt(effn),
  poll_margin = rep_poll_avg - dem_poll_avg, # Keep consistency in convention
  baseline = 2*pvi - generic_ballot_avg,
  dem_funds_2p_pct_offset = dem_funds_2p_pct - 50,
  inc_dummy = dem_inc_dummy - rep_inc_dummy,
  funds_pct_margin = if_else(dem_funds + rep_funds == 0, 0, (dem_funds - rep_funds) / (dem_funds + rep_funds)) * 100
)

model <- readRDS('../../model/house_model.RDS')

set.seed(42)
posterior <- posterior_predict(model, newdata = data)

y_hat <- colMeans(posterior) + 50

sd_yhat <- colSds(posterior)

fund_chances <- apply(posterior, 2, \(x) mean(x > 0) * 100)

tot_seats_sims <- apply(posterior, 1, \(x) sum(x > 0))

data <- data %>% mutate(
  y_pred = y_hat,
  y_pred_sd = sd_yhat,
  chance = fund_chances,
  index = row_number(),
  sims = lapply(index, function(index) posterior[, index])
)

posinterv <- as_tibble(posterior_interval(posterior, prob = 0.95))

posinterv <- posinterv %>% janitor::clean_names() %>% rename(
  low = x2_5_percent,
  hi = x97_5_percent
)

data <- data %>% mutate(
  ci_low = posinterv$low + 50,
  ci_hi = posinterv$hi + 50
)

ggplot(data = data, mapping = aes(x = y_pred, y = chance)) + geom_point() +
  labs(
    x = "Predicted values",
    y = "Predicted chances"
  )

ggplot() + geom_histogram(mapping = aes(x = tot_seats_sims), binwidth=1)

saveRDS(data, '../../model_output/house_predictions.RDS')
saveRDS(posterior, '../../model_output/house_posterior.RDS')