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
  poll_margin = rep_poll_avg - dem_poll_avg # Keep consistency in convention
)

model <- readRDS('../../model/house_model.RDS')

posterior <- posterior_predict(model, newdata = data)

