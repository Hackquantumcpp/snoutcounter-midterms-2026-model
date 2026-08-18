library(tidyverse)
library(janitor)
library(rstan)
library(rstanarm)
library(matrixStats)
library(broom.mixed)

data <- readRDS("../../model_output/house_predictions.RDS")

posterior <- readRDS("../../model_output/house_posterior.RDS")

dem_uncont <- read_csv('transformed/dem_uncontested_seats.csv')
rep_uncont <- read_csv('transformed/rep_uncontested_seats.csv')

thres <- 218 - dim(dem_uncont)[1] # Necessary contested seats that Dems must win to win House

tot_seats_sims <- apply(posterior, 1, \(x) sum(x > 0))

mean_seats <- mean(tot_seats_sims)

sd_seats <- sd(tot_seats_sims)

mean_seats_tot <- mean_seats + as.numeric(dim(dem_uncont)[1])

chamber_win_chance <- mean(tot_seats_sims > thres) * 100

## Time series (model output over time)

run_date <- today()

output <- tibble(
     date = as.Date(character()), y = double(), geo = character(), type = character()
) ## Initial setup

output <- output %>% add_row(date = run_date, y = mean_seats_tot, geo = "US House", type = "seats") %>%
  add_row(date = run_date, y = chamber_win_chance, geo = "US House", type = "chance") %>%
  add_row(date = run_date, y = sd_seats, geo = "US House", type = "seats_sd")

seat_level_out <- data %>% select(cd, y_pred, y_pred_sd, chance) %>% mutate(
  date = run_date
) %>% pivot_longer(
  cols = c(y_pred, y_pred_sd, chance),
  names_to = "type",
  values_to = "y"
) %>% rename(geo = cd)

output <- bind_rows(output, seat_level_out)

write_csv(output, "model_output/output_over_time.csv")
