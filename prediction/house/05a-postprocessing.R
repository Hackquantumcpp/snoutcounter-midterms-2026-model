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

saveRDS(tot_seats_sims, "model_output/tot_seats_sims.RDS")

## Time series (model output over time)

run_date <- today()

#output <- tibble(
#     date = as.Date(character()), y = double(), geo = character(), type = character()
#) ## Initial setup

output <- read_csv("model_output/output_over_time.csv") # Update output_over_time.csv

if (run_date %in% output$date) {
  output <- output %>% filter(date != run_date)
}

output <- output %>% add_row(date = run_date, y = mean_seats_tot, geo = "US House", type = "seats") %>%
  add_row(date = run_date, y = chamber_win_chance, geo = "US House", type = "chance") %>%
  add_row(date = run_date, y = sd_seats, geo = "US House", type = "seats_sd")

seat_level_out <- data %>% select(cd, y_pred, y_pred_sd, chance, ci_low, ci_hi) %>% mutate(
  date = run_date
) %>% pivot_longer(
  cols = c(y_pred, y_pred_sd, chance, ci_low, ci_hi),
  names_to = "type",
  values_to = "y"
) %>% rename(geo = cd)

output <- bind_rows(output, seat_level_out)

write_csv(output, "model_output/output_over_time.csv")

## Tipping point calculation

postibble <- as_tibble(posterior)

colnames(postibble) <- data$cd

saveRDS(postibble, "model_output/labeled_posterior.RDS")

#get_tipping_point <- function(sim_vector) {
  ## sim_vector is a vector, presumably a row vector from posterior prediction matrix,
  ## representing one full simulation of the House elections

#  dem_winner <- sapply(sim_vector, \(x) x > 0)
#  seats <- sum(dem_winner)
  
#  if (seats > thres) { # Democrats win in this sim
#    sorted_vec <- sort(sim_vector, decreasing = FALSE)
    
#    winvec <- sorted_vec[sorted_vec > 0]
#    seat_margin <- seats - thres
#  }
#  else { # Republicans win in this sim
#    sorted_vec <- sort(sim_vector, decreasing = TRUE)
    
#    winvec <- sorted_vec[sorted_vec < 0]
#    rep_seats <- dim(data)[1] - seats
#    seat_margin <- rep_seats - (218 - dim(rep_uncont)[1])
#  }
#  return(winvec[seat_margin])
#}

#View(postibble %>% rowwise() %>% mutate(
#  tipping_point_offset = list(get_tipping_point(c_across(everything())))
#))
