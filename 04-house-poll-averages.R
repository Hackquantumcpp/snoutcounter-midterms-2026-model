library(tidyverse)
library(janitor)
library(rstan)
library(rstanarm)

banned_pollsters <- c("ActiVote",
                      "Trafalgar Group", 
                      "Trafalgar Group/InsiderAdvantage",
                      "Big Data Poll",
                      "National Association of Independent Pollsters",
                      "Rasmussen Reports")

filepath <- "data/polls/house_polls_historical.csv"

polls <- read_csv(filepath)

polls <- polls %>% filter(!(display_name %in% banned_pollsters))
