library(tidyverse)
library(janitor)
library(rstan)
library(rstanarm)
library(DescTools)

banned_pollsters <- c("ActiVote",
                      "Trafalgar Group", 
                      "Trafalgar Group/InsiderAdvantage",
                      "Big Data Poll",
                      "National Association of Independent Pollsters",
                      "Rasmussen Reports")

url <- "https://www.nytimes.com/newsgraphics/polls/house.csv"

polls <- read_csv(url)

setwd("../../")
write_csv(polls, "2026_data/polls/house.csv")
setwd("prediction/house")
