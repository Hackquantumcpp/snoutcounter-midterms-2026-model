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

polls <- polls %>% filter(!(display_name %in% banned_pollsters)) %>%
  filter((state != 'US') & (stage == 'general'))

polls_pivot <- polls %>% pivot_wider(
  id_cols = c(poll_id, question_id),
  names_from = candidate_name,
  values_from = pct
)

# All candidates who are confirmed to be competing in general
confirmed_cands <- c("Anthony Constantino", "Blake Gendebien", "Cory Mills",
                     )
