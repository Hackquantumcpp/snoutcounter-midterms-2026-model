library(tidyverse)
library(janitor)
library(rstan)
library(rstanarm)
library(DescTools)
library(readxl)

banned_pollsters <- c("ActiVote",
                      "Trafalgar Group", 
                      "Trafalgar Group/InsiderAdvantage",
                      "Big Data Poll",
                      "National Association of Independent Pollsters",
                      "Rasmussen Reports")
## 2018-2024 Senate polling

