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

## 2018-24 House polling

filepath <- "data/polls/house_polls_historical.csv"

polls <- read_csv(filepath)

polls <- polls %>% filter(!(display_name %in% banned_pollsters)) %>%
  filter(stage == "general" & hypothetical == FALSE)

polls <- polls %>% filter(
  party %in% c("DEM", "REP") | answer == "Mund" # 2022 North Dakota House
)

polls <- polls %>% filter(
  is.na(population) == FALSE,
  is.na(end_date) == FALSE
)

tracking_polls_pipeline <- function(data_frame, cycle, state,
                                    seat_number, candidate) {
  df <- data_frame %>% filter(cycle == .env$cycle,
                                state == .env$state,
                                seat_number == .env$seat_number,
                                candidate_name == candidate,
                                tracking == TRUE)
  pollsters <- as.vector(df %>% distinct(pollster))$pollster
  
  df_tracking <- tibble()
  
  for (p in pollsters) {
    df_pollst <- df %>% filter(pollster == p) %>%
      rowwise() %>%
      mutate(interval = start_date %--% end_date) %>%
      ungroup() %>%
      arrange(desc(end_date))
    
    if (dim(df_pollst)[1] == 1) {
      df_tracking <- bind_rows(df_tracking, df_pollst)
      next
    }
    
    ptr <- 1
    
    while (ptr <= dim(df_pollst)[1]) {
      interv_metric <- df_pollst$interval[ptr]
      
      df_pollst <- df_pollst %>% filter(
        interval == interv_metric | !(int_overlaps(interval, interv_metric) == TRUE)
      )
      
      ptr <- ptr + 1
    }
    
    df_tracking <- bind_rows(df_tracking, df_pollst)
  }
  
  return(df_tracking)
}
poll_avg <- function(data_frame, cycle, state, seat_number, candidate) {
  # Copy data frame, filter for all those less than given date
  df_og <- data_frame
  df <- df_og %>% filter(cycle == .env$cycle,
                           state == .env$state,
                           seat_number == .env$seat_number,
                           candidate_name == candidate)
  
  # Wrangling
  df <- df %>% arrange(pollster) %>%
    rename(mode = methodology) %>% mutate(
      mode = replace_na(mode, "Unknown")
    )
  
  df_rcv <- df %>% filter(ranked_choice_reallocated == TRUE)
  df_rcv <- df_rcv %>% ## Handling RCV polls
    arrange(desc(ranked_choice_round)) %>%
    distinct(poll_id, .keep_all = TRUE)
  
  df_fptp <- df %>% filter(ranked_choice_reallocated == FALSE)
  
  df <- df_fptp
  
  df <- df %>% mutate(
    start_date = mdy(start_date),
    end_date = mdy(end_date),
    election_date = mdy(election_date)
  )

  df <- df %>%
    mutate(population = recode(population, "LV" = "b", "RV" = "c", "A" = "e")) %>% 
    arrange(population) %>% 
    distinct(poll_id, .keep_all = TRUE) %>% 
    mutate(population = recode(population, "b" = "LV", "c" = "RV", "e" = "A"))
  
  ### Sample size weights
  size_cap <- 5000
  df_nullsampsize <- df %>% filter(is.na(sample_size) == TRUE)
  
  impute_sample_size <- function(data_frame, data_frame_nullsampsize, pollster, mode) {
    df <- data_frame # Copy data frame
    df_pollst <- df %>% filter(pollster == .env$pollster)
    df_mode <- df %>% filter(mode == .env$mode)
    
    if (nrow(df_pollst) != 0) {
      return(median(df_pollst$sample_size))
    }
    else if (nrow(df_mode) != 0) {
      return (median(df_mode$sample_size))
    }
    else if (any(!is.na(df$sample_size))) {
      return (median(df$sample_size))
    }
    else {
      df_cycle <- df_og %>% filter(cycle == cycle)
      return (median(df_cycle$sample_size))
    }
  }
  
  impute_sample_size_dfnullsampsize <- function(pollster, mode) {
    return(impute_sample_size(df %>% select(pollster, mode, sample_size), df_nullsamplesize, pollster, mode))
  }
  
  df <- df %>% filter(is.na(sample_size) == FALSE) # TODO: handle null sample size polls
  df <- df %>% mutate(sample_size_winsr = pmin(sample_size, size_cap))
  df <- df %>% mutate(sample_size_winsr = Winsorize(sample_size_winsr, val = quantile(sample_size_winsr, probs = c(0.025, 0.975), na.rm = FALSE)))
  
  if (dim(df_nullsampsize)[1] != 0) {
    df_nullsampsize <- df_nullsampsize %>% rowwise() %>%
      mutate(sample_size_winsr = impute_sample_size_dfnullsampsize(pollster, mode)) %>%
      ungroup()
    
    df <- bind_rows(df, df_nullsampsize)
  }
  
  df <- df %>% mutate(sample_size_weight = sqrt(pmin(sample_size_winsr, size_cap)) / sqrt(median(pmin(sample_size_winsr, size_cap))))
  
  ### Quality weights
  df <- df %>%
    mutate(
      pollscore = coalesce(pollscore, 1),
      # quality_weight = if_else(predictive_plus_minus < 0.5, exp(-predictive_plus_minus/1.3), 0.2)
      quality_weight = if_else(pollscore <= 1, sqrt(1/2.4 * (1 - pollscore)) + 0.2, 0.2)    
    )
  
  pid_in_window <- function(end_date, pid) {
    return(polls_in_window(df, end_date, pid))
  }
  
  ### Multiple polls in short window weights
  # df <- df %>% group_by(pollster) %>%
  #  mutate(poll_spon_id = cur_group_id()) %>%
  #  ungroup()
  # df <- df %>% rowwise() %>% mutate(zone_flood_weight = 1 / sqrt(pid_in_window(end_date, poll_spon_id))) %>%
  #  ungroup()
  
  ### Recency weight
  window <- 30
  df <- df %>% mutate(recency_weight = 0.1^(as.numeric(election_date - end_date, units = "days")/window))
  
  ## Partisan downweight
  partisan_dw <- 0.8
  df <- df %>% mutate(
    partisan_downweight = if_else(is.na(partisan), 1, partisan_dw)
  )
  
  ## Internal downweight
  internal_dw <- 0.5 / 0.8
  df <- df %>% mutate(
    internal_downweight = if_else(internal == TRUE, internal_dw, 1)
  ) %>% mutate(
    internal_downweight = replace_na(1)
  )
  
  ### Bring it all together
  df <- df %>% mutate(total_weight = sample_size_weight * quality_weight * recency_weight * partisan_downweight * internal_downweight)
  df$total_weight <- df$total_weight / sum(df$total_weight)
  
  return(df)
}

avg_final <- function(data_frame, cycle, state, seat_number, candidate) {
  df <- data_frame
  
  df_weights <- poll_avg(data_frame, cycle, state, seat_number, candidate)
  avg <- sum(df_weights$total_weight * df_weights$pct)
  std <- sqrt(sum(df_weights$total_weight * (df_weights$pct - avg)^2))
  lower_ci <- avg - 1.96*std
  upper_ci <- avg + 1.96*std
  
  df_weights <- df_weights %>% mutate(
    effn_notime = -0.3*pollscore + 1,
    time_adj = exp(-as.numeric(election_date - end_date, units = "days")/30),
    effn = effn_notime * time_adj
  ) # Measure of "effective" number of polls
  
  return(c("avg" = avg, 
           "std" = std, 
           "lower_ci" = lower_ci, 
           "upper_ci" = upper_ci,
           "effn" = sum(df_weights$effn)))
}

unique_cands <- unique(
  polls %>% select(cycle, state, seat_number, candidate_name, party)
  )

cand_averages <- unique_cands %>% mutate(
  output = pmap(list(cycle, state, seat_number, candidate_name), function(cycle, state, seat_number, candidate_name) {
    return (avg_final(polls, cycle, state, seat_number, candidate_name))
  })
) %>% unnest_wider(output)





########################## 2014-16 House polling ####################################

polls_1416 <- read_csv('transformed/polls_silver_wrangled.csv')

ratings_14 <- read_csv('data/ratings/pollster_ratings_silver.csv') %>% janitor::clean_names()

polls_1416 <- polls_1416 %>% mutate(
  internal = coalesce(if_else((sponsor == cand1_name | sponsor == cand2_name | sponsor == 'unspecified Democratic sponsor' | sponsor == 'unspecified Republican sponsor'), TRUE, FALSE), FALSE)
)

polls_1416_house <- polls_1416 %>% filter(!(pollster %in% banned_pollsters)) %>%
  filter((year %in% c(2014, 2016)) & (type_simple == 'House-G')) %>% rename(
    sample_size = samplesize
  )


poll_avg_1416 <- function(data_frame, cycle, location, candidate, cand_1or2) {
  # Copy data frame, filter for all those less than given date
  df_og <- data_frame
  if (cand_1or2 == 1) {
    df_og <- df_og %>% rename(
      candidate_name = cand1_name,
      pct = cand1_pct
    )
  }
  else {
    df_og <- df_og %>% rename(
      candidate_name = cand2_name,
      pct = cand2_pct
    )
  }
  
  df <- df_og %>% filter(year == cycle,
                         location == .env$location,
                         candidate_name == candidate)
  
  # Wrangling
  df <- df %>% mutate(
    polldate = ymd(polldate),
    electiondate = ymd(electiondate)
  )
  
  df <- df %>%
    mutate(population = recode(population, "LV" = "b", "RV" = "c", "A" = "e")) %>% 
    arrange(population) %>% 
    distinct(poll_id_nate, .keep_all = TRUE) %>% 
    mutate(population = recode(population, "b" = "LV", "c" = "RV", "e" = "A"))
  
  ### Sample size weights
  size_cap <- 5000
  df_nullsampsize <- df %>% filter(is.na(sample_size) == TRUE)
  
  impute_sample_size <- function(data_frame, data_frame_nullsampsize, pollster, mode) {
    df <- data_frame # Copy data frame
    df_pollst <- df %>% filter(pollster == .env$pollster)
    df_mode <- df %>% filter(mode == .env$mode)
    
    if (nrow(df_pollst) != 0) {
      return(median(df_pollst$sample_size))
    }
    else if (nrow(df_mode) != 0) {
      return (median(df_mode$sample_size))
    }
    else if (any(!is.na(df$sample_size))) {
      return (median(df$sample_size))
    }
    else {
      df_cycle <- df_og %>% filter(cycle == cycle)
      return (median(df_cycle$sample_size))
    }
  }
  
  impute_sample_size_dfnullsampsize <- function(pollster, mode) {
    return(impute_sample_size(df %>% select(pollster, mode, sample_size), df_nullsamplesize, pollster, mode))
  }
  
  df <- df %>% filter(is.na(sample_size) == FALSE)
  df <- df %>% mutate(sample_size_winsr = pmin(sample_size, size_cap))
  df <- df %>% mutate(sample_size_winsr = Winsorize(sample_size_winsr, val = quantile(sample_size_winsr, probs = c(0.025, 0.975), na.rm = FALSE)))
  
  if (dim(df_nullsampsize)[1] != 0) {
    df_nullsampsize <- df_nullsampsize %>% rowwise() %>%
      mutate(sample_size_winsr = impute_sample_size_dfnullsampsize(pollster, mode)) %>%
      ungroup()
    
    df <- bind_rows(df, df_nullsampsize)
  }
  
  df <- df %>% mutate(sample_size_weight = sqrt(pmin(sample_size_winsr, size_cap)) / sqrt(median(pmin(sample_size_winsr, size_cap))))
  
  ### Quality weights
  ## For 2014-16, retroactively apply 2026 pollster ratings
  df <- df %>% left_join(
    ratings_14, join_by(pollster)
  )
  df <- df %>%
    mutate(
      pollscore = coalesce(predictive_plus_minus, 1),
      # quality_weight = if_else(predictive_plus_minus < 0.5, exp(-predictive_plus_minus/1.3), 0.2)
      quality_weight = if_else(predictive_plus_minus <= 1, sqrt(1/2.4 * (1 - pollscore)) + 0.2, 0.2)    
    )
  
  #pid_in_window <- function(end_date, pid) {
  #  return(polls_in_window(df, end_date, pid))
  #}
  
  ### Multiple polls in short window weights
  # df <- df %>% group_by(pollster) %>%
  #  mutate(poll_spon_id = cur_group_id()) %>%
  #  ungroup()
  # df <- df %>% rowwise() %>% mutate(zone_flood_weight = 1 / sqrt(pid_in_window(end_date, poll_spon_id))) %>%
  #  ungroup()
  
  ### Recency weight
  window <- 30
  df <- df %>% mutate(recency_weight = 0.1^(as.numeric(electiondate - polldate, units = "days")/window))
  
  ## Partisan downweight
  partisan_dw <- 0.8
  df <- df %>% mutate(
    partisan_downweight = if_else(is.na(partisan), 1, partisan_dw)
  )
  
  ## Internal downweight
  internal_dw <- 0.5 / 0.8
  df <- df %>% mutate(
    internal_downweight = if_else(internal == TRUE, internal_dw, 1)
  ) %>% mutate(
    internal_downweight = replace_na(1)
  )
  
  ### Bring it all together
  df <- df %>% mutate(total_weight = sample_size_weight * quality_weight * recency_weight * partisan_downweight * internal_downweight)
  df$total_weight <- df$total_weight / sum(df$total_weight)
  
  return(df)
}
avg_final_1416 <- function(data_frame, year, location, candidate, cand_1or2) {
  df <- data_frame
  
  df_weights <- poll_avg_1416(data_frame, year, location, candidate, cand_1or2)
  avg <- sum(df_weights$total_weight * df_weights$pct)
  std <- sqrt(sum(df_weights$total_weight * (df_weights$pct - avg)^2))
  lower_ci <- avg - 1.96*std
  upper_ci <- avg + 1.96*std
  
  df_weights <- df_weights %>% mutate(
    effn_notime = -0.3*pollscore + 1,
    time_adj = exp(-as.numeric(electiondate - polldate, units = "days")/30),
    effn = effn_notime * time_adj
  ) # Measure of "effective" number of polls
  
  return(c("avg" = avg, 
           "std" = std, 
           "lower_ci" = lower_ci, 
           "upper_ci" = upper_ci,
           "effn" = sum(df_weights$effn)))
}

unique_cand1s_1416 <- unique(
  polls_1416_house %>% select(year, location, cand1_name, cand1_party)
)
unique_cand2s_1416 <- unique(
  polls_1416_house %>% select(year, location, cand2_name, cand2_party)
)

cand_averages_1416_cand1s <- unique_cand1s_1416 %>% mutate(
  output = pmap(list(year, location, cand1_name), function(cycle, location, candidate_name) {
    return (avg_final_1416(polls_1416_house, cycle, location, candidate_name, 1))
  })
) %>% unnest_wider(output)

cand_averages_1416_cand2s <- unique_cand2s_1416 %>% mutate(
  output = pmap(list(year, location, cand2_name), function(cycle, location, candidate_name) {
    return (avg_final_1416(polls_1416_house, cycle, location, candidate_name, 2))
  })
) %>% unnest_wider(output)

cand_averages_1416_cand1s <- cand_averages_1416_cand1s %>% rename(
  candidate_name = cand1_name,
  party = cand1_party
) 

cand_averages_1416_cand2s <- cand_averages_1416_cand2s %>% rename(
  candidate_name = cand2_name,
  party = cand2_party
) 

cand_avgs_1416 <- bind_rows(cand_averages_1416_cand1s, cand_averages_1416_cand2s)


write_csv(cand_averages, "transformed/house_polling_averages.csv")
write_csv(cand_avgs_1416, "transformed/house_polling_averages_2014-16.csv")