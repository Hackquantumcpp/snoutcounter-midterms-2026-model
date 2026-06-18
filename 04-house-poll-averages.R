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
  
  df_fptp <- df %>% filter(ranked_choice_reallocated == FALSE)
  
  df_rcv <- df_rcv %>% ## Handling RCV polls
    arrange(desc(ranked_choice_round)) %>%
    distinct(poll_id, .keep_all = TRUE)
  
  df <- bind_rows(df_fptp, df_rcv)
  
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
  df <- df %>%
    mutate(
      pollscore = coalesce(pollscore, 5),
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
  
  ### Bring it all together
  df <- df %>% mutate(total_weight = sample_size_weight * quality_weight * recency_weight * partisan_downweight)
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
  
  return(c("avg" = avg, 
           "std" = std, 
           "lower_ci" = lower_ci, 
           "upper_ci" = upper_ci))
}

unique_cands <- unique(
  polls %>% select(cycle, state, seat_number, candidate_name)
  )

cand_averages <- unique_cands %>% mutate(
  output = pmap(list(cycle, state, seat_number, candidate_name), function(cycle, state, seat_number, candidate_name) {
    return (avg_final(polls, cycle, state, seat_number, candidate_name))
  })
) %>% unnest_wider(output)
                          