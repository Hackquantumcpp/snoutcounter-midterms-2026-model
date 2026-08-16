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

polls <- read_csv("transformed/relevent_house_polls.csv")

polls <- polls %>% filter(!(display_name %in% banned_pollsters)) %>%
  filter((state != 'US') & (stage == 'general'))

polls <- polls %>% filter(
  is.na(population) == FALSE,
  is.na(end_date) == FALSE
)

ratings_25 <- read_csv('../../2026_data/ratings/pollster_ratings_silver.csv') %>% janitor::clean_names()

ratings_24 <- read_csv('../../2026_data/ratings/pollster_ratings_silver_2024.csv') %>%
  janitor::clean_names()


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
poll_avg <- function(data_frame, state, seat_number, candidate) {
  # Copy data frame, filter for all those less than given date
  df_og <- data_frame
  df <- df_og %>% filter(state == .env$state,
                         seat_number == .env$seat_number,
                         candidate_name == candidate)
  
  # Wrangling
  df <- df %>% arrange(pollster) %>%
    rename(mode = methodology) %>% mutate(
      mode = replace_na(mode, "Unknown")
    )
  
  # df_rcv <- df %>% filter(ranked_choice_reallocated == TRUE)
  # df_rcv <- df_rcv %>% ## Handling RCV polls
  #  arrange(desc(ranked_choice_round)) %>%
  #  distinct(poll_id, .keep_all = TRUE)
  
  # df_fptp <- df %>% filter(ranked_choice_reallocated == FALSE)
  
  # df <- df_fptp
  
  df <- df %>% mutate(
    start_date = mdy(start_date),
    end_date = mdy(end_date),
    election_date = ymd(election_date)
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
  
  df <- df %>% mutate(
    pollster_ratname = recode(
      display_name,
      "Quantus Insights" = "Quantus Polls and News",
      "St. Anselm" = "Saint Anselm College"
    )
  )
  
  df_25 <- df %>% filter(end_date < ymd("2026-01-14")) %>% left_join(ratings_24 %>% rename(pollster_ratname = pollster),
                                                                     join_by(pollster_ratname))
  df_26 <- df %>% filter(end_date >= ymd("2026-01-14")) %>% left_join(ratings_25 %>% rename(pollster_ratname = pollster),
                                                                      join_by(pollster_ratname))
  df <- bind_rows(df_25, df_26)
  
  df <- df %>%
    filter(
      !(pollster_ratname %in% (ratings_25 %>% filter(grade == "F@@16") %>% select(pollster)))
    ) %>%
    mutate(
      predictive_plus_minus = coalesce(predictive_plus_minus, 1),
      # quality_weight = if_else(predictive_plus_minus < 0.5, exp(-predictive_plus_minus/1.3), 0.2)
      quality_weight = if_else(predictive_plus_minus <= 1, sqrt(1/2.4 * (1 - predictive_plus_minus)) + 0.2, 0.2)    
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
avg_final <- function(data_frame, state, seat_number, candidate) {
  df <- data_frame
  
  df_weights <- poll_avg(data_frame, state, seat_number, candidate)
  #if (state == "PA" & seat_number == 7) {
  #  View(df_weights)
  #}
  avg <- sum(df_weights$total_weight * df_weights$pct)
  std <- sqrt(sum(df_weights$total_weight * (df_weights$pct - avg)^2))
  lower_ci <- avg - 1.96*std
  upper_ci <- avg + 1.96*std
  
  df_weights <- df_weights %>% mutate(
    effn_notime = -0.3*predictive_plus_minus + 1,
    time_adj = exp(-as.numeric(today() - end_date, units = "days")/30),
    effn = effn_notime * time_adj
  ) # Measure of "effective" number of polls
  
  return(c("avg" = avg, 
           "std" = std, 
           "lower_ci" = lower_ci, 
           "upper_ci" = upper_ci,
           "effn" = sum(df_weights$effn)))
}

unique_cands <- unique(
  polls %>% select(state, seat_number, candidate_name, party)
) %>% arrange(state)

cand_averages <- unique_cands %>% mutate(
  output = pmap(list(state, seat_number, candidate_name), function(state, seat_number, candidate_name) {
    return (avg_final(polls, state, seat_number, candidate_name))
  })
) %>% unnest_wider(output)

write_csv(cand_averages, "transformed/house_polling_averages.csv")