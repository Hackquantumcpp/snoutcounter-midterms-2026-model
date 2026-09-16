library(tidyverse)
library(janitor)
library(rstan)
library(rstanarm)
library(DescTools)
library(readxl)
library(broom.mixed)
library(progressr)

options(mc.cores = parallel::detectCores(logical = FALSE))

banned_pollsters <- c("ActiVote",
                      "Trafalgar Group", 
                      "Trafalgar Group/InsiderAdvantage",
                      "Big Data Poll",
                      "National Association of Independent Pollsters",
                      "Rasmussen Reports")
## 2018-2024 Senate polling

filepath <- "data/polls/senate_polls_historical.csv"

polls <- read_csv(filepath)

polls <- polls %>% filter(!(display_name %in% banned_pollsters)) %>%
  filter(stage == "general" & hypothetical == FALSE)

polls <- polls %>% filter(
  party %in% c("DEM", "REP") | candidate_name %in% c("Angus S. King Jr.", "Dan Osborn",
                                                     "Evan McMullin", "Bernie Sanders") # 
)

polls <- polls %>% filter(
  is.na(population) == FALSE,
  is.na(end_date) == FALSE
)

tracking_polls_pipeline <- function(data_frame, cycle, state, candidate) {
  df <- data_frame %>% filter(cycle == .env$cycle,
                              state == .env$state,
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

polls_in_window <- function(data_frame, date, pid) {
  df <- data_frame # Copy data frame
  
  thres = date - 14
  df <- df %>% filter(poll_spon_id == pid & end_date >= thres)
  return(max(dim(df)[1], 1)) ## REMEMBER, IMPORTANT, NOT ZERO INDEXED IN R!!!
}

poll_avg <- function(data_frame, cycle, state, candidate) {
  # Copy data frame, filter for all those less than given date
  df_og <- data_frame
  df <- df_og %>% filter(cycle == .env$cycle,
                         state == .env$state,
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
  
  #if ((state == "Rhode Island") & (cycle == 2024)) {
  #  print("hello world")
  #}
  
  ### Sample size weights
  size_cap <- 5000
  df_nullsampsize <- df %>% filter(is.na(sample_size) == TRUE)
  
  impute_sample_size <- function(data_frame, data_frame_nullsampsize, pollster, mode, cycle) {
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
      df_cycle <- df_og %>% filter(cycle == cycle) %>% filter(is.na(sample_size) == FALSE)
      return (median(df_cycle$sample_size))
    }
  }
  
  impute_sample_size_dfnullsampsize <- function(pollster, mode, cycle) {
    return(impute_sample_size(df %>% select(pollster, mode, cycle, sample_size), df_nullsamplesize, pollster, mode, cycle))
  }
  
  df <- df %>% filter(is.na(sample_size) == FALSE) # TODO: handle null sample size polls
  df <- df %>% mutate(sample_size_winsr = pmin(sample_size, size_cap))
  df <- df %>% mutate(sample_size_winsr = Winsorize(sample_size_winsr, val = quantile(sample_size_winsr, probs = c(0.025, 0.975), na.rm = FALSE)))
  
  if (dim(df_nullsampsize)[1] != 0) {
    df_nullsampsize <- df_nullsampsize %>% rowwise() %>%
      mutate(sample_size_winsr = impute_sample_size_dfnullsampsize(pollster, mode, cycle)) %>%
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
  
  ## Multiple polls in short window weights
  df <- df %>% group_by(pollster) %>%
    mutate(poll_spon_id = cur_group_id()) %>%
    ungroup()
  df <- df %>% rowwise() %>% mutate(zone_flood_weight = 1 / sqrt(pid_in_window(end_date, poll_spon_id))) %>%
    ungroup()
  
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
  df <- df %>% mutate(total_weight = sample_size_weight * quality_weight * recency_weight * partisan_downweight * internal_downweight * zone_flood_weight)
  df$total_weight <- df$total_weight / sum(df$total_weight)
  
  return(df)
}

avg_final <- function(data_frame, cycle, state, candidate) {
  df <- data_frame
  
  df_weights <- poll_avg(data_frame, cycle, state, candidate)
  #if ((state == "Rhode Island") & (cycle == 2024)) {
  # Debug
  #View(df_weights)
  #}
  
  message(paste("Running average for", cycle, state, "SEN, Candidate:", candidate))

  if (nrow(df_weights) <= 1) {
    avg <- sum(df_weights$total_weight * df_weights$pct)
    std <- sqrt(sum(df_weights$total_weight * (df_weights$pct - avg)^2))
    lower_ci <- avg - 1.96*std
    upper_ci <- avg + 1.96*std
  }
  
  else {  
    all_cols <- c("pollster", "partisan", "population", "mode", "sponsor_candidate")
    usable_cols <- all_cols[sapply(all_cols, function(col) {
      col %in% names(df_weights) && length(unique(df_weights[[col]])) > 1
    })]
    missing_cols <- setdiff(all_cols, usable_cols) ## Misnomer, columns are not actually "missing" but only have one level
    
    date_interv <- sort(unique(df_weights$end_date))
    
    avg_oneday <- function(date) {
      df_weights_onday <- poll_avg(data_frame %>% filter(mdy(end_date) <= date), cycle, state, candidate)
      #print(paste(date, dim(data_frame %>% filter(mdy(end_date) <= date))))
      avg <- sum(df_weights_onday$total_weight * df_weights_onday$pct)
      std <- sqrt(sum(df_weights_onday$total_weight * (df_weights_onday$pct - avg)^2))
      lower_ci <- avg - 1.96*std
      upper_ci <- avg + 1.96*std
      return(list(cand_avg = avg,
                  std = std,
                  lower_ci = lower_ci,
                  upper_ci = upper_ci))
    }
    
    with_progress({
      p <- progressor(along = date_interv)
      
      df_avg <- bind_cols(
        tibble(end_date = date_interv),
        map_dfr(date_interv, function(d) {
          p()
          avg_oneday(d)
        })
      )
    })
    
    df_weights <- df_weights %>% left_join(df_avg %>% select(end_date, cand_avg), join_by(end_date)) %>% 
      mutate(partisan = coalesce(partisan, "NA"), sponsor_candidate = coalesce(sponsor_candidate, "NA"))
    
    raneff_terms <- paste0("(1 | ", usable_cols, ")" )
    formula_str <- paste("pct ~ 0 +", paste(raneff_terms, collapse = " + "), "+ cand_avg")
    
    if (length(usable_cols) == 0) {
      avg <- df_avg %>% filter(end_date == max(df_avg$end_date)) %>% pull(cand_avg)
      std <- df_avg %>% filter(end_date == max(df_avg$end_date)) %>% pull(std)
      lower_ci <- df_avg %>% filter(end_date == max(df_avg$end_date)) %>% pull(lower_ci)
      upper_ci <- df_avg %>% filter(end_date == max(df_avg$end_date)) %>% pull(upper_ci)
    }
    
    else {
    
      if (length(missing_cols) > 0) {
        message("Dropped (missing or single-level): ", paste(missing_cols, collapse = ", "))
      }
      
      fit <- stan_glmer( as.formula(formula_str),
                         family = gaussian(),
                         data = df_weights,
                         prior = normal(0, 1, autoscale = TRUE),
                         prior_covariance = decov(scale = 0.50),
                         adapt_delta = 0.99,
                         refresh = 100,
                         seed = 1010
      )
      
      tidy_raneffs <- tidy(fit, effects = "ran_vals") %>% select(group, level, estimate)
      pop_a <- tidy_raneffs %>% filter(group == 'population' & level == 'lv') %>% pull(estimate)
      np_a <- tidy_raneffs %>% filter(group == 'partisan' & level == 'NA') %>% pull(estimate)
      nospon_a <- tidy_raneffs %>% filter(group == 'sponsor_candidate' & level == 'NA') %>% pull(estimate)
      
      sign_flip_cols <- intersect(c("pollster", "mode"), usable_cols)
      other_cols <- intersect(c("population", "partisan", "sponsor_candidate"), usable_cols)
      
      adj_cols <- c() 
      for (col in sign_flip_cols) {
        
        #rel_re <- tidy_raneffs %>% filter(group == col) %>% 
        #  transmute(!!col := level, !!col_adj_name := -1 * estimate) %>%
        #  mutate(pollster = str_remove(pollster, "_"))
        if (col == "pollster") {
          raneffs = ranef(fit)$pollster
          df_weights <- df_weights %>% left_join( (rownames_to_column(raneffs)) %>% 
                                                    rename(pollster = rowname, house_effect = "(Intercept)") %>%
                                                    mutate(house_effect = -1 * house_effect), join_by(pollster))
          col_adj_name <- "house_effect"
        }
        else {
          raneffs = ranef(fit)$mode
          df_weights <- df_weights %>% left_join( (rownames_to_column(raneffs)) %>% 
                                                    rename(mode = rowname, mode_effect = "(Intercept)") %>%
                                                    mutate(mode_effect = -1 * mode_effect), join_by(mode))
          col_adj_name <- "mode_effect"
        }
        
        adj_cols <- c(adj_cols, col_adj_name)
      }
      
      for (col in other_cols) {
        col_adj_name <- paste0(col, "_adj")
        rel_re <- tidy_raneffs %>% filter(group == col) %>% 
          transmute(!!col := level, !!col_adj_name := estimate)
        df_weights <- left_join(df_weights, rel_re, by = col)
        if (col == "population") {
          df_weights <- df_weights %>% mutate(population_adj = pop_a - population_adj)
        }
        else if (col == "partisan") {
          df_weights <- df_weights %>% mutate(partisan_adj = np_a - partisan_adj)
        }
        else {
          df_weights <- df_weights %>% mutate(sponsor_candidate_adj = nospon_a - sponsor_candidate_adj)
        }
        adj_cols <- c(adj_cols, col_adj_name)
      }
      
      if (length(adj_cols) > 0) {
        df_weights <- df_weights %>%
          mutate(across(all_of(adj_cols), ~ ifelse(is.na(.x), 0, .x)))
        df_weights$tot_adj <- rowSums(df_weights[, adj_cols, drop = FALSE])
      } else {
        df_weights$tot_adj <- 0
      }
      
      df_weights <- df_weights %>% mutate(pct = pct + tot_adj)
      
      with_progress({
        p <- progressor(along = date_interv)
        
        df_avg_final <- bind_cols(
          tibble(end_date = date_interv),
          map_dfr(date_interv, function(d) {
            p()
            avg_oneday(d)
          })
        )
      })
      
      avg <- df_avg_final %>% filter(end_date == max(df_avg_final$end_date)) %>% pull(cand_avg)
      std <- df_avg_final %>% filter(end_date == max(df_avg_final$end_date)) %>% pull(std)
      lower_ci <- df_avg_final %>% filter(end_date == max(df_avg_final$end_date)) %>% pull(lower_ci)
      upper_ci <- df_avg_final %>% filter(end_date == max(df_avg_final$end_date)) %>% pull(upper_ci)
    }
    
  }
  
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
  polls %>% select(cycle, state, candidate_name, party)
)

cand_averages <- unique_cands %>% mutate(
  output = pmap(list(cycle, state, candidate_name), function(cycle, state, candidate_name) {
    return (avg_final(polls, cycle, state, candidate_name))
  })
) %>% unnest_wider(output)
