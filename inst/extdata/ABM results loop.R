rm(list = ls())
options(digits = 4)

## packages used
{
  
  library('dplyr')
  library('magrittr') # double-pipe operator %<>% 
  library("purrr") # rbernoulli
  library("ggplot2")
  library("fs")
  library('lemon')
  library("plotly")
  library('lubridate')
  library("pryr")
  library("useful")
  library(RColorBrewer )
  library(htmltools)
  library('readr')
  library(school.epi.abm)
  
}

list_of_factors = list(
  n_schools = 1000,
  "testing_fraction" = c(
    0,
    .25,
    1),
  
  "increase_in_accuracy_after_education" = c( .1, .5),
  
  "decrease_in_exogenous_risk_after_education" = c(.1, .5),
  
  "pr_become_educated_after_outreach" = c(0, .5, 1),
  
  "n_weeks_in_time_window" = c(8)
  
)

conditions_to_cross = 
  expand.grid(list_of_factors) %>% 
  as_tibble() %>%
  filter(
    increase_in_accuracy_after_education == decrease_in_exogenous_risk_after_education)


message("We have ", nrow(conditions_to_cross), " combinations to test.")

sim_numbers_to_run = 1:nrow(conditions_to_cross)
library('beepr')
should_beep = TRUE

sim_numbers_summary = paste(range(sim_numbers_to_run), collapse = "-")

results = NULL

for (cur_scenario in sim_numbers_to_run)
{
  
  print(unlist(conditions_to_cross[cur_scenario,]))
  
  simulation_outputs = do.call(
    run_simulation,
    conditions_to_cross[cur_scenario,])
  
  class_records = simulation_outputs$class_records
  inputs = simulation_outputs$inputs
  
  average_class_data_by_day = 
    summarize_records(class_records)
  
  cols_to_save = grep(value = TRUE, "cumulative", colnames(average_class_data_by_day ))
  cols_to_exclude = grep(value = TRUE, "today", colnames(average_class_data_by_day ))
  
  summary_columns = average_class_data_by_day %>% 
    slice_tail(n = 1) %>%
    select(-all_of(cols_to_exclude))
  
  list_inputs = sapply(F = is.list, X = inputs)
  
  inputs[list_inputs] = 
    sapply(
      X = inputs[list_inputs],
      F = function(x)
      {
        paste(x, collapse = ", ")
      }
    )
  
  input_columns = as_tibble(inputs)
  
  new_results = 
    bind_cols(
      input_columns,
      summary_columns
    )
  
  colnames(new_results) = gsub("_", " ", colnames(new_results))
  
  outcome_vars = c(
    "% of enrolled students infected since baseline (cumulative)",
    "% of enrolled students infected from school (cumulative)",
    "% of enrolled students infected outside school (cumulative)"	,
    "# schooldays quarantined per student (cumulative)",
    "% of household adults infected since baseline (cumulative)",
    "% of household adults infected by student (cumulative)"
  )
  
  digits1 = 1
  for (cur in outcome_vars)
  {
    
    new_results[paste(cur, "summary")] =
      paste(
        sep = "",
        formatC(new_results[[cur]], format = 'f', digits = digits1),
        " (",
        formatC(new_results[[paste(cur,".5% quantile", sep = "")]], format = 'f', digits = digits1),
        ", ",
        formatC(new_results[[paste(cur,".95% quantile", sep = "")]], format = "f", digits = digits1),
        ")"
        
      )
    
    new_results[paste(cur, "summary2")] =
      paste(
        sep = "",
        formatC(new_results[[cur]], format = 'f', digits = digits1),
        " (",
        formatC(new_results[[paste(cur,".2.5% quantile", sep = "")]], format = 'f', digits = digits1),
        ", ",
        formatC(new_results[[paste(cur,".97.5% quantile", sep = "")]], format = "f", digits = digits1),
        ")"
        
      )
    
  }
  
  new_results %<>% relocate(
    `% of schools with no on-campus transmissions so far`, 
    `% of schools with no detected infection clusters so far`,
    .after = last_col())
  
  results %<>% bind_rows(new_results)
  
}