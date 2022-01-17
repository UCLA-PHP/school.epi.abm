#' Title
#'
#' @param simulation_outputs 
#'
#' @return
#' @export
#'
analyze_results = function(simulation_outputs)
{
  
  class_records = simulation_outputs$class_records
  average_class_data_by_day = summarize_records(class_records)
  inputs = simulation_outputs$inputs
  
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
  
  return(new_results)
}