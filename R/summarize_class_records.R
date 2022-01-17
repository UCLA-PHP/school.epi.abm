#' Title
#'
#' @param class_records 
#' @param label_columns 
#' @param summary_grouping_columns 
#' @param school_records_by_date 
#'
#' @return
#' @export
#'

summarize_records = function(
  class_records,
  label_columns = c(
    "date",
    "day_of_week",
    "school_day",
    "surveillance testing today",
    # "n_students_on_roster",
    "school",
    "class"),
  summary_grouping_columns = c(
    "date",
    "school_day"
  ),
  school_records_by_date = 
    class_records %>% 
    group_by(date, school_day, school) %>%
    summarize(
      .groups = "drop",
      "n_classes_in_school" = n(),
      "n_classes_quarantined_today" = sum(class_quarantined_today),
      
      across(
        any_of(
          c(
            "n_students_quarantined_today",
            "n households associated with class",
            "n households with positive attestations today",
            "n households that have received outreach",
            "n_household_adults_associated_with_class",
            "n household adults educated about COVID safety",
            "n household adults newly infected today",
            "n household adults infected from exogenous source today",
            "n household adults infected by student today",
            "n household adults infected (cumulative)",
            "n household adults infected from exogenous sources (cumulative)",
            "n household adults infected since baseline (cumulative)",
            "n household adults infected by student (cumulative)",
            "n household adults infected by other household adult (cumulative)",
            "n_symptomatic_students_in_classroom_today",
            "n_students_in_attendance_today",
            "n_students_on_roster",
            "n students newly infected today",
            "n students educated about COVID safety (cumulative)",
            "n_students_infected_cumulative",
            "n_students_infected_since_baseline_cumulative",
            "n students infected from exogenous sources (cumulative)",
            "n students infected by household adult (cumulative)",
            "n_students_infected_outside_school_cumulative",
            "n_transmissions_in_class_cumulative",
            "n_outbreaks",
            "n_initially_uninfected_students_in_classroom_today",
            "n_transmissions_in_class_today",
            "n students newly infected from exogenous source today",
            "n_infectious_students_in_classroom_today",
            "n_students_at_home_today",
            "n_uninfectious_students_at_home_today",
            "n_infectious_students_at_home_today",
            "n_initially_infected_students_in_classroom_today"
          )
        ),
        sum
      )
    )
)
{
  
  
  school_records_by_date %<>%
    group_by(school) %>%
    arrange(date) %>%
    mutate(
      
      "n student-days missed (cumulative)" = 
        cumsum(n_students_quarantined_today * school_day),
      
      "% of households contacted after positive attestations (cumulative)" =
        `n households that have received outreach` / `n households associated with class` * 100,
      
      "% of households with positive attestations today" = 
        `n households with positive attestations today` / `n households associated with class` * 100,
      
      "% of household adults newly infected today" = 
        `n household adults newly infected today`/ n_household_adults_associated_with_class * 100,
      
      "% of household adults educated about COVID safety (cumulative)" = 
        `n household adults educated about COVID safety` / n_household_adults_associated_with_class * 100,
      
      "% of household adults newly infected from exogenous sources today" = 
        `n household adults infected from exogenous source today`/ n_household_adults_associated_with_class * 100,
      
      "% of household adults newly infected by student today" = 
        `n household adults infected by student today`/ n_household_adults_associated_with_class * 100,
      
      
      "% of enrolled students in school today" = 
        (n_students_in_attendance_today/n_students_on_roster) * 100,
      
      # "% of students in school today.CL" = 
      #   t.test(n_students_in_attendance_today/n_students_on_roster)$conf.int[1]*100,
      # 
      # "% of students in school today.CU" = 
      #   t.test(n_students_in_attendance_today/n_students_on_roster)$conf.int[2]*100,
      
      "% of enrolled students symptomatic in school today" = 
        (n_symptomatic_students_in_classroom_today / n_students_on_roster) * 100,
      
      "% of classes quarantined today" = 
        (n_classes_quarantined_today / n_classes_in_school) * 100,
      
      "% of household adults infected (cumulative)" = 
        `n household adults infected (cumulative)` / n_household_adults_associated_with_class * 100,
      
      "% of household adults infected since baseline (cumulative)" = 
        `n household adults infected since baseline (cumulative)` / n_household_adults_associated_with_class * 100,
      
      "% of household adults infected from exogenous sources (cumulative)" = 
        `n household adults infected from exogenous sources (cumulative)` / n_household_adults_associated_with_class * 100,
      
      "% of household adults infected by student (cumulative)" = 
        `n household adults infected by student (cumulative)` / n_household_adults_associated_with_class * 100,
      
      "% of household adults infected by other household adult (cumulative)" = 
        `n household adults infected by other household adult (cumulative)` / n_household_adults_associated_with_class * 100,
      
      
      "% of enrolled students educated about COVID safety (cumulative)" = 
        `n students educated about COVID safety (cumulative)` / n_students_on_roster * 100,
      
      "% of enrolled students infected (cumulative)" = 
        (n_students_infected_cumulative/n_students_on_roster) * 100,
      
      "% of enrolled students infected since baseline (cumulative)" = 
        (n_students_infected_since_baseline_cumulative/n_students_on_roster) * 100,
      
      "% of enrolled students infected from exogenous sources (cumulative)" = 
        (`n students infected from exogenous sources (cumulative)`/n_students_on_roster) * 100,
      
      "% of enrolled students infected by a household adult (cumulative)" = 
        (`n students infected by household adult (cumulative)`/n_students_on_roster) * 100,
      
      "% of enrolled students infected outside school (cumulative)" = 
        (n_students_infected_outside_school_cumulative / n_students_on_roster) * 100,
      
      "% of enrolled students infected from school (cumulative)" = 
        (n_transmissions_in_class_cumulative / n_students_on_roster) * 100,
      "# schooldays quarantined per student (cumulative)" = 
        (`n student-days missed (cumulative)`/n_students_on_roster),
      "# initially uninfected students in classroom today" = 
        (n_initially_uninfected_students_in_classroom_today),
      "# outbreaks detected per school (cumulative)" = 
        n_outbreaks,
      "% of enrolled students newly infected today" = 
        100 * `n students newly infected today` / n_students_on_roster,
      
      "% of students newly infected from exogenous source today" = 
        `n students newly infected from exogenous source today` / n_students_on_roster * 100,
      
      "% of enrolled students newly infected from school today" = 
        100 * 
        if_else(
          condition = school_day,
          true = n_transmissions_in_class_today / n_students_on_roster,
          false = as.numeric(NA)),
      "% attendees infectious today" = 
        100 * (n_infectious_students_in_classroom_today / n_students_in_attendance_today),
      
      "% of enrolled students infectious at school today" = 
        if_else(
          school_day,
          (n_infectious_students_in_classroom_today / n_students_on_roster) * 100,
          as.numeric(NA)),
      
      "% of enrolled students at home today" = 
        if_else(
          school_day,
          (n_students_at_home_today / n_students_on_roster) * 100,
          as.numeric(NA)),
      
      "% of enrolled students uninfectious at home today" = 
        if_else(
          school_day,
          (n_uninfectious_students_at_home_today / n_students_on_roster) * 100,
          as.numeric(NA)),
      
      "% of enrolled students infectious at home today" = 
        if_else(
          school_day,
          (n_infectious_students_at_home_today / n_students_on_roster) * 100,
          as.numeric(NA)),
      
      "% classrooms with 3+ infected attendees today today" = 
        (n_initially_infected_students_in_classroom_today >= 3) * 100)  %>%
    
    mutate(
      
      "% of enrolled students symptomatic in school today" = 
        if_else(
          school_day,
          `% of enrolled students symptomatic in school today`,
          as.numeric(NA)
        ),
      `% of classes quarantined today` = 
        if_else(
          date >= min(date[school_day]), # want this one to not have gaps
          `% of classes quarantined today`,
          as.numeric(NA)
        ),
      
      "% of enrolled students infected from school (cumulative)" = 
        if_else(
          date >= min(date[school_day]), # want this one to not have gaps
          `% of enrolled students infected from school (cumulative)`,
          as.numeric(NA)
        ),
      "# schooldays quarantined per student (cumulative)"  = 
        if_else(
          date >= min(date[school_day]), # want this one to not have gaps
          `# schooldays quarantined per student (cumulative)`,
          as.numeric(NA)
        ),
      "% of enrolled students in school today" = 
        if_else(
          school_day, 
          `% of enrolled students in school today`, 
          as.numeric(NA))) 
  
  columns_to_summarize = setdiff(
    names(school_records_by_date),
    c(
      label_columns,
      "n_classes_in_school",
      grep("date", value = TRUE, names(school_records_by_date))))
  
  average_class_data_by_day = 
    school_records_by_date %>%
    group_by_at(c(summary_grouping_columns, "school")) %>%
    summarize(
      .groups = "drop",
      across(
        any_of(columns_to_summarize) & (where(is.numeric) | where(is.logical)),
        mean)) %>%
    group_by_at(summary_grouping_columns) %>%
    summarize(
      .groups = "drop",
      
      "% of schools with no on-campus transmissions so far" = 
        mean(n_transmissions_in_class_cumulative == 0) * 100,
      
      "% of schools with no detected infection clusters so far" = 
        mean(n_outbreaks == 0) * 100,
      
      across(
        any_of(columns_to_summarize) & where(is.numeric),
        list(
          mean = mean,
          sd = sd,
          "5% quantile" = function(x)
          {
            test1 = try(quantile(x, p = .05), silent = TRUE)
            if(!inherits(test1, "try-error") & sum(!is.na(x)) > 3)
            {
              return(test1)
            } else return(NA)
          },
          "95% quantile" = function(x)
          {
            test1 = try(quantile(x, p = .95), silent = TRUE)
            if(!inherits(test1, "try-error") & sum(!is.na(x)) > 3)
            {
              return(test1)
            } else return(NA)
          },
          "2.5% quantile" = function(x)
          {
            test1 = try(quantile(x, p = .025), silent = TRUE)
            if(!inherits(test1, "try-error") & sum(!is.na(x)) > 3)
            {
              return(test1)
            } else return(NA)
          },
          "97.5% quantile" = function(x)
          {
            test1 = try(quantile(x, p = .975), silent = TRUE)
            if(!inherits(test1, "try-error") & sum(!is.na(x)) > 3)
            {
              return(test1)
            } else return(NA)
          },
          "95% CI low" =
            function(x)
            {
              
              test1 = try(t.test(x, na.rm = TRUE), silent = TRUE)
              
              if(!inherits(test1, "try-error") & sum(!is.na(x)) > 3)
              {
                return(test1$conf.int[1])
              } else return(NA)
            },
          "95% CI high" =
            function(x)
            {
              test1 = try(t.test(x, na.rm = TRUE), silent = TRUE)
              if(!inherits(test1, "try-error") & sum(!is.na(x)) > 3)
              {
                return(test1$conf.int[2])
              } else return(NA)
            }
        ),
        .names = "{col}.{fn}"))
  
  average_class_data_by_day %<>%
    mutate(
      "% of schools with no detected infection clusters so far" = 
        if_else(
          date >= min(date[school_day]) - days(1),
          `% of schools with no detected infection clusters so far`,
          as.numeric(NA)),
      "% of schools with no on-campus transmissions so far" = 
        if_else(
          date >= min(date[school_day]) - days(1),
          `% of schools with no on-campus transmissions so far`,
          as.numeric(NA)),
      across(where(is.numeric), signif, digits = 4))
  
  names(average_class_data_by_day) = 
    sub(
      ".mean$",
      "",
      names(average_class_data_by_day))
  
  if(nrow(average_class_data_by_day) != length(unique(class_records$date)))
    browser(message("Error in summarize_records()."))
  
  return(average_class_data_by_day)
  
}
