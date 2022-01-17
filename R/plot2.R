plot2 = function(
  average_class_data_by_day,
  plot3_ymax = 5,
  plot4_ymax = plot3_ymax,
  plot5_ymax = 3,
  x1 = 0,
  x2 = .5,
  x3 = .5
)
{
  
  daily_infection_rates_students = c(
    "% of enrolled students newly infected today",
    "% of students newly infected from exogenous source today",
    "% of enrolled students newly infected from school today"
    
  )
  
  daily_infection_rates_hh_adults = c(
    "% of household adults newly infected today",
    "% of household adults newly infected from exogenous sources today",
    "% of household adults newly infected by student today"
  )
  
  point_and_line_vars_small = 
    c(
      # "% of enrolled students in school today",
      # "% of enrolled students symptomatic in school today",
      # "% of enrolled students uninfectious at home today",
      # "% of enrolled students at home today",
      "% of enrolled students symptomatic in school today",
      "% of enrolled students infectious at school today")
  
  point_and_line_vars_very_large =  c(
    "% of enrolled students in school today")
  
  point_and_line_vars_large = 
    c(
      "% of households with positive attestations today",
      "% of enrolled students infectious at home today",
      "% of enrolled students uninfectious at home today",
      "% of enrolled students at home today"
      # "% of enrolled students infectious at school today",
      # "% of enrolled students newly infected from school today"
    )
  point_and_line_vars = 
    c(
      daily_infection_rates_hh_adults,
      daily_infection_rates_students,
      point_and_line_vars_small,
      point_and_line_vars_large,
      point_and_line_vars_very_large
    )
  
  line_only_vars1 = 
    c(
      "% of schools with no on-campus transmissions so far",
      "% of schools with no detected infection clusters so far")
  
  line_only_vars_hh_adults = c(
    "% of household adults infected (cumulative)",
    "% of household adults infected from exogenous sources (cumulative)",
    "% of household adults infected since baseline (cumulative)",
    "% of household adults infected by student (cumulative)",
    "% of household adults infected by other household adult (cumulative)"
    
  )
  
  line_only_vars_student_infections = 
    c(
      "% of enrolled students infected (cumulative)",
      "% of enrolled students infected from exogenous sources (cumulative)",
      "% of enrolled students infected by a household adult (cumulative)",
      "% of enrolled students infected since baseline (cumulative)",
      # "% of enrolled students infected outside school (cumulative)",
      "% of enrolled students infected from school (cumulative)"
      
    )
  
  line_only_vars_quarantines = c(
    "% of classes quarantined today",
    "# outbreaks detected per school (cumulative)",
    "# schooldays quarantined per student (cumulative)"
  )
  
  cumulative_outreach_vars = c(
    "% of households contacted after positive attestations (cumulative)",
    "% of enrolled students educated about COVID safety (cumulative)",
    "% of household adults educated about COVID safety (cumulative)"
  )
  
  
  line_only_vars = c(
    line_only_vars1,
    line_only_vars_hh_adults,
    line_only_vars_student_infections,
    line_only_vars_quarantines,
    cumulative_outreach_vars
  )
  
  for (cur_var in c(point_and_line_vars, line_only_vars))
  {
    
    average_class_data_by_day[[paste(cur_var, "text")]] = 
      paste(sep = "", 
            "Date: ", average_class_data_by_day[["date"]], 
            "<br>",
            cur_var, ": ", formatC(average_class_data_by_day[[cur_var]], digits = 3, format = "f"))
    
  }
  plot2 = 
    plot_ly(
      average_class_data_by_day, 
      x = ~date 
      # y = build.formula(lhs = NULL, rhs = var),
    ) %>%
    layout(
      # legend = list(orientation = 'h', y = 0),
      # hoverlabel = list(namelength = -1),
      yaxis = list(
        # title = "Daily outcomes",
        title = "",
        rangemode = "tozero"),
      # title = "Other daily outcomes",
      xaxis = list(
        title = "",
        range = range(average_class_data_by_day$date)))
  
  
  for (cur_var in point_and_line_vars_large)
  {
    
    plot2 %<>%
      add_trace(
        # legendgroup = 'group2', #  Traces part of the same legend group hide/show at the same time when toggling legend items.
        # https://plotly.com/r/reference/#layout-legend
        
        text = build.formula(lhs = NULL, rhs = paste(cur_var, "text")),
        type = "scatter", 
        mode = "lines+markers",
        y = build.formula(lhs = NULL, rhs = cur_var),
        name = cur_var
      )
  }
  
    plot2  %<>% layout(
      title = "Student quarantine rates")
    
    
  legendstats <- list(
    # x = 0.1, 
    y = -.25,
    orientation = 'h',
    font = list(
      family = "sans-serif",
      size = 12,
      color = "#000")
    # bgcolor = "#E2E2E2",
    # bordercolor = "#FFFFFF",
    # borderwidth = 2
  )
  plot2 %<>%
    style(hoverinfo = "text") %>% 
    layout(legend = legendstats)

  return(plot2)
  
}