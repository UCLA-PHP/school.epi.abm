#' Title
#'
#' @param class_records 
#' @param average_class_data_by_day 
#' @param plot3_ymax 
#' @param plot4_ymax 
#' @param plot5_ymax 
#' @param x1 
#' @param x2 
#' @param x3 
#'
#' @return
#' @export
#'
plot1_plotly = function(
  class_records, 
  average_class_data_by_day = summarize_records(class_records),
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
  plot8 = plot7 = plot6 = plot2 = 
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
  
  cur_var = point_and_line_vars_very_large
  
  plot1 = 
    plot2 %>%
    add_trace(
      # legendgroup = 'group1',
      
      text = build.formula(lhs = NULL, rhs = paste(cur_var, "text")),
      type = "scatter", 
      mode = "lines+markers",
      y = build.formula(lhs = NULL, rhs = cur_var),
      name = cur_var
    ) %>%
    layout(
      # title = cur_var,
      yaxis = list(
        # title = cur_var,
        title = "",
        rangemode = "tozero")) %>%
    layout(showlegend = TRUE)
  
  for (cur_var in line_only_vars1)
  {
    
    plot1 %<>%
      add_trace(
        # legendgroup = 'group1',
        
        text = build.formula(lhs = NULL, rhs = paste(cur_var, "text")),
        type = "scatter", 
        mode = "lines",
        y = build.formula(lhs = NULL, rhs = cur_var),
        name = cur_var
      )
    
  }
  
  
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
  
  for (cur_var in point_and_line_vars_small)
  {
    
    plot6 %<>%
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
  
  for (cur_var in daily_infection_rates_students)
  {
    
    plot7 %<>%
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
  
  for (cur_var in daily_infection_rates_hh_adults)
  {
    
    plot8 %<>%
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
  
  plot3_ymax2 = max(plot3_ymax, max(average_class_data_by_day[, line_only_vars_student_infections], na.rm = TRUE), na.rm = TRUE) * 1.1
  plot4_ymax2 = max(plot4_ymax, max(average_class_data_by_day[, line_only_vars_hh_adults], na.rm = TRUE), na.rm = TRUE) * 1.1
  plot5_ymax2 = max(plot5_ymax, max(average_class_data_by_day[, line_only_vars_quarantines], na.rm = TRUE), na.rm = TRUE) * 1.1
  cumulative_outreach_vars_ymax2 = max(plot5_ymax, max(average_class_data_by_day[, cumulative_outreach_vars], na.rm = TRUE), na.rm = TRUE) * 1.1
  cumulative_outreach_vars_plot = plot5 = plot4 = plot3  = 
    plot_ly(
      
      average_class_data_by_day, 
      x = ~date 
      # y = build.formula(lhs = NULL, rhs = var),
    ) %>%
    layout(
      # legend = list(orientation = 'h', y = 0),
      # hoverlabel = list(namelength = -1),
      # yaxis = list(title = "Cumulative outcomes"),
      # title = "Key outcomes",
      yaxis = list(
        #   # title = "Percentage of enrolled students",
        # 
        # range = c(0, plot3_ymax2),  
        # rangemode = "tozero"
        title = ""),
      xaxis = list(
        title = "",
        range = range(average_class_data_by_day$date)))
  
  plot3 %<>% layout(yaxis = list(range = c(0, plot3_ymax2)))
  plot4 %<>% layout(yaxis = list(range = c(0, plot4_ymax2)))
  plot5 %<>% layout(yaxis = list(range = c(0, plot5_ymax2)))
  cumulative_outreach_vars_plot %<>% layout(yaxis = list(range = c(0, cumulative_outreach_vars_ymax2)))
  # cur_var = point_and_line_vars[2]
  
  # plot3 %<>%
  #   add_trace(
  #     # text = paste(sep = "", cur_var,":<br>", get(cur_var)),
  #     text = build.formula(lhs = NULL, rhs = paste(cur_var, "text")),
  #     type = "scatter", 
  #     mode = "lines+markers",
  #     y = build.formula(lhs = NULL, rhs = cur_var),
  #     name = cur_var
  #   )
  
  
  
  for (cur_var in line_only_vars_student_infections)
  {
    plot3 %<>%
      add_trace(
        # legendgroup = 'group3',
        # text = paste(sep = "", cur_var,":<br>", get(cur_var)),
        text = build.formula(lhs = NULL, rhs = paste(cur_var, "text")),
        type = "scatter", 
        mode = "lines",
        y = build.formula(lhs = NULL, rhs = cur_var),
        name = cur_var
      )
  }
  
  for (cur_var in line_only_vars_hh_adults)
  {
    plot4 %<>%
      add_trace(
        # legendgroup = 'group3',
        # text = paste(sep = "", cur_var,":<br>", get(cur_var)),
        text = build.formula(lhs = NULL, rhs = paste(cur_var, "text")),
        type = "scatter", 
        mode = "lines",
        y = build.formula(lhs = NULL, rhs = cur_var),
        name = cur_var
      )
  }
  
  for (cur_var in line_only_vars_quarantines)
  {
    plot5 %<>%
      add_trace(
        # legendgroup = 'group3',
        # text = paste(sep = "", cur_var,":<br>", get(cur_var)),
        text = build.formula(lhs = NULL, rhs = paste(cur_var, "text")),
        type = "scatter", 
        mode = "lines",
        y = build.formula(lhs = NULL, rhs = cur_var),
        name = cur_var
      )
  }
  
  for (cur_var in cumulative_outreach_vars)
  {
    cumulative_outreach_vars_plot %<>%
      add_trace(
        # legendgroup = 'group3',
        # text = paste(sep = "", cur_var,":<br>", get(cur_var)),
        text = build.formula(lhs = NULL, rhs = paste(cur_var, "text")),
        type = "scatter", 
        mode = "lines",
        y = build.formula(lhs = NULL, rhs = cur_var),
        name = cur_var
      )
  }
  
  plots = list(
    plot1  %>% layout(
      annotations = list(x = x1 , y = 1.1, 
                         text = "Attendance and transmission-free rates", 
                         showarrow = FALSE, 
                         xref='paper', yref='paper')
    ), 
    plot3 %>% layout(
      annotations = list(x = x2 , y = 1.1, 
                         text = "Cumulative incidence rates: students", 
                         showarrow = FALSE, 
                         xref='paper', yref='paper')
    ), 
    plot4  %>% layout(
      annotations = list(x = x3 , y = 1.1, 
                         text = "Cumulative incidence rates: household adults", 
                         showarrow = FALSE, 
                         xref='paper', yref='paper')
    ),
    plot5 %>% layout(
      annotations = list(x = x1 , y = 1.1, 
                         text = "School days missed and classes quarantined", 
                         showarrow = FALSE, 
                         xref='paper', yref='paper')
    ), 
    plot2  %>% layout(
      annotations = list(x = x2 , y = 1.1, 
                         text = "Student quarantine rates", 
                         showarrow = FALSE, 
                         xref='paper', yref='paper')
    ), 
    plot6  %>% layout(
      annotations = list(x = x3 , y = 1.1, 
                         text = "Rates of infected students in school", 
                         showarrow = FALSE, 
                         xref='paper', yref='paper')
    ),  
    plot7  %>% layout(
      annotations = list(x = x1 , y = 1.1, 
                         text = "Daily incidence rates: students", 
                         showarrow = FALSE, 
                         xref='paper', yref='paper')
    ), 
    plot8 %>% layout(
      annotations = list(
                        x = x2 , 
                        y = 1.1, 
                         text = "Daily incidence rates: household adults", 
                         showarrow = FALSE, 
                         xref='paper', yref='paper')
    ),
    
    cumulative_outreach_vars_plot %>% layout(
      annotations = list(
                         x = x3 , 
                         y = 1.1, 
                         text = "Outreach after positive attestations", 
                         showarrow = FALSE, 
                         xref='paper', yref='paper')
    ))
  
  legendstats <- list(
    # x = 0.1, 
    y = -.1,
    orientation = 'h',
    font = list(
      family = "sans-serif",
      size = 12,
      color = "#000")
    # bgcolor = "#E2E2E2",
    # bordercolor = "#FFFFFF",
    # borderwidth = 2
  )
  
  subplot(
    plots,
    nrows = 3,
    shareX = FALSE,
    titleX = TRUE,
    margin = 0.07,
    titleY = TRUE
  ) %>%
    style(hoverinfo = "text") %>% 
    layout(legend = legendstats)
  # browsable(div(
  #   style = "display: flex; flex-wrap: wrap; justify-content: center",
  #   div(plot1, style = "width: 100%; border: solid;"),
  #   div(plot3, style = "width: 100%; border: solid;"),
  #   div(plot2, style = "width: 100%; border: solid;")
  # ))
  
  # plot1 = 
  #   plot_ly(data = average_class_data_by_day, 
  #           x = ~date, 
  #           y = ~`% of students in school`,
  #           type = "scatter", 
  #           mode= "lines+markers",
  #           name = "% of students in school today") %>%
  #   
  #   layout(
  #     yaxis = list(
  #       # range = c(0,100),
  #       title = "Percentage of enrolled students"), 
  #     xaxis = list(title = "Date")) %>%
  #   
  #   add_trace(y = ~`P(class quarantined)`,
  #             name = "% of classes quarantined") %>%
  #   
  #   add_trace(
  #     y = ~`P(class quarantined)`,
  #     name = "% of classes quarantined") %>%
  #   
  #   add_trace(
  #     y = ~`% infected`, 
  #     name = "% of enrolled students infected") %>%
  #   
  #   add_trace(
  #     y = ~`% infected from school`,
  #     name = "% of enrolled students infected from school")%>%
  #   
  #   add_trace(
  #     y = ~`% infected outside school`,
  #     name = "% of enrolled students infected outside school") %>%
  #   
  #   add_trace(
  #     y = ~`% enrolled students uninfectious at home`,
  #     name = "% enrolled students uninfectious at home today") %>%
  #   
  #   add_trace(
  #       y = ~`% enrolled students infectious at school`,
  #       name = "% enrolled students infectious at school today") %>%
  #   
  #   # geom_line(
  #   #   aes(
  #   #     y = `% attendees infectious today`,
  #   #     col ="% attendees infectious today")) +
  #   add_trace(
  #       y = ~`% transmissions today`,
  #       name = "% enrolled students newly infected from school today")
  #   # geom_line(
  #   #   aes(
  #   #     y = `% classrooms with 3+ infected attendees today`,
  #   #     col = "% classrooms with 3+ infected attendees today")) +
  
  
}