plot1 = function(
  average_class_data_by_day,
  line_only_vars = NULL,
  point_and_line_vars = NULL,
  ymax = NULL,
  title
)
{
  
  for (cur_var in c(line_only_vars, point_and_line_vars))
  {
    
    average_class_data_by_day[[paste(cur_var, "text")]] =
      paste(sep = "",
            "Date: ", average_class_data_by_day[["date"]],
            "<br>",
            cur_var, ": ", formatC(average_class_data_by_day[[cur_var]], digits = 3, format = "f"))
    
  }
  
  plot1 =
    plot_ly(
      average_class_data_by_day,
      x = ~date
    ) %>%
    layout(
      yaxis = list(
        title = "",
        rangemode = "tozero"),
      # title = "Other daily outcomes",
      xaxis = list(
        title = "",
        range = range(average_class_data_by_day$date)))
  
  for (cur_var in point_and_line_vars)
  {
    
    plot1 %<>%
      add_trace(
        text = build.formula(lhs = NULL, rhs = paste(cur_var, "text")),
        type = "scatter",
        mode = "lines+markers",
        y = build.formula(lhs = NULL, rhs = cur_var),
        name = cur_var
      ) %>%
      layout(
        yaxis = list(
          title = ""),
        showlegend = TRUE)
    
  }
  
  for (cur_var in line_only_vars)
  {
    
    plot1 %<>%
      add_trace(
        text = build.formula(lhs = NULL, rhs = paste(cur_var, "text")),
        type = "scatter",
        mode = "lines",
        y = build.formula(lhs = NULL, rhs = cur_var),
        name = cur_var
      )
    
  }
  
  if(!is.null(ymax))
  {
    plot1 %<>% layout(yaxis = list(range = c(0, ymax)))
  } else
  {
    plot1 %<>% layout(yaxis = list(
      rangemode = "tozero"))
  }
  
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
  
  plot1 %<>% 
    style(hoverinfo = "text") %>%
    layout(
      legend = legendstats,
      title = title)
  
  return(plot1)
  
}
