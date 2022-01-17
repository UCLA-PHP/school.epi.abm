#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # output$downloadData <- downloadHandler(
  #   filename = "Model Documentation.pdf",
  #   content = function(file) {
  #     file.copy("www/Model Documentation.pdf", file)
  #   }
  # )
  
  url <- "https://medrxiv.org/cgi/content/short/2021.02.27.21252535v1"
         
  url2 =  "https://github.com/d-morrison/school.epi.abm"
  
  # url2 = a("dmorrison01@ucla.edu", href = "dmorrison01@ucla.edu")
  message1 = "This model was created by Doug Morrison, Roch Nianogo, Vladimir Manuel, Onyebuchi A. Arah, Nathaniel Anderson, Tony Kuo, and Moira Inkelas at UCLA."
  message2 = "Questions about the model can be sent to dmorrison01@ucla.edu."
  message3 = "Download our article preprint from medRxiv:"
  # message_full = paste(message1, message2, message3, sep = "\n")
  output$tab = 
    shiny::renderUI(shiny::mainPanel(
      shiny::h1("About this model:"),
      shiny::p(message1),
      shiny::br(),
      shiny::p(message2),
      shiny::br(),
      shiny::p(message3),
      shiny::a(url, href = url),
      shiny::p("See the source code here:"),
      shiny::a(url2, href = url2)))
  
  shiny::observeEvent(
    input$submit_loc, 
    {
      shinydashboard::updateTabItems(session, "tabs",  "graphs")
    })
  
  average_class_data_by_day = 
    shiny::eventReactive(
      input$submit_loc,
      
      {
        
        test_days = c(
          input$test_on_Mondays,
          input$test_on_Tuesdays,
          input$test_on_Wednesdays,
          input$test_on_Thursdays,
          input$test_on_Fridays)
        
        shiny::withProgress(message = "Running simulation", value = 0, 
                     {
                       class_records = run_simulation(
                         print_args = TRUE,
                         in_shiny = TRUE,
                         n_schools = input$n_schools,
                         n_classes_per_school = input$n_classes_per_school,
                         n_students_per_class = input$n_students_per_class,
                         n_weeks_in_time_window = input$n_weeks_to_simulate,
                         
                         n_close_contacts_in_class = input$n_close_contacts_in_class,
                         
                         baseline_collection_date = input$baseline_collection_date,
                         
                         days_from_baseline_collection_until_baseline_return =
                           lubridate::days(input$turnaround_time_for_baseline_tests),
                         
                         in_person_school_start_date = input$in_person_school_start_date, 
                         
                         student_exogenous_infection_risk_per_day_without_education = 
                           input$student_exogenous_infection_risk_per_day_without_education/100,
                         
                         adult_exogenous_infection_risk_per_day_without_education =
                           input$adult_exogenous_infection_risk_per_day_without_education/100,
                         
                         pr_household_adult_infected_per_infectious_household_adult_per_day = 
                           p_infect_per_day(
                             p_infect = input$pr_household_adult_infected_per_infectious_household_adult/100,
                             n_days_infectious = input$first_postinfectious_day - input$first_infectious_day),
                         
                         pr_student_infected_per_infectious_household_adult_per_day = 
                           p_infect_per_day(
                             p_infect = input$pr_student_infected_per_infectious_household_adult/100,
                             n_days_infectious = input$first_postinfectious_day - input$first_infectious_day),
                         
                         
                         pr_household_adult_infected_by_infectious_student_per_day =
                           p_infect_per_day(
                             p_infect = input$pr_household_adult_infected_by_infectious_student/100,
                             n_days_infectious = input$first_postinfectious_day - input$first_infectious_day),
                         
                         wait_time_for_surveillance_test_results = 
                           lubridate::days(ceiling(input$wait_time_for_surveillance_test_results)),
                         
                         surveillance_testing_days = 
                           day.name[1:5][test_days],
                         
                         cluster_time_window = lubridate::days(input$cluster_time_window),
                         
                         n_students_recently_in_class_and_diagnosed_for_outbreak =
                           input$n_students_recently_in_class_and_diagnosed_for_outbreak,
                         
                         pr_adult_symptomatic_if_infected = input$pr_adult_symptomatic_if_infected/100,
                         pr_student_symptomatic_if_infected = input$pr_student_symptomatic_if_infected/100,
                         
                         test_specificity = input$test_specificity/100,
                         test_sensitivity_day_0 = input$test_sensitivity_day_0/100,
                         test_sensitivity_day_1 = input$test_sensitivity_day_1/100,
                         `test sensitivity day 2` = input$test_sensitivity_day_2/100,
                         `test sensitivity day 3` = input$"test_sensitivity_day_3"/100,
                         `test sensitivity day 4` = input$"test_sensitivity_day_4"/100,
                         `peak test sensitivity` = input$"peak_test_sensitivity"/100,
                         `days of peak test sensitivity` = input$"days_of_peak_sensitivity",
                         `test sensitivity on last day of symptoms` = input$"test positivity at end of active infection"/100,
                         `decrease in sensitivity per day after recovery` = input$"Daily decline in test positivity after recovery"/100,
                         
                         attestation_sensitivity_symptomatic = input$attestation_sensitivity_symptomatic/100 ,
                         attestation_sensitivity_presymptomatic = input$attestation_sensitivity_presymptomatic_asymptomatic/100 ,
                         attestation_sensitivity_asymptomatic = input$attestation_sensitivity_presymptomatic_asymptomatic/100 ,
                         attestation_specificity = input$attestation_specificity/100,
                         
                         wait_time_for_household_education_after_positive_attestation = 
                           lubridate::days(input$wait_time_for_household_education_after_positive_attestation),
                         
                         wait_time_for_testing_after_outreach = 
                           lubridate::days(input$wait_time_for_testing_after_outreach),
                         
                         wait_time_for_attestation_triggered_test_results = 
                           lubridate::days(input$wait_time_for_attestation_triggered_test_results),
                         
                         increase_in_accuracy_after_education = input$increase_in_accuracy_after_education/100,
                         decrease_in_exogenous_risk_after_education = input$decrease_in_exogenous_risk_after_education/100,
                         
                         baseline_prevalence_of_covid_safety_education = input$baseline_prevalence_of_covid_safety_education/100,
                         pr_become_educated_after_outreach = input$pr_become_educated_after_outreach/100,
                         
                         baseline_prevalence_students = input$baseline_prevalence_students/100,
                         baseline_prevalence_adults = input$baseline_prevalence_adults/100,
                         baseline_cumulative_incidence_adults = input$baseline_cumulative_incidence_adults/100,
                         baseline_cumulative_incidence_students = input$baseline_cumulative_incidence_students/100,
                         
                         baseline_vaccinated_students = input$baseline_vaccinated_students/100,
                         baseline_vaccinated_adults = input$baseline_vaccinated_adults/100, 
                         
                         risk_per_infected_classmate = input$risk_per_infected_classmate/100,
                         risk_per_infected_close_contact = input$risk_per_infected_close_contact/100,
                         
                         testing_fraction = input$testing_fraction/100,
                         
                         first_infectious_day = lubridate::days(input$first_infectious_day),
                         last_infectious_day = lubridate::days(input$first_postinfectious_day - 1),
                         
                         first_symptomatic_day = lubridate::days(input$first_symptomatic_day),
                         last_symptomatic_day = lubridate::days(input$first_postsymptomatic_day - 1),
                         
                         last_active_infection_day = lubridate::days(input$first_recovery_day - 1),
                         
                         quarantine_length_after_outbreak = 
                           lubridate::days(input$quarantine_length_after_outbreak),
                         quarantine_length_after_student_positive_test = 
                           lubridate::days(input$quarantine_length_after_student_positive_test),
                         quarantine_length_after_adult_positive_test = 
                           lubridate::days(input$quarantine_length_after_adult_positive_test),
                         
                         quarantine_length_after_positive_attestation =
                           lubridate::days(input$quarantine_length_after_positive_attestation)
                       )$class_records
                     })
        
        summarize_records(class_records)
        
      }) 
  
  in_person_school_start_date = shiny::eventReactive(
    input$submit_loc,
    {input$in_person_school_start_date})
  
  plot3_ymax = shiny::eventReactive(
    input$submit_loc,
    {input$plot3_ymax})
  
  output$"attendance and transmission-free rates" = plotly::renderPlotly(
    plot1(
      
      title = "Attendance and transmission-free rates",
      
      line_only_vars =
        c(
          "% of schools with no on-campus transmissions so far",
          "% of schools with no detected infection clusters so far"),
      
      point_and_line_vars =  c(
        "% of enrolled students in school today"),
      
      average_class_data_by_day = average_class_data_by_day()
    )
  )
  
  output$"Student quarantine rates" = 
    plotly::renderPlotly(
      plot1(
        title = "Student quarantine rates",
        point_and_line_vars = c(
          "% of classes quarantined today",
          "% of households with positive attestations today",
          "% of enrolled students infectious at home today",
          "% of enrolled students uninfectious at home today",
          "% of enrolled students at home today"
          # "% of enrolled students infectious at school today",
          # "% of enrolled students newly infected from school today"
        ),
        average_class_data_by_day = average_class_data_by_day()))
  
  output$"School days missed and classes quarantined" = 
    plotly::renderPlotly(
      plot1(
        title = "School days missed and classes quarantined (cumulative)",
        line_only_vars = c(
          "# outbreaks detected per school (cumulative)",
          "# schooldays quarantined per student (cumulative)"
        ),
        average_class_data_by_day = average_class_data_by_day()))
  
  
  
  output$plot_student_cumul_inc_rates =  plotly::renderPlotly(
    plot1(
      title = "Cumulative incidence rates: students",
      line_only_vars = c(
        # "% of enrolled students infected (cumulative)",
        "% of enrolled students infected since baseline (cumulative)",
        "% of enrolled students infected from exogenous sources (cumulative)",
        "% of enrolled students infected by a household adult (cumulative)",
        "% of enrolled students infected from school (cumulative)"
        
      ),
      average_class_data_by_day = average_class_data_by_day()))
  
  output$plot_hh_adult_cumul_inc_rates =  plotly::renderPlotly(
    plot1(
      title = "Cumulative incidence rates: adults",
      line_only_vars = c(
        # "% of household adults infected (cumulative)",
        "% of household adults infected since baseline (cumulative)",
        "% of household adults infected from exogenous sources (cumulative)",
        "% of household adults infected by student (cumulative)",
        "% of household adults infected by other household adult (cumulative)"
        
      ),
      average_class_data_by_day = average_class_data_by_day()))
  
  output$"Rates of infected students in school" =  plotly::renderPlotly(
    plot1(
      title = "Rates of infected students in school",
      point_and_line_vars = c(
        # "% of enrolled students in school today",
        # "% of enrolled students symptomatic in school today",
        # "% of enrolled students uninfectious at home today",
        # "% of enrolled students at home today",
        "% of enrolled students symptomatic in school today",
        "% of enrolled students infectious at school today"),
      average_class_data_by_day = average_class_data_by_day()))
  
  
  output$"Daily incidence rates students" =  plotly::renderPlotly(
    plot1(
      title = "Daily incidence rates: students",
      point_and_line_vars = c(
        "% of enrolled students newly infected today",
        "% of students newly infected from exogenous source today",
        "% of enrolled students newly infected from school today" 
      ),
      average_class_data_by_day = average_class_data_by_day()))
  
  output$"Daily incidence rates household adults" =  plotly::renderPlotly(
    plot1(
      title = "Daily incidence rates: household adults",
      point_and_line_vars = c(
        "% of household adults newly infected today",
        "% of household adults newly infected from exogenous sources today",
        "% of household adults newly infected by student today"
      ),
      average_class_data_by_day = average_class_data_by_day()))
  
  output$"Outreach after positive attestations" =  plotly::renderPlotly(
    plot1(
      title = "Outreach after positive attestations",
      line_only_vars = c(
        "% of households contacted after positive attestations (cumulative)",
        "% of enrolled students educated about COVID safety (cumulative)",
        "% of household adults educated about COVID safety (cumulative)"
      ),
      average_class_data_by_day = average_class_data_by_day()))
  
  
}
