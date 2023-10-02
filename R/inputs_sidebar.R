sidebar_column = function()
{
  # column 1
  {
    column1 = box(
      # width = 3,
      #style="overflow-x: scroll; overflow-y: scroll",
      # tags$table(width = "50%"),
      tags$head(
        tags$style(
          type =  "text/css",
          css_settings()) 
      ),
      
      actionButton(
        class = "btn-success",
        inputId = "submit_loc",
        label = "Run simulation"), 
      tags$hr(),
      
      
      helpText("Size of simulation:"),
      
      numericInput(
        "n_schools",
        # width = '800px',
        label = "How many schools to simulate: ",
        value = 10,
        min = 1,
        step = 1),
      
      
      numericInput(
        "n_weeks_to_simulate",
        label = "How many weeks of in-person school to simulate: ",
        value = 8,
        min = 1,
        step = 1),
      tags$hr(),
      helpText("School structure: "),
      
      numericInput(
        "n_classes_per_school",
        # width = '800px',
        label = "Classes per school: ",
        value = 4*7, # 60 students per grade / 15 students per class * grades K-6
        min = 1,
        step = 1),
      
      numericInput(
        "n_students_per_class",
        label = "Students per class: ",
        value = 15,
        min = 1,
        step = 1),
      
      numericInput(
        "n_close_contacts_in_class",
        label = "Number of close classmates per student: ",
        value = 5,
        min = 0,
        step = 1),
      
      
      tags$hr(),
      # helpText(" \r\n"),
      helpText("Initial population characteristics: "),
      
      
      numericInput(
        "baseline_cumulative_incidence_students",
        "Baseline cumulative incidence of COVID infection among students (%): ",
        min = 0,
        max = 100,
        value = 4,
        step = .1),
      
      numericInput(
        "baseline_cumulative_incidence_adults",
        "Baseline cumulative incidence of COVID infection among adults (%): ",
        min = 0,
        max = 100,
        value = 4,
        step = .1),
      
      numericInput(
        "baseline_prevalence_students",
        "Baseline prevalence of active COVID infection among students (%): ",
        min = 0,
        max = 100,
        value = 0.6,
        step = .1),
      
      numericInput(
        "baseline_prevalence_adults",
        "Baseline prevalence of active COVID infection among adults (%): ",
        min = 0,
        max = 100,
        value = 0.6,
        step = .1),
      
      
      numericInput(
        "baseline_vaccinated_adults",
        "Baseline proportion of adults vaccinated (%): ",
        min = 0,
        max = 100,
        value = 0,
        step = 1),
      
      numericInput(
        "baseline_vaccinated_students",
        "Baseline proportion of students vaccinated (%): ",
        min = 0,
        max = 100,
        value = 0,
        step = 1),
      
      
      
      numericInput(
        "baseline_prevalence_of_covid_safety_education",
        "Baseline prevalence of COVID safety education (%): ",
        min = 0,
        max = 100,
        value = 10,
        step = 1),
      
      numericInput(
        "pr_become_educated_after_outreach",
        "Probability of receptiveness to COVID safety outreach (%): ",
        min = 0,
        max = 100,
        value = 50,
        step = 1),
      
      tags$hr(),
      helpText("Baseline testing:"),
      
      dateInput(
        "baseline_collection_date",
        "Date of baseline test collection: ",
        value = as.Date("2020-12-05")), # to change this date, might have to revised default vaccination, cumulative incidence rates, etc
        # value = as.Date("2021-01-01")),
      
      numericInput(
        "turnaround_time_for_baseline_tests",
        "Lab turnaround time for baseline test results (days): ",
        min = 0,
        value = 2,
        step = 1), 
      
      dateInput(
        "in_person_school_start_date",
        "Date when in-person classes start",
        value = as.Date("2021-01-04")),
      
      tags$hr(),
      helpText("Surveillance testing: "),
      
      numericInput("testing_fraction",
                   "Testing fraction (% of enrolled students tested per testing day): ",
                   min = 0,
                   max = 100,
                   value = 25,
                   step = 1),   
      
      checkboxInput("test_on_Mondays",
                    "Test on Mondays?",
                    value = TRUE),
      
      checkboxInput("test_on_Tuesdays",
                    "Test on Tuesdays?"),
      
      checkboxInput("test_on_Wednesdays",
                    "Test on Wednesdays?"),
      
      checkboxInput("test_on_Thursdays",
                    "Test on Thursdays?"),
      
      checkboxInput("test_on_Fridays",
                    "Test on Fridays?"),
      
      numericInput(
        "wait_time_for_surveillance_test_results",
        "Lab turnaround time for surveillance test results (days): ",
        min = 0,
        value = 2,
        step = 1),
      
      
      numericInput(
        "quarantine_length_after_student_positive_test",
        "Quarantine length after student tests positive (days): ",
        min = 0,
        value = 14,
        step = 1), 
      
      numericInput(
        "cluster_time_window",
        "Time window for detecting classroom outbreaks (days): ",
        min = 1,
        value = 14,
        step = 1), 
      
      
      numericInput(
        "n_students_recently_in_class_and_diagnosed_for_outbreak",
        "Number of recent infections required to declare a classroom outbreak: ",
        min = 1,
        value = 3,
        step = 1), 
      
      
      numericInput(
        "quarantine_length_after_outbreak",
        "Quarantine length after classroom cluster detected (days): ",
        min = 0,
        value = 14,
        step = 1),
      
      tags$hr(),
      helpText("Test accuracy: "),
      
      numericInput(
        "test_specificity",
        "Test specificity (%): ",
        min = 0,
        max = 100,
        value = 99.9,
        step = 0.1),
      
      numericInput(
        "test_sensitivity_day_0",
        "Test sensitivity on day of infection (%): ",
        min = 0,
        max = 100,
        value = 0.1,
        step = 0.1),
      
      numericInput(
        "test_sensitivity_day_1",
        "Test sensitivity 1 day after infection (%): ",
        min = 0,
        max = 100,
        value = 0.1,
        step = 0.1),
      
      numericInput(
        "test_sensitivity_day_2",
        "Test sensitivity 2 days after infection (%): ",
        min = 0,
        max = 100,
        value = 0.1,
        step = 0.1),
      
      numericInput(
        "test_sensitivity_day_3",
        "Test sensitivity 3 days after infection (%): ",
        min = 0,
        max = 100,
        value = 50,
        step = 0.1),
      
      numericInput(
        "test_sensitivity_day_4",
        "Test sensitivity 4 days after infection (%): ",
        min = 0,
        max = 100,
        value = 80,
        step = 0.1),
      
      numericInput(
        "peak_test_sensitivity",
        'Test sensitivity 5+ days after infection ("peak sensitivity") (%): ',
        min = 0,
        max = 100,
        value = 95,
        step =0.1),
      
      numericInput(
        "days_of_peak_sensitivity",
        'Days of peak sensitivity: ',
        min = 1,
        value = 3,
        step = 1),
      
      numericInput(
        "test positivity at end of active infection",
        "Test positivity at end of active infection (%): ",
        min = 0,
        max = 100,
        value = 50,
        step = .1),
    
    numericInput(
      "Daily decline in test positivity after recovery",
      "Daily decline in test positivity after recovery (%): ",
      min = 0,
      max = 100,
      value = 10,
      step = .1))
  }
  
  # column 2
  {
    column2 = box(
      tags$head(
        tags$style(
          type =  "text/css",
          css_settings()) 
      ),
      
      
      tags$hr(),
      helpText("Self-report attestation characteristics: "),
      
      numericInput(
        "attestation_sensitivity_symptomatic",
        "Attestation sensitivity if infected and symptomatic (%): ",
        min = 0,
        max = 100,
        value = 90,
        step = 1),
      
      numericInput(
        "attestation_sensitivity_presymptomatic_asymptomatic",
        "Attestation sensitivity if infected and presymptomatic/asymptomatic (based on knowledge of exposure) (%): ",
        min = 0,
        max = 100,
        value = 10,
        step = 1),
      
      numericInput(
        "attestation_specificity",
        "Attestation specificity (%): ",
        min = 0,
        max = 100,
        value = 99.9,
        step = 0.1),
      
      numericInput(
        "quarantine_length_after_positive_attestation",
        "Quarantine length after positive attestation (days): ",
        min = 0,
        value = 3,
        step = 1), 
      
      tags$hr(),
      helpText("Education and testing after positive attestations: "),
      
      numericInput(
        "wait_time_for_household_education_after_positive_attestation",
        "Response time for contacting households after positive attestations (days)",
        min = 0,
        value = 1,
        step = 1),
      
      numericInput(
        "increase_in_accuracy_after_education",
        "Increase in attestation accuracy after education (%): ",
        min = 0,
        max = 100,
        value = 10,
        step = 1),
      
      numericInput(
        "decrease_in_exogenous_risk_after_education",
        "Decrease in exogenous risk from COVID safety education (%): ",
        min = 0,
        max = 100,
        value = 10,
        step = 1),
      
      numericInput(
        "wait_time_for_testing_after_outreach",
        "Response time from attestation followup call until test sample collection (days): ",
        min = 0,
        value = 1,
        step = 1), 
      
      
      numericInput(
        "wait_time_for_attestation_triggered_test_results",
        "Lab turnaround time for attestation-triggered test results (days): ",
        min = 0,
        value = 2,
        step = 1), 
      
      
      numericInput(
        "quarantine_length_after_adult_positive_test",
        "Student quarantine length after household adult tests positive (days): ",
        min = 0,
        value = 14,
        step = 1), 
      
      
      tags$hr(),
      helpText("Infection time-course: "),
      
      numericInput("first_infectious_day",
                   "Days from infection until infectious: ",
                   min = 0,
                   value = 3,
                   step = 1),   
      
      numericInput("first_postinfectious_day",
                   "Days from infection until no longer infectious: ",
                   min = 0,
                   value = 13,
                   step = 1),   
      
      numericInput("first_symptomatic_day",
                   "Days from infection until symptomatic: ",
                   min = 0,
                   value = 5,
                   step = 1),   
      
      numericInput("first_postsymptomatic_day",
                   "Days from infection until no longer symptomatic: ",
                   min = 0,
                   value = 15,
                   step = 1),   
      
      numericInput("first_recovery_day",
                   "Days from infection until no longer actively infected: ",
                   min = 0,
                   value = 15,
                   step = 1),
      
      tags$hr(),
      helpText("Probability of symptoms if infected: "),
      
      numericInput("pr_adult_symptomatic_if_infected",
                   "- for adults (%): ",
                   min = 0,
                   max = 100,
                   value = 70,
                   step = 1),
      
      numericInput("pr_student_symptomatic_if_infected",
                   "- for students (%): ",
                   min = 0,
                   max = 100,
                   value = 50,
                   step = 1),   
      
      
      tags$hr(),
      helpText("Exogenous infection risks: "),
      
      numericInput("student_exogenous_infection_risk_per_day_without_education",
                   "- to students (% per day): ",
                   min = 0,
                   max = 100,
                   value = 0.04,
                   step = 0.01),
      
      numericInput("adult_exogenous_infection_risk_per_day_without_education",
                   "- to parents (% per day): ",
                   min = 0,
                   max = 100,
                   value = 0.04,
                   step = 0.01),
      
      tags$hr(),
      helpText("In-home transmission risks: "),
      
      numericInput("pr_student_infected_per_infectious_household_adult",
                   "- from infectious adult in household to student (% per infection): ",
                   min = 0,
                   max = 100,
                   value = 16,
                   step = 1),
      
      numericInput("pr_household_adult_infected_per_infectious_household_adult",
                   "- from infectious adult in household to partner (% per infection): ",
                   min = 0,
                   max = 100,
                   value = 40,
                   step = 1),
      
      numericInput("pr_household_adult_infected_by_infectious_student",
                   "- from student to adult in household (% per infection): ",
                   min = 0,
                   max = 100,
                   value = 40,
                   step = 1),
      
      tags$hr(),
      helpText("At-school transmission risks: "),
      
      
      numericInput("risk_per_infected_close_contact",
                   "Risk of transmission to close classmates per infectious student (% per day): ",
                   min = 0,
                   max = 100,
                   value = .1,
                   step = .001),
      
      numericInput("risk_per_infected_classmate",
                   "Risk of transmission to distant classmates per infectious student (% per day): ",
                   min = 0,
                   max = 100,
                   value = .05,
                   step = .001)#,
      
      # tags$hr(),
      # helpText("Graphics options: "),
      # 
      # numericInput("plot3_ymax",
      #              "Maximum value displayed on y-axis in graph 2: ",
      #              min = 0,
      #              max = 100,
      #              value = 20,
      #              step = 1)
      )
    
  }
  
  fluidRow(
    (column1),
    (column2)
  )
  
}