#' Run the agent-based model
#'
#' Simulates COVID-19 transmission among students in K-6 classrooms and the
#' adults in their households, under a chosen combination of surveillance
#' testing, symptom attestation, quarantine, and COVID-safety education
#' outreach. The model is described in Morrison et al. (2023),
#' <https://doi.org/10.3389/fpubh.2023.856940>.
#'
#' Probabilities are on the 0--1 scale. Durations and offsets are
#' [lubridate::days()] periods unless noted otherwise.
#'
#' @param n_schools number of schools to simulate
#' @param n_grades_per_school number of grades in each school
#' @param n_classes_per_school number of classes in each school
#' @param n_students_per_class number of students enrolled in each class
#' @param n_weeks_in_time_window number of weeks to simulate after
#'   in-person school starts
#' @param pr_student_infected_per_infectious_household_adult_per_day daily
#'   probability that a student is infected by each infectious adult in their
#'   household
#' @param pr_household_adult_infected_by_infectious_student_per_day daily
#'   probability that a household adult is infected by an infectious student
#'   in the household
#' @param pr_household_adult_infected_per_infectious_household_adult_per_day
#'   daily probability that a household adult is infected by each other
#'   infectious adult in the household
#' @param pr_student_symptomatic_if_infected probability that an infected
#'   student develops symptoms
#' @param pr_adult_symptomatic_if_infected probability that an infected adult
#'   develops symptoms
#' @param test_specificity probability that a test of an uninfected person is
#'   negative
#' @param test_sensitivity_day_0 probability of a positive test on the day of
#'   infection
#' @param test_sensitivity_day_1 probability of a positive test one day after
#'   infection
#' @param `test sensitivity day 2` probability of a positive test two days
#'   after infection
#' @param `test sensitivity day 3` probability of a positive test three days
#'   after infection
#' @param `test sensitivity day 4` probability of a positive test four days
#'   after infection
#' @param `peak test sensitivity` the maximum test sensitivity achieved during
#'   an infection, reached after day 4
#' @param `days of peak test sensitivity` how long test sensitivity stays at
#'   its peak
#' @param `test sensitivity on last day of symptoms` test sensitivity on
#'   `last_active_infection_day`
#' @param `daily decrease in sensitivity from peak until recovery` daily change
#'   in test sensitivity between the end of the peak and
#'   `last_active_infection_day` (negative for a decrease)
#' @param `decrease in sensitivity per day after recovery` daily decrease in
#'   test sensitivity after `last_active_infection_day`
#' @param attestation_sensitivity_symptomatic probability that a household
#'   with a symptomatic member reports it on the daily attestation
#' @param attestation_sensitivity_asymptomatic probability of a positive
#'   attestation for an asymptomatic infection, representing attestations due
#'   to known exposures
#' @param attestation_sensitivity_presymptomatic probability of a positive
#'   attestation for a presymptomatic infection
#' @param attestation_specificity probability that a household with no
#'   infection gives a negative attestation
#' @param baseline_prevalence_of_covid_safety_education proportion of
#'   households that have COVID-safety education at the start of the
#'   simulation
#' @param pr_become_educated_after_outreach probability that a household
#'   becomes educated about COVID safety after receiving outreach
#' @param baseline_prevalence_adults proportion of adults actively infected at
#'   baseline
#' @param baseline_prevalence_students proportion of students actively
#'   infected at baseline
#' @param baseline_cumulative_incidence_adults proportion of adults ever
#'   infected by baseline
#' @param baseline_cumulative_incidence_students proportion of students ever
#'   infected by baseline
#' @param baseline_recovered_adults proportion of adults recovered (and
#'   immune) at baseline
#' @param baseline_recovered_students proportion of students recovered (and
#'   immune) at baseline
#' @param baseline_vaccinated_adults proportion of adults vaccinated (and
#'   immune) at baseline
#' @param baseline_vaccinated_students proportion of students vaccinated (and
#'   immune) at baseline
#' @param increase_in_accuracy_after_education proportional increase in
#'   attestation accuracy after COVID-safety education: the new accuracy is
#'   `a + (1 - a) * increase_in_accuracy_after_education`, where `a` is the
#'   accuracy before education
#' @param decrease_in_exogenous_risk_after_education proportional decrease in
#'   daily infection risk from outside the school and household after
#'   COVID-safety education
#' @param adult_exogenous_infection_risk_per_day_without_education daily
#'   probability that an adult without COVID-safety education is infected
#'   outside the school and household
#' @param adult_exogenous_infection_risk_per_day_with_education daily
#'   probability that an adult with COVID-safety education is infected outside
#'   the school and household
#' @param student_exogenous_infection_risk_per_day_without_education daily
#'   probability that a student without COVID-safety education is infected
#'   outside the school and household
#' @param student_exogenous_infection_risk_per_day_with_education daily
#'   probability that a student with COVID-safety education is infected
#'   outside the school and household
#' @param risk_per_infected_close_contact daily probability that a student is
#'   infected by each infectious close contact in their class
#' @param risk_per_infected_classmate daily probability that a student is
#'   infected by each infectious classmate
#' @param testing_fraction proportion of each school's enrolled students
#'   tested on each surveillance testing day
#' @param first_infectious_day time from infection until a person becomes
#'   infectious
#' @param last_infectious_day time from infection until a person stops being
#'   infectious
#' @param n_days_infectious number of days a person is infectious
#' @param first_symptomatic_day time from infection until symptoms start, for
#'   symptomatic infections
#' @param last_symptomatic_day time from infection until symptoms end, for
#'   symptomatic infections
#' @param last_active_infection_day time from infection until the infection
#'   resolves
#' @param quarantine_length_after_adult_positive_test how long a student
#'   quarantines after a household adult tests positive
#' @param quarantine_length_after_student_positive_test how long a student
#'   quarantines after testing positive
#' @param quarantine_length_after_positive_attestation how long a student
#'   quarantines after a positive attestation
#' @param quarantine_length_after_outbreak how long a class quarantines after
#'   an outbreak is detected
#' @param in_person_school_start_date the date in-person school starts (a
#'   [base::Date])
#' @param wait_time_for_surveillance_test_results time from a surveillance
#'   test until its result is returned
#' @param wait_time_for_attestation_triggered_test_results time from a test
#'   triggered by a positive attestation until its result is returned
#' @param wait_time_for_household_education_after_positive_attestation time
#'   from a positive attestation until the household receives outreach
#' @param wait_time_for_testing_after_outreach time from outreach until the
#'   household is tested
#' @param surveillance_testing_days names of the weekdays on which
#'   surveillance testing happens, such as `"Monday"`
#' @param days_from_baseline_collection_until_baseline_return time from
#'   baseline test collection until baseline results are returned
#' @param baseline_return_date the date baseline test results are returned (a
#'   [base::Date])
#' @param baseline_collection_date the date baseline tests are collected (a
#'   [base::Date])
#' @param n_students_recently_in_class_and_diagnosed_for_outbreak number of
#'   students in one class diagnosed within `cluster_time_window` that counts
#'   as an outbreak
#' @param cluster_time_window time window for counting diagnoses toward an
#'   outbreak
#' @param n_adults_per_household number of adults in each student's household
#' @param browse_at_start_of_days if `TRUE`, call [browser()] at the start of
#'   each simulated day (for debugging)
#' @param browse_before_return if `TRUE`, call [browser()] before returning
#'   (for debugging)
#' @param n_close_contacts_in_class size of the close-contact groups that each
#'   class is divided into
#' @param print_args if `TRUE`, print every argument value at the start
#' @param verbose if `TRUE`, print progress messages
#' @param plot_data if `TRUE`, plot interim results each simulated school day
#' @param plot3_ymax minimum upper limit of the y-axis for the interim plot of
#'   student infection rates
#' @param in_shiny used to provide progress reporting to the dashboard when
#'   running the model through the Shiny GUI
#'
#' @returns a list with elements `class_records`, a data frame with one row
#'   per class per simulated day, and `inputs`, a list of the argument values
#'   used
#' @export
#'
run_simulation = function(
  
  n_schools = 1000,
  n_grades_per_school = 7,
  
  n_classes_per_school = 
    n_grades_per_school * ceiling(60 / n_students_per_class),
  
  n_students_per_class = 15,
  
  n_weeks_in_time_window = 4 * 2,
  
  pr_student_infected_per_infectious_household_adult_per_day = 
    p_infect_per_day(
      p_infect = .16,
      n_days_infectious = n_days_infectious),
  
  pr_household_adult_infected_by_infectious_student_per_day = 
    p_infect_per_day(
      p_infect = .4,
      n_days_infectious = n_days_infectious),
  
  pr_household_adult_infected_per_infectious_household_adult_per_day = 
    p_infect_per_day(
      p_infect = .4,
      n_days_infectious = n_days_infectious),
  
  pr_student_symptomatic_if_infected = 0.50,
  pr_adult_symptomatic_if_infected = 0.70,
  test_specificity = .999,
  test_sensitivity_day_0 = 1 - test_specificity,
  test_sensitivity_day_1 = 1 - test_specificity,
  `test sensitivity day 2` = 1 - test_specificity,
  `test sensitivity day 3` = .5,
  `test sensitivity day 4` = .7,
  `peak test sensitivity` = .95,
  `days of peak test sensitivity` = days(3),
  `test sensitivity on last day of symptoms` = .5,
  `daily decrease in sensitivity from peak until recovery` = 
    (`test sensitivity on last day of symptoms` - `peak test sensitivity`)  * 
    (days(1) /
    (last_active_infection_day - (days(4) + `days of peak test sensitivity`))),
  
  `decrease in sensitivity per day after recovery` = .1,
    
  attestation_sensitivity_symptomatic = .9,
  attestation_sensitivity_asymptomatic = max(.1, 1 - attestation_specificity), # represents attestations due to known exposures
  attestation_sensitivity_presymptomatic = attestation_sensitivity_asymptomatic,
  attestation_specificity = .999,
  
  baseline_prevalence_of_covid_safety_education = .1,
  
  # a parameter for responsiveness to outreach; not everyone will listen: 
  pr_become_educated_after_outreach = .5,
  # maybe separate for exogenous risk response and attestation response
  
  baseline_prevalence_adults = 0.006, # prevalence at baseline
  baseline_prevalence_students = baseline_prevalence_adults,
  
  baseline_cumulative_incidence_adults = .04,
  baseline_cumulative_incidence_students = baseline_cumulative_incidence_adults,
  
  baseline_recovered_adults = baseline_cumulative_incidence_adults - baseline_prevalence_adults,
  baseline_recovered_students = baseline_cumulative_incidence_students - baseline_prevalence_students,
  
  baseline_vaccinated_adults = 0,
  baseline_vaccinated_students = 0,
  
  increase_in_accuracy_after_education = 0.1,
  decrease_in_exogenous_risk_after_education = 0.1,
  
  adult_exogenous_infection_risk_per_day_without_education = 0.0004,
  
  adult_exogenous_infection_risk_per_day_with_education = 
    adult_exogenous_infection_risk_per_day_without_education * 
    (1 - decrease_in_exogenous_risk_after_education),
  
  student_exogenous_infection_risk_per_day_without_education = 
    adult_exogenous_infection_risk_per_day_without_education,
  
  student_exogenous_infection_risk_per_day_with_education = 
    student_exogenous_infection_risk_per_day_without_education * 
    (1 - decrease_in_exogenous_risk_after_education),
  
  risk_per_infected_close_contact = .001,
  risk_per_infected_classmate = .0005,
  
  testing_fraction = .25,
  first_infectious_day = days(3),
  last_infectious_day = days(12),
  n_days_infectious = 
    (last_infectious_day - first_infectious_day) / days(1) + 1,
  
  first_symptomatic_day = days(5),
  last_symptomatic_day = days(14),
  
  last_active_infection_day = days(14),
  
  quarantine_length_after_adult_positive_test = days(14),
  quarantine_length_after_student_positive_test = days(14),
  quarantine_length_after_positive_attestation = days(3),
  quarantine_length_after_outbreak = days(14),
  
  in_person_school_start_date = as.Date("2021-01-04"), # in-person school start date
  
  wait_time_for_surveillance_test_results = days(2),
  
  wait_time_for_attestation_triggered_test_results = days(2),
  
  wait_time_for_household_education_after_positive_attestation = days(1),
  
  wait_time_for_testing_after_outreach = days(1),
  
  surveillance_testing_days = "Monday",
  
  days_from_baseline_collection_until_baseline_return = days(2),
  
  baseline_return_date = baseline_collection_date + 
    days_from_baseline_collection_until_baseline_return,
  
  baseline_collection_date = as.Date("2020-12-05"),
  
  n_students_recently_in_class_and_diagnosed_for_outbreak = 3,
  
  cluster_time_window = days(14),
  
  n_adults_per_household = 2,
  
  browse_at_start_of_days = FALSE,
  
  browse_before_return = FALSE,
  
  n_close_contacts_in_class = 5,
  
  print_args = FALSE,
  
  verbose = FALSE,
  
  plot_data = verbose | browse_at_start_of_days,
  
  plot3_ymax = 6,
  
  in_shiny = FALSE
  
  
)
{
  
  if(verbose) message("Starting simulation, at ", Sys.time())
  
  ## print argument values (for debugging purposes)
  {
    
    inputs = as.list(environment())
    
    if(print_args)
    {
      for(cur in names(inputs))
      {
        message(cur, " = ", inputs[[cur]])  
        
      }
    }
    
  }
  
  ## simulation parameters and auxiliary functions
  {
    
    pr_test_positive = function(days_since_infected)
    {
      
      if(is.difftime(days_since_infected)) 
      {
        days_since_infected = as.period(days_since_infected)
      }
        
      to_return = case_when(
        is.na(days_since_infected) ~ 1 - test_specificity,
        days_since_infected == days(0) ~ test_sensitivity_day_0,
        days_since_infected == days(1) ~ test_sensitivity_day_1,
        days_since_infected == days(2) ~ `test sensitivity day 2`,
        days_since_infected == days(3) ~ `test sensitivity day 3`,
        days_since_infected == days(4) ~ `test sensitivity day 4`,
        days_since_infected >= days(5) & 
        days_since_infected < (days(5) + `days of peak test sensitivity`) ~
          `peak test sensitivity`,
        
        days_since_infected <= last_active_infection_day ~ 
          `peak test sensitivity` + 
          `daily decrease in sensitivity from peak until recovery` * 
          ((days_since_infected - days(4) - `days of peak test sensitivity`)/days(1)),
        
        days_since_infected > last_active_infection_day ~ 
          `test sensitivity on last day of symptoms` *
           (1 - `decrease in sensitivity per day after recovery`)^
          ((days_since_infected - last_active_infection_day)/days(1)),
        
        TRUE ~ NA_real_)
      
      return(to_return)
      
    }
    
    # graph test sensitivity curve
    if(FALSE)
      {
      
      xlims = 0:60
      predvals = pr_test_positive(days(xlims))
      
      dat1 = tibble(
        `Days since infection` = xlims,
        `Pr(PCR-positive) (%)` = predvals * 100,
        infectious = 
          `Days since infection` %>% between(
            first_infectious_day/days(1),
            last_infectious_day/days(1)
          ),
        symptomatic = 
          `Days since infection` %>% between(
            first_symptomatic_day/days(1),
            last_symptomatic_day/days(1)
          )
      )
      
      test_curve = ggplot(
        data = dat1,
        aes(
        x = `Days since infection`,
        y = `Pr(PCR-positive) (%)`)) +
          geom_point() + geom_line() +
        ylim(0,100) +
        xlab("Days since infection") +
        ylab("Probability of positive PCR test (%)") +
        theme_bw() +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(
                # face="bold",
                size=18))
        
      
      print(test_curve)
        
      
      readr::write_csv(dat1, file =
                  path(results_path, "test sensitivity by infection duration.csv"))
      
      # pdf(path(results_path, "test sensitivity by infection duration.pdf"))
      svg(
        width = 7, 
        height = 5,
        # width = 12 * 96, height = 8 * 96,
        path(results_path, "test sensitivity by infection duration.svg"))
      
      print(test_curve)
      
      dev.off()
      browser()
    }
    
    attestation_accuracy_after_education = function(
      accuracy_before_education,
      increase_in_accuracy_after_education)
    {
      
      accuracy = 
        accuracy_before_education + 
        (1 - accuracy_before_education) * increase_in_accuracy_after_education
      
      return(accuracy)
      
    }
    
    attestation_specificity_educated = 
      attestation_accuracy_after_education(
        attestation_specificity,
        increase_in_accuracy_after_education)
    
    attestation_sensitivity_asymptomatic_educated = 
      attestation_accuracy_after_education(
        attestation_sensitivity_asymptomatic,
        increase_in_accuracy_after_education)
    
    attestation_sensitivity_presymptomatic_educated = 
      attestation_accuracy_after_education(
        attestation_sensitivity_presymptomatic,
        increase_in_accuracy_after_education)
    
    attestation_sensitivity_symptomatic_educated = 
      attestation_accuracy_after_education(
        attestation_sensitivity_symptomatic,
        increase_in_accuracy_after_education)
    
    attestation_positive_probability = function(
      days_since_infected,
      symptomatic_if_infected,
      has_covid_safety_education)
    {
      
      case_when(
        
        is.na(days_since_infected) & !has_covid_safety_education ~ 
          1 - attestation_specificity,
        
        is.na(days_since_infected) & has_covid_safety_education ~ 
          1 - attestation_specificity_educated,
        
        days_since_infected < first_symptomatic_day &
          !symptomatic_if_infected & 
          !has_covid_safety_education ~
          attestation_sensitivity_asymptomatic,
        
        days_since_infected < first_symptomatic_day &
          !symptomatic_if_infected & 
          has_covid_safety_education ~ 
          attestation_sensitivity_asymptomatic_educated,
        
        days_since_infected < first_symptomatic_day & 
          symptomatic_if_infected &
          !has_covid_safety_education ~ 
          attestation_sensitivity_presymptomatic,
        
        days_since_infected < first_symptomatic_day & 
          symptomatic_if_infected &
          has_covid_safety_education ~ 
          attestation_sensitivity_presymptomatic_educated,
        
        days_since_infected <= last_symptomatic_day  & 
          symptomatic_if_infected &
          !has_covid_safety_education ~ 
          attestation_sensitivity_symptomatic,
        
        days_since_infected <= last_symptomatic_day  & 
          symptomatic_if_infected &
          has_covid_safety_education ~ 
          attestation_sensitivity_symptomatic_educated,
        
        days_since_infected <= last_symptomatic_day  & 
          !symptomatic_if_infected &
          !has_covid_safety_education ~ 
          1 - attestation_specificity,
        
        days_since_infected <= last_symptomatic_day  & 
          !symptomatic_if_infected &
          has_covid_safety_education ~ 
          1 - attestation_specificity_educated,
        
        days_since_infected > last_symptomatic_day  & 
          !has_covid_safety_education ~
          1 - attestation_specificity,
        
        days_since_infected > last_symptomatic_day  & 
          has_covid_safety_education ~
          1 - attestation_specificity_educated,
        
        TRUE ~ as.numeric(NA))
      
    }
    
    daily_vars = c(
      "n_students_in_attendance_today",
      "n_symptomatic_students_in_classroom_today",
      "n_initially_uninfected_students_in_classroom_today",
      "n_initially_infected_students_in_classroom_today", 
      "n_students_quarantined_today", 
      "n_infectious_students_in_classroom_today", 
      "n_recovered_students_in_classroom_today")
    
    daily_vars2 = 
      c(
        
        "n_preinfectious_students_in_class_today",
        "n_transmissions_in_class_today",
        "n_infectious_students_at_home_today",
        "n_uninfectious_students_at_home_today",
        "n_students_at_home_today")
    
    set.seed(1)
    
  }
  
  ## simulate data at baseline
  {
    
    ## initialize table of simulated schools; not used at present except to build `classes` table
    {
      
      schools = tibble(
        school = 1:n_schools, 
        n_classes_in_school = n_classes_per_school)
      
    }
    
    ## classes
    {
      
      classes = 
        initialize_classes(
          schools = schools,
          n_students_per_class = 
            n_students_per_class)
      
    }
    
    ## initialize students
    {
      
      baseline_infection_date_distribution = 
        baseline_collection_date - days(1:(last_active_infection_day/days(1))) 
      # picked an arbitrary uniform distribution; 
      # minimum duration of infection = 1, not 0, because I will start simulating new infections the same day
      
      students = 
        classes %>%
        group_by(school, class) %>%
        reframe(
          "student_number_in_class" = 1:n_students_on_roster,
          "close_contact_group" = ((.data$student_number_in_class - 1) %/% n_close_contacts_in_class) + 1, 
          # if the number of students in class isn't divisible by `n_close_contacts_in_class`, 
          # there will be some students with fewer close contacts.
          "ID" = paste(.data$class, .data$student_number_in_class, sep = ":")) %>% 
        # rowwise() %>% # slows down the processing substantially, 2020-09-03
        mutate(
          "household_ID" = .data$ID, # might be changed in the future
          "student_number_in_class" = NULL)
      
      students = 
        initialize_agents(
          agents = 
            students,
          baseline_prevalence_of_covid_safety_education = 
            baseline_prevalence_of_covid_safety_education,
          pr_receptive_to_outreach = 
            pr_become_educated_after_outreach,
          pr_symptomatic_if_infected = 
            pr_student_symptomatic_if_infected,
          pr_recovered_baseline = 
            baseline_recovered_students,
          pr_vaccinated_baseline = 
            baseline_vaccinated_students,
          pr_active_infection_baseline = 
            baseline_prevalence_students,
          baseline_infection_date_distribution = 
            baseline_infection_date_distribution,
          baseline_collection_date = 
            baseline_collection_date)
          
      
      if(verbose)
      {
        cur_message = paste(
          sep = "",
          "At baseline, ",
          with(students,sum(infected & !recovered, na.rm = TRUE)), 
          " out of ",
          nrow(students),
          " students (",
          round( with(students,sum(infected & !recovered, na.rm = TRUE))/nrow(students) * 100, 2),
          
          "%) were actively infected with COVID.")
        
        message(cur_message)
        
      }
      
      ## test all students at baseline
      {
        
        test_results = 
          students %>% 
          select("ID", "infection_date") %>%
          mutate(
            collection_date = baseline_collection_date,
            result_date = baseline_return_date,
            test_type = "baseline",
            test_result = rbernoulli(
              n = n(),
              p = pr_test_positive(baseline_collection_date - .data$infection_date)))
        
      }
      
    }
    
    # initialize households
    {
      
      households = 
        students %>%
        select(
          "school",
          "class",
          "household_ID",
          "quarantine_end_date") %>%
        mutate(
          "n_adults_in_household" = n_adults_per_household,
          "received_outreach_after_positive_attestation" = FALSE)
      
    }
    
    # initialize household adults
    {
      
      household_adults = 
        households %>% 
        reframe(
          .by = "household_ID",
          ID = paste(.data$household_ID, ":A", 1:.data$n_adults_in_household, sep = "")) %>%
        # `.by` keeps households in order of first appearance, but the
        # downstream random draws were defined with households sorted by ID
        # (as `group_by()` returns them); sort to keep seeded results stable:
        arrange(.data$household_ID)
      
      household_adults =
        initialize_agents(
          agents = 
            household_adults,
          baseline_prevalence_of_covid_safety_education = 
            baseline_prevalence_of_covid_safety_education,
          pr_receptive_to_outreach = 
            pr_become_educated_after_outreach,
          pr_symptomatic_if_infected = 
            pr_adult_symptomatic_if_infected,
          pr_recovered_baseline = 
            baseline_recovered_adults,
          pr_vaccinated_baseline = 
            baseline_vaccinated_adults,
          pr_active_infection_baseline = 
            baseline_prevalence_adults,
          baseline_infection_date_distribution = 
            baseline_infection_date_distribution,
          baseline_collection_date =
            baseline_collection_date)
      
    }
    
    # calendar
    {
      
      calendar = tibble(
        "date" = seq.Date(
          from = baseline_collection_date, # start the day of baseline collection
          to = in_person_school_start_date + days(n_weeks_in_time_window * 7 - 1),
          by = "day"),
        # days_since_in_person_school_start_date = date - in_person_school_start_date,
        "day_of_week" = weekdays(date),
        "school_day" = day_of_week %in% day.name[1:5] & date >= in_person_school_start_date,
        "surveillance testing today" = 
          day_of_week %in% unlist(surveillance_testing_days) & 
          date >= in_person_school_start_date)
      
    }
    
    # records
    {
      
      student_records = NULL
      class_records = NULL
      # attestation_records = NULL
      scheduled_tests = NULL
      scheduled_household_outreach_efforts = NULL
      completed_household_outreach_efforts = NULL
      classroom_cluster_investigations = NULL # not currently used
      
    }
    
  }
  
  ## iterate over calendar days
  {
    
    for (i in 1:nrow(calendar))
    {
      
      # setup
      {
        
        cur_date = calendar[i, "date", drop = TRUE]
        
        message("Starting simulation of date ", cur_date, ", day ", i, " of ", nrow(calendar), "\n(Real time: ", Sys.time(), "; mem used = ", round(as.numeric(lobstr::mem_used())/10^6, 1), " MB)")
        
        if(in_shiny)
        {
          shiny::incProgress(1/nrow(calendar), detail = paste("Day ", i, " of ", nrow(calendar)))
        }
        
        cur_message = paste(
          sep = "",
          sum(students$infected & !students$infected_at_baseline), 
          " out of ",
          nrow(students),
          " students (",
          round(sum(students$infected & !students$infected_at_baseline)/nrow(students) * 100, 2),
          "%) have been infected with COVID since baseline.")
        
        message(cur_message)
        
        transmissions_per_school = 
          students %>%
          group_by(school) %>%
          summarize(
            .groups = "drop",
            n_transmissions = sum(infection_source == "from school", na.rm = TRUE)
          ) %>%
          summarize(
            pct_no_transmissions = 
              mean(n_transmissions == 0) * 100
          ) %>% unlist()
        
        cur_message = paste(
          sep = "",
          transmissions_per_school,
          "% of schools have no transmissions so far."
        )
        
        if(verbose) message(cur_message)
        
        if(plot_data & cur_date > in_person_school_start_date)
        {
          
          plot1a = plot1_plotly(
            class_records, 
            plot3_ymax = plot3_ymax)
          
          print(plot1a)
          
        }
        
        if(browse_at_start_of_days) 
        {
          
          browser()
        }
        
      }
      
      # update status and time duration variables 
      {
        
        students %<>% 
          mutate(
            
            days_since_infected = 
              cur_date - .data$infection_date, # NA if not previously infected
            
            active_infection = 
              !is.na(.data$days_since_infected) & 
              .data$days_since_infected <= last_active_infection_day,
            
            symptomatic_today =
              .data$symptomatic_if_infected &
              .data$infected &
              .data$days_since_infected >= first_symptomatic_day &
              .data$days_since_infected <= last_symptomatic_day,
            
            infectious_today = 
              .data$infected &
              .data$days_since_infected >= first_infectious_day &
              .data$days_since_infected <= last_infectious_day
            
            # recovered = infected & days_since_infected > 14,
          )
        
        if(verbose)
        {
          
          message("students:")
          print(with(students, addmargins(table(symptomatic_today, infectious_today))))
          
        }
        
        household_adults %<>% 
          mutate(
            
            days_since_infected = cur_date - .data$infection_date, # NA if not previously infected
            
            active_infection = 
              !is.na(.data$days_since_infected) & 
              .data$days_since_infected <= last_active_infection_day,
            
            symptomatic_today =
              .data$symptomatic_if_infected &
              .data$infected &
              .data$days_since_infected >= first_symptomatic_day &
              .data$days_since_infected <= last_symptomatic_day,
            
            infectious_today = 
              .data$infected &
              .data$days_since_infected >= first_infectious_day &
              .data$days_since_infected <= last_infectious_day
            
            # recovered = infected & days_since_infected > 14,
          )
        
        if(verbose)
        {
          
          message("household adults:")
          print(with(household_adults, addmargins(table(symptomatic_today, infectious_today))))
          
        }
        
      }
      
      # update quarantines based on test results
      {
        
        # might want to change this to "test_results_summary" and add "last negative test collection date",
        # "last negative collection date before first positive collection date", 
        # "first negative collection date after last positive collection date", etc
        
        positive_test_results_summary = 
          test_results %>% 
          filter(.data$result_date <= cur_date,
                 .data$test_result == TRUE) %>%
          summarize(
            .by = "ID",
            
            # could use min(collection_date) and max(collection_date),  
            # but since I know the test results are arranged by date, 
            # this way saves computing time:
            # 
            earliest_positive_test_collection_date = 
              .data$collection_date[1], 
            
            latest_positive_test_collection_date = 
              .data$collection_date[n()])
        
        household_adults %<>%
          select(-any_of("earliest_positive_test_collection_date")) %>%
          left_join(
            by = "ID",
            positive_test_results_summary %>% 
              select("ID", "earliest_positive_test_collection_date"))
        
        households %<>%
          select(-any_of(c("most_recent_adult_diagnosis", "n_adult_diagnoses"))) %>%
          left_join(
            by = "household_ID",
            household_adults %>%
              summarize(
                .by = "household_ID",
                n_adult_diagnoses = 
                  sum(!is.na(.data$earliest_positive_test_collection_date)),
                most_recent_adult_diagnosis = 
                  suppressWarnings( 
                    # warning occurs if neither parent has tested positive yet
                    max(na.rm = TRUE, .data$earliest_positive_test_collection_date)))
          ) %>%
          mutate(
            most_recent_adult_diagnosis = if_else(
              .data$n_adult_diagnoses > 0,
              .data$most_recent_adult_diagnosis,
              as.Date(NA)
            ),
            quarantine_end_date = 
              pmax(
                na.rm = TRUE,
                .data$quarantine_end_date,
                .data$most_recent_adult_diagnosis + quarantine_length_after_adult_positive_test
              )
          )
        
        students %<>%
          select(-any_of("earliest_positive_test_collection_date")) %>%
          left_join(
            by = "ID",
            positive_test_results_summary %>% 
              select("ID", "earliest_positive_test_collection_date"))
        # we deal with students' test results and quarantine end date later, when we calculate attendance status
        
        if(verbose) message(
          sum(
            students$ID %in% 
              (test_results %>% 
                 filter(result_date == cur_date))$ID
          ),
          " students received test results today.")
        
        if(verbose) message(
          sum(
            students$ID %in% 
              (test_results %>% 
                 filter(result_date == cur_date, test_result == TRUE))$ID
          ),
          " students received positive COVID test results today.")
        
        
      }
      
      # try to detect classroom outbreaks
      if(cur_date > in_person_school_start_date)
      {
        
        students_in_class_during_latest_window = 
          student_records %>%
          filter(
            cur_date - .data$date <= cluster_time_window,
            .data$in_school_today) %>%
          select("ID", "class") %>%
          unique()
        
        diagnoses_in_latest_window = 
          students %>%
          filter(cur_date - .data$earliest_positive_test_collection_date <= cluster_time_window)
        
        n_students_in_class_and_diagnosed_during_latest_window = 
          inner_join(
            students_in_class_during_latest_window,
            diagnoses_in_latest_window,
            by = c("class", "ID")) %>%
          group_by(class) %>%
          summarise(
            .groups = 'drop',
            n_students_in_class_and_diagnosed_during_latest_window = n())
        
        latest_date_with_students = 
          class_records %>%
          summarise(
            .by = "class",
            n_dates_with_students = 
              length(.data$date[.data$n_students_in_attendance_today > 0]),
            latest_date_with_students = 
              suppressWarnings(max(.data$date[.data$n_students_in_attendance_today > 0]))) %>%
          mutate(
            latest_date_with_students = 
              if_else(
                .data$n_dates_with_students == 0,
                as.Date(NA), # something about the NA format gets corrupted otherwise, as of 2020-12-04
                .data$latest_date_with_students
              ),
            n_dates_with_students = NULL
          )
        
        classes %<>% 
          select(-any_of(
            c(
              "n_students_in_class_and_diagnosed_during_latest_window",
              "latest_date_with_students"))) %>%
          left_join(
            by = "class",
            n_students_in_class_and_diagnosed_during_latest_window) %>%
          left_join(
            by = "class",
            latest_date_with_students) %>%
          mutate(
            "n_students_in_class_and_diagnosed_during_latest_window" = 
              if_else(
                is.na(.data$n_students_in_class_and_diagnosed_during_latest_window),
                as.integer(0),
                .data$n_students_in_class_and_diagnosed_during_latest_window),
            
            "recent_outbreak_detected_in_class" = 
              .data$n_students_in_class_and_diagnosed_during_latest_window >= 
              n_students_recently_in_class_and_diagnosed_for_outbreak,
            
            "outbreak_newly_detected_in_class" = 
              .data$recent_outbreak_detected_in_class &
              (is.na(.data$quarantine_end_date) | 
                 .data$quarantine_end_date <= cur_date),
            
            "quarantine_end_date" = 
              if_else(
                .data$outbreak_newly_detected_in_class,
                .data$latest_date_with_students + quarantine_length_after_outbreak,
                .data$quarantine_end_date),
            
            "class_quarantined_today" =
              !is.na(.data$quarantine_end_date) &
              .data$quarantine_end_date > cur_date,
            
            "last_detected_cluster_date" = 
              if_else(
                .data$outbreak_newly_detected_in_class,
                cur_date,
                .data$last_detected_cluster_date),
            
            "n_outbreaks" = .data$n_outbreaks + .data$outbreak_newly_detected_in_class)
        
      }
      
      # update attestations
      {
        
        household_adults %<>%
          mutate(
            attestation_positive_probability = 
              attestation_positive_probability(
                .data$days_since_infected,
                .data$symptomatic_if_infected,
                .data$has_covid_safety_education),
            attestation_positive = rbernoulli(
              n = n(),
              p = .data$attestation_positive_probability))
        
        if(any(is.na(household_adults$attestation_positive_probability)))
        {
          browser(message("error in adults' attestation probability calculations"))
        }
        
        students %<>% 
          mutate(
            attestation_positive_probability = 
              attestation_positive_probability(
                .data$days_since_infected,
                .data$symptomatic_if_infected,
                .data$has_covid_safety_education),
            attestation_positive = rbernoulli(
              n = n(),
              p = .data$attestation_positive_probability))
        
        if(any(is.na(students$attestation_positive_probability)))
        {
          browser(message("error in students' attestation probability calculations"))
        }
        
        
        # attestation_records %<>%
        #   bind_rows(
        #     students %>%
        #       select(ID, attestation_positive) %>%
        #       mutate(date = cur_date)
        #   ) %>%
        #   bind_rows(
        #     household_adults %>% 
        #       select(ID, attestation_positive) %>%
        #       mutate(date = cur_date))
        
        households %<>%
          select(-any_of(c(
            "n_infectious_adults_in_household_today",
            "n_adult_positive_attestations",
            "student_attestation_positive"))) %>%
          
          left_join(
            by = "household_ID",
            
            household_adults %>%
              summarise(
                .by = "household_ID",
                n_infectious_adults_in_household_today = sum(.data$infectious_today),
                # n_symptomatic_adults_in_household_today = sum(symptomatic_today),
                
                n_adult_positive_attestations = sum(.data$attestation_positive))) %>%
          
          left_join(
            by = "household_ID",
            
            students %>% 
              select("household_ID", "attestation_positive") %>% 
              rename(student_attestation_positive = "attestation_positive")) %>%
          
          mutate(
            
            household_attestation_positive = 
              .data$student_attestation_positive | .data$n_adult_positive_attestations > 0,
            
            # could simplify this to look more like the handling of test results, 
            # specifically, calculate "date of most recent positive attestation"
            # and use pmax on that.
            quarantine_end_date = 
              if_else(
                .data$household_attestation_positive,
                pmax(
                  na.rm = TRUE,
                  .data$quarantine_end_date,
                  cur_date + quarantine_length_after_positive_attestation),
                .data$quarantine_end_date
              )
            
          )
        
        new_households_to_contact = 
          households %>% 
          semi_join(
            by = "household_ID",
            students %>% 
              filter(is.na(.data$earliest_positive_test_collection_date))) %>%
          filter(.data$household_attestation_positive) %>%
          mutate(
            date_of_covid_education_outreach = 
              cur_date + wait_time_for_household_education_after_positive_attestation) %>%
          select(
            "household_ID",
            "date_of_covid_education_outreach",
            "n_adult_positive_attestations",
            "student_attestation_positive"
          )
        
        # merge duplicate pending outreaches to the same households:
        scheduled_household_outreach_efforts %<>%
          bind_rows(new_households_to_contact) %>%
          summarise(
            .by = "household_ID",
            date_of_covid_education_outreach = min(.data$date_of_covid_education_outreach),
            n_adult_positive_attestations = max(.data$n_adult_positive_attestations),
            student_attestation_positive = any(.data$student_attestation_positive))
        
        if(verbose) message(sum(households$household_attestation_positive), " households attested positive today.")
        
      }
      
      # update students' attendance statuses
      {
        
        students %<>% 
          
          select(-any_of("household_quarantine_end_date")) %>%
          left_join(
            by = "household_ID",
            households %>%
              select("household_ID", "quarantine_end_date") %>%
              rename(household_quarantine_end_date = "quarantine_end_date")
          ) %>%
          
          mutate(
            
            quarantine_end_date = if_else(
              condition = 
                is.na(.data$earliest_positive_test_collection_date),
              true = 
                pmax(
                  na.rm = TRUE,
                  # class quarantine end date is accounted for below
                  .data$household_quarantine_end_date,
                  .data$quarantine_end_date), # they might already be quarantined for longer due to detected cluster in class
              false = 
                .data$earliest_positive_test_collection_date + 
                quarantine_length_after_student_positive_test),
            
            student_quarantined_today = 
              !is.na(.data$quarantine_end_date) & 
              (cur_date <= .data$quarantine_end_date)
            
          ) %>%
          
          select(-any_of("class_quarantined_today")
          ) %>%
          
          left_join(
            by = "class",
            classes %>% select("class", "class_quarantined_today")
          ) %>%
          
          mutate(
            in_school_today = 
              cur_date >= in_person_school_start_date &
              calendar[i, "school_day", drop = TRUE] &
              !.data$class_quarantined_today &
              !.data$student_quarantined_today)
        
        if(verbose)
        {
          message("students quarantined today:")
          print(addmargins(with(students, table(student_quarantined_today, in_school_today))))
          
        }
        
      }
      
      # run tests if scheduled:
      if(calendar[i, "surveillance testing today", drop = TRUE])
      {
        
        # can't filter by in-school, bc I need to count enrolled students
        new_tests = 
          students %>% 
          select("school", "in_school_today", "ID", "infection_date") %>%
          mutate(
            .by = "school",
            tested_today = 
              .data$ID %in% sample(
                replace = FALSE,
                x = .data$ID[.data$in_school_today],
                size = min(
                  sum(.data$in_school_today), 
                  floor(n() * testing_fraction)))) %>% 
          filter(.data$tested_today) %>%
          # rowwise() %>% # might save some time by avoiding unnecessary RNG
          mutate(
            collection_date = cur_date,
            result_date = cur_date + wait_time_for_surveillance_test_results,
            test_type = "surveillance",
            pr_positive = pr_test_positive(cur_date - .data$infection_date),
            test_result = 
              if_else(
                condition = .data$tested_today,
                true = rbernoulli(
                  n = n(),
                  p = .data$pr_positive),
                false = NA))
        
        test_results %<>% 
          bind_rows(
            new_tests %>%
              select(ID, infection_date, collection_date, result_date, test_type, test_result))
        
        if(verbose)
        {
          
          message(nrow(new_tests), " surveillance tests were collected in school today")
          cur_table =
            with(new_tests, 
                 
                 table(
                   useNA = 'ifany',
                   # cut(days_since_infected, breaks = c(0, 2 ,as.numeric(last_active_infection_day), Inf), right = FALSE),
                   pr_positive,
                   test_result
                 ))
          
          print(addmargins(cur_table))
          print(proportions(cur_table, margin = 1))
        
        }
        
      }
      
      # generate new in-class transmissions
      {
        # this step gets run even on weekends, for summary statistics,
        # but no transmissions actually occur on weekends because `n_infectious_students_in_classroom_today` == 0
        
        classes %<>% 
          select(-any_of(daily_vars)) %>%
          left_join(
            by = "class",
            students %>% 
              summarise(
                .by = "class",
                
                "n_students_in_attendance_today" = 
                  sum(.data$in_school_today),
                
                "n_initially_uninfected_students_in_classroom_today" = 
                  sum(.data$in_school_today & !.data$infected),
                
                "n_students_quarantined_today" = 
                  sum(.data$student_quarantined_today | .data$class_quarantined_today),
                
                "n_infectious_students_in_classroom_today" = 
                  sum(.data$in_school_today & .data$infectious_today),
                
                "n_symptomatic_students_in_classroom_today" = 
                  sum(.data$in_school_today & .data$symptomatic_today),
                
                "n_initially_infected_students_in_classroom_today" = 
                  sum(.data$in_school_today & .data$active_infection),
                
                "n_recovered_students_in_classroom_today" = 
                  sum(.data$in_school_today & 
                        .data$infected & 
                        .data$days_since_infected > .env$last_symptomatic_day)))
        
        close_contact_groups = 
          students %>%
          summarize(
            .by = c("class", "close_contact_group"),
            n_infectious_close_contacts_in_school_today = 
              (.env$n_close_contacts_in_class > 0) * 
              sum(.data$in_school_today & .data$infectious_today))
        
        students %<>% 
          select(-any_of(c("n_infectious_students_in_classroom_today", "n_infectious_close_contacts_in_school_today"))) %>%
          # count the number of infectious students in each class:
          
          left_join(
            by = c("class", "close_contact_group"),
            close_contact_groups) %>%
          left_join(
            by = "class",
            classes %>% select("class", "n_infectious_students_in_classroom_today")) %>%
          mutate(
            
            pr_infected_from_school_today = 
              1 - 
              (1 - .env$risk_per_infected_close_contact)^.data$n_infectious_close_contacts_in_school_today *
              (1 - .env$risk_per_infected_classmate)^(.data$n_infectious_students_in_classroom_today),
            
            # TODO: consider simulating the possibility of re-infection after recovery
            infected_from_school_today =
              .data$in_school_today &
              !.data$immune & 
              !.data$infected &
              rbernoulli(
                n = n(),
                p = .data$pr_infected_from_school_today),
            
            infected = .data$infected | .data$infected_from_school_today,
            
            infection_source = if_else(
              .data$infected_from_school_today,
              "from school",
              .data$infection_source),
            
            active_infection = .data$active_infection | .data$infected_from_school_today,
            
            infection_date = 
              if_else(
                condition = .data$infected_from_school_today,
                true = cur_date, 
                false = .data$infection_date))
        
        classes %<>% 
          select(-any_of(daily_vars2)) %>%
          left_join(
            by = "class",
            students %>% 
              summarise(
                .by = "class",
                
                n_infectious_students_at_home_today = 
                  sum(!.data$in_school_today & .data$infectious_today),
                
                
                n_uninfectious_students_at_home_today = 
                  sum(!.data$in_school_today & !.data$infectious_today),
                
                n_students_at_home_today = 
                  sum(!.data$in_school_today),
                
                n_preinfectious_students_in_class_today = 
                  sum(.data$in_school_today & .data$infected & .data$days_since_infected < .env$first_infectious_day),
                
                n_transmissions_in_class_today = 
                  sum(.data$infected_from_school_today)))
        
      }
      
      # simulate out-of-school infections
      {
        
        ## a better way to handle attribution without unfairly prioritizing one 
        ## source is to coin-flip all sources, and record all sufficient sources 
        ## of infection 
        
        ## simulate out-of-school infections of students
        {
          
          students %<>% 
            
            mutate(
              infected_outside_school_and_home_today = 
                !.data$immune & !.data$infected &
                rbernoulli(
                  n = n(),
                  p = if_else(
                    .data$has_covid_safety_education,
                    student_exogenous_infection_risk_per_day_with_education,
                    student_exogenous_infection_risk_per_day_without_education
                  )),
              
              infected = .data$infected | .data$infected_outside_school_and_home_today,
              
              infection_source = 
                if_else(
                  .data$infected_outside_school_and_home_today,
                  "exogenous",
                  .data$infection_source),
              
              active_infection = .data$active_infection | .data$infected_outside_school_and_home_today,
              
              infection_date = if_else(
                .data$infected_outside_school_and_home_today,
                cur_date,
                .data$infection_date)) %>%
            
            select(
              -any_of(
                c(
                  "infected_outside_school_and_home_today",
                  "n_infectious_adults_in_household_today")
              )) %>%
            
            left_join(
              by = "household_ID",
              households %>% select("household_ID", "n_infectious_adults_in_household_today")
            ) %>%
            
            mutate(
              infected_by_household_adult_today = 
                !.data$immune & !.data$infected &
                rbernoulli(
                  n = n(),
                  p = 1 - 
                    (1 - pr_student_infected_per_infectious_household_adult_per_day)^.data$n_infectious_adults_in_household_today),
              
              infected = .data$infected | .data$infected_by_household_adult_today,
              
              infection_source = 
                if_else(
                  .data$infected_by_household_adult_today,
                  "household adult",
                  .data$infection_source),
              
              active_infection = .data$active_infection | .data$infected_by_household_adult_today,
              
              infection_date = if_else(
                .data$infected_by_household_adult_today,
                cur_date,
                .data$infection_date)
            ) %>%
            
            select(-"infected_by_household_adult_today")
          
        }
        
        # simulate infections of adults
        {
          
          household_adults %<>%
            mutate(
              infected_outside_home_today = 
                !.data$immune & !.data$infected &
                rbernoulli(
                  n = n(),
                  p = if_else(
                    .data$has_covid_safety_education,
                    adult_exogenous_infection_risk_per_day_with_education,
                    adult_exogenous_infection_risk_per_day_without_education)),
              
              infected = .data$infected | .data$infected_outside_home_today,
              
              infection_source = 
                if_else(
                  .data$infected_outside_home_today,
                  "exogenous",
                  .data$infection_source),
              
              active_infection = .data$active_infection | .data$infected_outside_home_today,
              
              infection_date = if_else(
                .data$infected_outside_home_today,
                cur_date,
                .data$infection_date),
              
              infected_outside_home_today = NULL
            ) %>%
            select(-any_of("n_infectious_adults_in_household_today")
            ) %>%
            left_join(
              by = "household_ID",
              households %>% select(all_of(c("n_infectious_adults_in_household_today", "household_ID")))
            ) %>%
            mutate(
              infected_by_other_household_adult_today = 
                !.data$immune & !.data$infected &
                rbernoulli(
                  n = n(),
                  p = 1 - 
                    (1 - pr_household_adult_infected_per_infectious_household_adult_per_day)^.data$n_infectious_adults_in_household_today),
              
              infected = .data$infected | .data$infected_by_other_household_adult_today,
              
              infection_source = 
                if_else(
                  .data$infected_by_other_household_adult_today,
                  "other household adult",
                  .data$infection_source),
              
              active_infection = .data$active_infection | .data$infected_by_other_household_adult_today,
              
              infection_date = if_else(
                .data$infected_by_other_household_adult_today,
                cur_date,
                .data$infection_date),
              
              infected_by_other_household_adult_today = NULL
            ) %>%
            select(-any_of("student_infectious_today")
            ) %>%
            left_join(
              by = "household_ID",
              students %>% 
                select("household_ID", "infectious_today") %>% 
                rename(student_infectious_today = "infectious_today")
            ) %>%
            mutate(
              infected_by_student_today = 
                !.data$immune & !.data$infected &
                .data$student_infectious_today &
                rbernoulli(
                  n = n(),
                  p = pr_household_adult_infected_by_infectious_student_per_day),
              
              infected = .data$infected | .data$infected_by_student_today,
              
              infection_source = 
                if_else(
                  .data$infected_by_student_today,
                  "student",
                  .data$infection_source),
              
              active_infection = .data$active_infection | .data$infected_by_student_today,
              
              infection_date = if_else(
                .data$infected_by_student_today,
                cur_date,
                .data$infection_date),
              
              infected_by_student_today = NULL)
          
        }
        
      }
      
      # calls to households with positive attestations
      {
        # not sure when in the day this should occur
        
        household_education_calls_today = 
          scheduled_household_outreach_efforts %>% 
          filter(.data$date_of_covid_education_outreach == cur_date)
        
        scheduled_household_outreach_efforts %<>%
          filter(.data$date_of_covid_education_outreach > cur_date)
        
        completed_household_outreach_efforts %<>%
          bind_rows(household_education_calls_today)
        
        if(verbose)
        {
          message(nrow(completed_household_outreach_efforts), " outreach calls have been completed.")
        }
        
        households %<>%
          mutate(
            received_outreach_after_positive_attestation = 
              .data$received_outreach_after_positive_attestation | 
              .data$household_ID %in% household_education_calls_today$household_ID)
        
        household_adults %<>%
          mutate(
            has_covid_safety_education = 
              .data$has_covid_safety_education | 
              (.data$receptive_to_outreach &
                 .data$household_ID %in% household_education_calls_today$household_ID))
        
        students %<>%
          mutate(
            has_covid_safety_education = 
              .data$has_covid_safety_education | 
              (.data$receptive_to_outreach &
                 .data$household_ID %in% household_education_calls_today$household_ID))
        
        if(verbose) 
          message(nrow(household_education_calls_today), " households received outreach calls today.")
        
        
      }
      
      if(nrow(household_education_calls_today) > 0)
      {
        
        # schedule new tests
        {
          
          households_to_test_adults = 
            household_education_calls_today %>%
            filter(.data$n_adult_positive_attestations > 0)
          
          households_to_test_students = 
            household_education_calls_today %>%
            filter(.data$student_attestation_positive)
          
          new_scheduled_tests = 
            household_adults %>% 
            filter(
              .data$household_ID %in% households_to_test_adults$household_ID
            ) %>%
            select("ID") %>%
            bind_rows(
              students %>%
                filter(.data$household_ID %in% households_to_test_students$household_ID) %>%
                select("ID")
            ) %>%
            mutate(
              collection_date = cur_date + wait_time_for_testing_after_outreach)
          
          scheduled_tests %<>%
            bind_rows(new_scheduled_tests)
          
          
        }
        
        {# test adults
          
          tests_scheduled_today =
            scheduled_tests %>% 
            filter(.data$collection_date == cur_date)
          
          new_adult_tests = 
            household_adults %>%
            select("ID", "infection_date") %>%
            filter(.data$ID %in% tests_scheduled_today$ID) %>%
            mutate(
              collection_date = cur_date,
              result_date = cur_date + wait_time_for_attestation_triggered_test_results,
              test_type = "attestation followup",
              test_result = rbernoulli(
                n = n(),
                p = pr_test_positive(cur_date - .data$infection_date)))
          
          test_results %<>%
            bind_rows(new_adult_tests)
          
          if(verbose)
            message(nrow(new_adult_tests), " household adults were tested today.")
          
        }
        
        {# test students
          
          new_student_tests = 
            students %>%
            select(
              "ID", "infected", "infection_date"
            ) %>%
            filter(.data$ID %in% tests_scheduled_today$ID) %>%
            mutate(
              collection_date = cur_date,
              result_date = cur_date + wait_time_for_attestation_triggered_test_results,
              test_type = "attestation followup",
              test_result = rbernoulli(
                n = n(),
                p = pr_test_positive(cur_date - .data$infection_date)))
          
          test_results %<>%
            bind_rows(new_student_tests)
          
          
          if(verbose)
          {
            message("results of student tests in response to attestations collected today:")
            cur_table = 
              with(new_student_tests, table(infected, test_result))
                   
            print(cur_table) # can't add margins easily, might be no tests
            print(proportions(cur_table, margin = 1))
            
          }
          
        }
        
      }
      
      # update (cumulative) totals
      {
        
        adult_infection_counts_by_household = 
          household_adults %>% 
          summarize(
            .by = "household_ID",
            "n household adults educated about COVID safety" = 
              sum(.data$has_covid_safety_education),
            "n adults actively infected today" =
              sum(.data$active_infection | (.data$infection_date == cur_date), na.rm = TRUE),
            "n household adults infected (cumulative)" = 
              sum(.data$infected, na.rm = TRUE),
            
            "n household adults infected from exogenous sources (cumulative)" = 
              sum(.data$infection_source == "exogenous", na.rm = TRUE),
            
            "n household adults newly infected today" = 
              sum(.data$infected & .data$infection_date == cur_date, na.rm = TRUE),
            
            "n household adults infected by student today" = 
              sum(.data$infected & .data$infection_date == cur_date & .data$infection_source == "student", na.rm = TRUE),
            
            "n household adults infected from exogenous source today" = 
              sum(.data$infected & .data$infection_date == cur_date & .data$infection_source == "exogenous", na.rm = TRUE),
            
            "n household adults infected since baseline (cumulative)" = 
              sum(.data$infected & !.data$infected_at_baseline),
            "n household adults infected by other household adult (cumulative)" = 
              sum(.data$infected & .data$infection_source == "other household adult"),
            "n household adults infected by student (cumulative)" = 
              sum(.data$infected & .data$infection_source == "student")
          )
        
        cols_to_overwrite = 
          setdiff(
            names(adult_infection_counts_by_household), 
            "household_ID")
        
        households %<>%
          select(-any_of(cols_to_overwrite)) %>%
          left_join(
            adult_infection_counts_by_household,
            by = "household_ID")
        
        household_data_by_class = 
          households %>%
          summarise(
            .by = "class",
            "n households with positive attestations today" = 
              sum(.data$household_attestation_positive),
            
            "n households associated with class" = n(),
            across(
              any_of(c(cols_to_overwrite, "n_adults_in_household", "received_outreach_after_positive_attestation")), 
              sum)
          ) %>%
          rename(
            "n_household_adults_associated_with_class" = "n_adults_in_household",
            "n households that have received outreach" = "received_outreach_after_positive_attestation")
        
        cols_to_overwrite3 = 
          setdiff(
            names(household_data_by_class), 
            "class")
        
        classes %<>%
          select(-any_of(cols_to_overwrite3)) %>%
          left_join(
            by = "class",
            household_data_by_class)
        
        student_infection_counts_by_household = 
          students %>%
          summarize(
            .by = "household_ID",
            "n students actively infected today" = 
              sum(.data$active_infection | (.data$infection_date == cur_date), na.rm = TRUE))
        
        cols_to_overwrite2 = 
          setdiff(
            names(student_infection_counts_by_household), 
            "household_ID")
        
        households %<>%
          select(-any_of(cols_to_overwrite2)) %>%
          left_join(
            student_infection_counts_by_household,
            by = "household_ID")
        
        student_infection_counts_by_class = 
          students %>% 
          summarise(
            .by = "class",
            
            "n students educated about COVID safety (cumulative)" = 
              sum(.data$has_covid_safety_education),
            
            "n students newly infected today" = 
              sum(.data$infected & .data$infection_date == cur_date),
            
            "n_students_infected_cumulative" = 
              sum(.data$infected),
            
            "n students newly infected from exogenous source today" = 
              sum(.data$infected & .data$infection_date == cur_date & .data$infection_source == "exogenous", na.rm = TRUE),
            
            "n_students_infected_since_baseline_cumulative" = 
              sum(.data$infected & !.data$infected_at_baseline),
            
            "n students infected from exogenous sources (cumulative)" = 
              sum(.data$infection_source == "exogenous", na.rm = TRUE),
            
            "n students infected by household adult (cumulative)" = 
              sum(.data$infection_source == "household adult", na.rm = TRUE),
            
            "n_students_infected_outside_school_cumulative" = 
              sum(is.element(.data$infection_source,  c("household adult", "exogenous")), na.rm = TRUE),
            
            "n_students_infected_at_home_cumulative" = 
              sum(is.element(.data$infection_source,  c("household adult")), na.rm = TRUE),
            
            "n_transmissions_in_class_cumulative" = 
              sum(.data$infection_source == "from school", na.rm = TRUE))
        
        vars_to_overwrite = 
          setdiff(names(student_infection_counts_by_class), "class")
        
        classes %<>% 
          select(-any_of(vars_to_overwrite)) %>%
          left_join(
            by = "class",
            student_infection_counts_by_class)
        
        
      }
      
      student_records %<>% 
        bind_rows(
          students %>% 
            select(c("ID", "class", "in_school_today"))%>%
            mutate(date = cur_date))
      
      class_records %<>% bind_rows(
        calendar[i, ] %>% bind_cols(classes))
      
    }
    
  }
  
  to_return = list(
    class_records = class_records,
    inputs = inputs)
  
  message("Ending simulation at ", Sys.time())
  
  if(browse_before_return) browser()
  
  return(to_return)
  
}