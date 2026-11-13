# Run the agent-based model

Simulates COVID-19 transmission among students in K-6 classrooms and the
adults in their households, under a chosen combination of surveillance
testing, symptom attestation, quarantine, and COVID-safety education
outreach. The model is described in Morrison et al. (2023),
[doi:10.3389/fpubh.2023.856940](https://doi.org/10.3389/fpubh.2023.856940)
.

## Usage

``` r
run_simulation(
  n_schools = 1000,
  n_grades_per_school = 7,
  n_classes_per_school = n_grades_per_school * ceiling(60/n_students_per_class),
  n_students_per_class = 15,
  n_weeks_in_time_window = 4 * 2,
  pr_student_infected_per_infectious_household_adult_per_day = p_infect_per_day(p_infect
    = 0.16, n_days_infectious = n_days_infectious),
  pr_household_adult_infected_by_infectious_student_per_day = p_infect_per_day(p_infect =
    0.4, n_days_infectious = n_days_infectious),
  pr_household_adult_infected_per_infectious_household_adult_per_day =
    p_infect_per_day(p_infect = 0.4, n_days_infectious = n_days_infectious),
  pr_student_symptomatic_if_infected = 0.5,
  pr_adult_symptomatic_if_infected = 0.7,
  test_specificity = 0.999,
  test_sensitivity_day_0 = 1 - test_specificity,
  test_sensitivity_day_1 = 1 - test_specificity,
  `test sensitivity day 2` = 1 - test_specificity,
  `test sensitivity day 3` = 0.5,
  `test sensitivity day 4` = 0.7,
  `peak test sensitivity` = 0.95,
  `days of peak test sensitivity` = days(3),
  `test sensitivity on last day of symptoms` = 0.5,
  `daily decrease in sensitivity from peak until recovery` =
    (`test sensitivity on last day of symptoms` - `peak test sensitivity`) *
    (days(1)/(last_active_infection_day - (days(4) + `days of peak test sensitivity`))),
  `decrease in sensitivity per day after recovery` = 0.1,
  attestation_sensitivity_symptomatic = 0.9,
  attestation_sensitivity_asymptomatic = max(0.1, 1 - attestation_specificity),
  attestation_sensitivity_presymptomatic = attestation_sensitivity_asymptomatic,
  attestation_specificity = 0.999,
  baseline_prevalence_of_covid_safety_education = 0.1,
  pr_become_educated_after_outreach = 0.5,
  baseline_prevalence_adults = 0.006,
  baseline_prevalence_students = baseline_prevalence_adults,
  baseline_cumulative_incidence_adults = 0.04,
  baseline_cumulative_incidence_students = baseline_cumulative_incidence_adults,
  baseline_recovered_adults = baseline_cumulative_incidence_adults -
    baseline_prevalence_adults,
  baseline_recovered_students = baseline_cumulative_incidence_students -
    baseline_prevalence_students,
  baseline_vaccinated_adults = 0,
  baseline_vaccinated_students = 0,
  increase_in_accuracy_after_education = 0.1,
  decrease_in_exogenous_risk_after_education = 0.1,
  adult_exogenous_infection_risk_per_day_without_education = 4e-04,
  adult_exogenous_infection_risk_per_day_with_education =
    adult_exogenous_infection_risk_per_day_without_education * (1 -
    decrease_in_exogenous_risk_after_education),
  student_exogenous_infection_risk_per_day_without_education =
    adult_exogenous_infection_risk_per_day_without_education,
  student_exogenous_infection_risk_per_day_with_education =
    student_exogenous_infection_risk_per_day_without_education * (1 -
    decrease_in_exogenous_risk_after_education),
  risk_per_infected_close_contact = 0.001,
  risk_per_infected_classmate = 5e-04,
  testing_fraction = 0.25,
  first_infectious_day = days(3),
  last_infectious_day = days(12),
  n_days_infectious = (last_infectious_day - first_infectious_day)/days(1) + 1,
  first_symptomatic_day = days(5),
  last_symptomatic_day = days(14),
  last_active_infection_day = days(14),
  quarantine_length_after_adult_positive_test = days(14),
  quarantine_length_after_student_positive_test = days(14),
  quarantine_length_after_positive_attestation = days(3),
  quarantine_length_after_outbreak = days(14),
  in_person_school_start_date = as.Date("2021-01-04"),
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
```

## Arguments

- n_schools:

  number of schools to simulate

- n_grades_per_school:

  number of grades in each school

- n_classes_per_school:

  number of classes in each school

- n_students_per_class:

  number of students enrolled in each class

- n_weeks_in_time_window:

  number of weeks to simulate after in-person school starts

- pr_student_infected_per_infectious_household_adult_per_day:

  daily probability that a student is infected by each infectious adult
  in their household

- pr_household_adult_infected_by_infectious_student_per_day:

  daily probability that a household adult is infected by an infectious
  student in the household

- pr_household_adult_infected_per_infectious_household_adult_per_day:

  daily probability that a household adult is infected by each other
  infectious adult in the household

- pr_student_symptomatic_if_infected:

  probability that an infected student develops symptoms

- pr_adult_symptomatic_if_infected:

  probability that an infected adult develops symptoms

- test_specificity:

  probability that a test of an uninfected person is negative

- test_sensitivity_day_0:

  probability of a positive test on the day of infection

- test_sensitivity_day_1:

  probability of a positive test one day after infection

- test sensitivity day 2:

  probability of a positive test two days after infection

- test sensitivity day 3:

  probability of a positive test three days after infection

- test sensitivity day 4:

  probability of a positive test four days after infection

- peak test sensitivity:

  the maximum test sensitivity achieved during an infection, reached
  after day 4

- days of peak test sensitivity:

  how long test sensitivity stays at its peak

- test sensitivity on last day of symptoms:

  test sensitivity on `last_active_infection_day`

- daily decrease in sensitivity from peak until recovery:

  daily change in test sensitivity between the end of the peak and
  `last_active_infection_day` (negative for a decrease)

- decrease in sensitivity per day after recovery:

  daily decrease in test sensitivity after `last_active_infection_day`

- attestation_sensitivity_symptomatic:

  probability that a household with a symptomatic member reports it on
  the daily attestation

- attestation_sensitivity_asymptomatic:

  probability of a positive attestation for an asymptomatic infection,
  representing attestations due to known exposures

- attestation_sensitivity_presymptomatic:

  probability of a positive attestation for a presymptomatic infection

- attestation_specificity:

  probability that a household with no infection gives a negative
  attestation

- baseline_prevalence_of_covid_safety_education:

  proportion of households that have COVID-safety education at the start
  of the simulation

- pr_become_educated_after_outreach:

  probability that a household becomes educated about COVID safety after
  receiving outreach

- baseline_prevalence_adults:

  proportion of adults actively infected at baseline

- baseline_prevalence_students:

  proportion of students actively infected at baseline

- baseline_cumulative_incidence_adults:

  proportion of adults ever infected by baseline

- baseline_cumulative_incidence_students:

  proportion of students ever infected by baseline

- baseline_recovered_adults:

  proportion of adults recovered (and immune) at baseline

- baseline_recovered_students:

  proportion of students recovered (and immune) at baseline

- baseline_vaccinated_adults:

  proportion of adults vaccinated (and immune) at baseline

- baseline_vaccinated_students:

  proportion of students vaccinated (and immune) at baseline

- increase_in_accuracy_after_education:

  proportional increase in attestation accuracy after COVID-safety
  education: the new accuracy is
  `a + (1 - a) * increase_in_accuracy_after_education`, where `a` is the
  accuracy before education

- decrease_in_exogenous_risk_after_education:

  proportional decrease in daily infection risk from outside the school
  and household after COVID-safety education

- adult_exogenous_infection_risk_per_day_without_education:

  daily probability that an adult without COVID-safety education is
  infected outside the school and household

- adult_exogenous_infection_risk_per_day_with_education:

  daily probability that an adult with COVID-safety education is
  infected outside the school and household

- student_exogenous_infection_risk_per_day_without_education:

  daily probability that a student without COVID-safety education is
  infected outside the school and household

- student_exogenous_infection_risk_per_day_with_education:

  daily probability that a student with COVID-safety education is
  infected outside the school and household

- risk_per_infected_close_contact:

  daily probability that a student is infected by each infectious close
  contact in their class

- risk_per_infected_classmate:

  daily probability that a student is infected by each infectious
  classmate

- testing_fraction:

  proportion of each school's enrolled students tested on each
  surveillance testing day

- first_infectious_day:

  time from infection until a person becomes infectious

- last_infectious_day:

  time from infection until a person stops being infectious

- n_days_infectious:

  number of days a person is infectious

- first_symptomatic_day:

  time from infection until symptoms start, for symptomatic infections

- last_symptomatic_day:

  time from infection until symptoms end, for symptomatic infections

- last_active_infection_day:

  time from infection until the infection resolves

- quarantine_length_after_adult_positive_test:

  how long a student quarantines after a household adult tests positive

- quarantine_length_after_student_positive_test:

  how long a student quarantines after testing positive

- quarantine_length_after_positive_attestation:

  how long a student quarantines after a positive attestation

- quarantine_length_after_outbreak:

  how long a class quarantines after an outbreak is detected

- in_person_school_start_date:

  the date in-person school starts (a
  [base::Date](https://rdrr.io/r/base/Dates.html))

- wait_time_for_surveillance_test_results:

  time from a surveillance test until its result is returned

- wait_time_for_attestation_triggered_test_results:

  time from a test triggered by a positive attestation until its result
  is returned

- wait_time_for_household_education_after_positive_attestation:

  time from a positive attestation until the household receives outreach

- wait_time_for_testing_after_outreach:

  time from outreach until the household is tested

- surveillance_testing_days:

  names of the weekdays on which surveillance testing happens, such as
  `"Monday"`

- days_from_baseline_collection_until_baseline_return:

  time from baseline test collection until baseline results are returned

- baseline_return_date:

  the date baseline test results are returned (a
  [base::Date](https://rdrr.io/r/base/Dates.html))

- baseline_collection_date:

  the date baseline tests are collected (a
  [base::Date](https://rdrr.io/r/base/Dates.html))

- n_students_recently_in_class_and_diagnosed_for_outbreak:

  number of students in one class diagnosed within `cluster_time_window`
  that counts as an outbreak

- cluster_time_window:

  time window for counting diagnoses toward an outbreak

- n_adults_per_household:

  number of adults in each student's household

- browse_at_start_of_days:

  if `TRUE`, call [`browser()`](https://rdrr.io/r/base/browser.html) at
  the start of each simulated day (for debugging)

- browse_before_return:

  if `TRUE`, call [`browser()`](https://rdrr.io/r/base/browser.html)
  before returning (for debugging)

- n_close_contacts_in_class:

  size of the close-contact groups that each class is divided into

- print_args:

  if `TRUE`, print every argument value at the start

- verbose:

  if `TRUE`, print progress messages

- plot_data:

  if `TRUE`, plot interim results each simulated school day

- plot3_ymax:

  minimum upper limit of the y-axis for the interim plot of student
  infection rates

- in_shiny:

  used to provide progress reporting to the dashboard when running the
  model through the Shiny GUI

## Value

a list with elements `class_records`, a data frame with one row per
class per simulated day, and `inputs`, a list of the argument values
used

## Details

Probabilities are on the 0–1 scale. Durations and offsets are
[`lubridate::days()`](https://lubridate.tidyverse.org/reference/period.html)
periods unless noted otherwise.
