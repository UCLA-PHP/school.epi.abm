initialize_agents = function(
  agents,
  baseline_prevalence_of_covid_safety_education,
  pr_receptive_to_outreach,
  pr_symptomatic_if_infected,
  pr_recovered_baseline,
  pr_vaccinated_baseline,
  pr_active_infection_baseline,
  baseline_infection_date_distribution,
  baseline_collection_date
)
{
  
  agents %<>% 
    mutate(
      "recovered" = .data$ID %in% 
        sample(
          replace = FALSE,
          x = .data$ID,
          size = round(n() * pr_recovered_baseline)),
      
      "vaccinated" = .data$ID %in% 
        sample(
          replace = FALSE,
          x = .data$ID[!.data$recovered],
          size = round(sum(!.data$recovered) * pr_vaccinated_baseline)),
      
      "immune" = .data$recovered | .data$vaccinated,
      
      "active_infection" = 
        .data$ID %in%
        sample(
          replace = FALSE,
          size = round(n() * pr_active_infection_baseline),
          x = .data$ID[!.data$immune]),
      
      "infected_at_baseline" = .data$recovered | .data$active_infection,
      
      "infected" = .data$infected_at_baseline,
      
      infection_date = 
        if_else(
          .data$recovered,
          as.Date(baseline_collection_date) - days(15),      
          if_else(
            .data$active_infection,
            sample(
              baseline_infection_date_distribution, 
              size = n(), 
              replace = TRUE),
            as.Date(NA))),
      
      "infection_source" = if_else(
        .data$infected,
        "infected at baseline",
        as.character(NA)),
      
      # recovered = days_since_infected > max(last_symptomatic_day, last_infectious_day),
      
      "quarantine_end_date" = as.Date(NA),
      
      
      "has_covid_safety_education" = 
        .data$ID %in%
        sample(
          replace = FALSE,
          x = .data$ID,
          size = round(n() * baseline_prevalence_of_covid_safety_education)),
      
      "receptive_to_outreach" =
        .data$has_covid_safety_education | 
        .data$ID %in%
        sample(
          replace = FALSE,
          x = .data$ID,
          size = round(n() * pr_receptive_to_outreach)),
      
      "symptomatic_if_infected" = 
        .data$ID %in%
        sample(
          replace = FALSE,
          x = .data$ID,
          size = round(n() * pr_symptomatic_if_infected))
      
    )
  
  return(agents)
  
}