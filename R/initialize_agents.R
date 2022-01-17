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
      "recovered" = ID %in% 
        sample(
          replace = FALSE,
          x = ID,
          size = round(n() * pr_recovered_baseline)),
      
      "vaccinated" = ID %in% 
        sample(
          replace = FALSE,
          x = ID[!recovered],
          size = round(sum(!recovered) * pr_vaccinated_baseline)),
      
      "immune" = recovered | vaccinated,
      
      "active_infection" = 
        ID %in%
        sample(
          replace = FALSE,
          size = round(n() * pr_active_infection_baseline),
          x = ID[!immune]),
      
      "infected_at_baseline" = recovered | active_infection,
      
      "infected" = infected_at_baseline,
      
      infection_date = 
        if_else(
          recovered,
          as.Date(baseline_collection_date) - days(15),      
          if_else(
            active_infection,
            sample(
              baseline_infection_date_distribution, 
              size = n(), 
              replace = TRUE),
            as.Date(NA))),
      
      "infection_source" = if_else(
        infected,
        "infected at baseline",
        as.character(NA)),
      
      # recovered = days_since_infected > max(last_symptomatic_day, last_infectious_day),
      
      "quarantine_end_date" = as.Date(NA),
      
      
      "has_covid_safety_education" = 
        ID %in%
        sample(
          replace = FALSE,
          x = ID,
          size = round(n() * baseline_prevalence_of_covid_safety_education)),
      
      "receptive_to_outreach" =
        has_covid_safety_education | 
        ID %in%
        sample(
          replace = FALSE,
          x = ID,
          size = round(n() * pr_receptive_to_outreach)),
      
      "symptomatic_if_infected" = 
        ID %in%
        sample(
          replace = FALSE,
          x = ID,
          size = round(n() * pr_symptomatic_if_infected))
      
    )
  
  return(agents)
  
}