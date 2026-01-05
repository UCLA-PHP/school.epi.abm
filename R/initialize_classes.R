initialize_classes = function(
  schools,
  n_students_per_class)
{
  
  classes = 
    schools %>%
    group_by("school") %>%
    summarise(
      .groups = "drop", 
      class = paste(.data$school, 1:.data$n_classes_in_school, sep = ":")) %>% 
    mutate(
      n_students_on_roster = n_students_per_class,
      n_outbreaks = 0,
      last_detected_cluster_date = as.Date(NA),
      class_quarantined_today = FALSE,
      quarantine_end_date = as.Date(NA))
  
}