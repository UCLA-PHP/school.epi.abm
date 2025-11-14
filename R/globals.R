# Declare all data.table column names and variables used in NSE
utils::globalVariables(c(
  # Core variables
  "ID", "infected", "immune", "in_school_today", "infection_date",
  "test_result", "collection_date", "result_date", "test_type",
  "household_ID", "quarantine_end_date", "active_infection",
  "infectious_today", "tested_positive_today",
  
  # Variables with spaces (exact names from your data.table operations)
  "n household adults infected (cumulative)",
  "n households with positive attestations today"
))

# Mark package as data.table aware
.datatable.aware <- TRUE
