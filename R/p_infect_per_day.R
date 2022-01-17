p_infect_per_day = function(p_infect, n_days_infectious)
{
  1 - (1 - p_infect)^(1/n_days_infectious)
}
