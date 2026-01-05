plans <- add_summary_stats(
  plans, map, 
  current = map$middle_scenario4, 
  schools = schools_idx, 
  commute_times = commute_times, 
  capacity = schools_capacity,
  level = "middle"
)
validate_analysis(plans, map, "middle")
summary(plans)