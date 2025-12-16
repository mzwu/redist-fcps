plans <- add_summary_stats(
  plans, map, 
  current = map$middle25, 
  schools = schools_idx, 
  commute_times = commute_times, 
  capacity = schools_capacity,
  level = "middle"
)
validate_analysis(plans, map)
summary(plans)