plans <- add_summary_stats(
  plans, map, 
  current = map$elem_scenario4, 
  schools = schools_idx, 
  commute_times = commute_times, 
  capacity = schools_capacity,
  level = "elem"
)
validate_analysis(plans, map, "elem")
summary(plans)