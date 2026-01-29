plans <- add_summary_stats(
  plans, map, 
  current = map$high_current, 
  schools = schools_idx, 
  commute_times = commute_times, 
  capacity = schools_capacity,
  level = "high"
)
validate_analysis(plans, map, "high")
summary(plans)

projected_average_heatmap(plans, map, schools_idx, commute_times, "high")
current_commute_heatmap(plans, map, schools_idx, commute_times, "high")
