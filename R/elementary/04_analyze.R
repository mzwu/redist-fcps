plans <- add_summary_stats(
  plans, map, 
  current = map$elem_current, 
  schools = schools_idx, 
  commute_times = commute_times, 
  capacity = schools_capacity,
  level = "elem"
)
validate_analysis(plans, map, "elem")
summary(plans)

cutoff <- plans %>%
  filter(draw == "elem_scenario5") %>%
  pull(disrupt_count) %>%
  unique()
plans_test <- plans %>%
  filter(disrupt_count <= cutoff)
