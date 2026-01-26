plans <- add_summary_stats(
  plans, map, 
  current = map$middle_current, 
  schools = schools_idx, 
  commute_times = commute_times, 
  capacity = schools_capacity,
  level = "middle"
)
validate_analysis(plans, map, "middle")
summary(plans)

plans_thin <- plans %>%
  filter(school_outside_zone == 0)
write_rds(plans, here(paste0("data-raw/middle/plans/plans_ms_com1_inc11_split0_cap1_pop0.2_thin0.rds")))
