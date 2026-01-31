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

plans_test <- plans %>%
  filter(school_outside_zone <= 1, elem_split_feeders <= 17) %>%
  group_by(draw) %>% 
  mutate(cap = max(capacity_util),
         com = max(max_commute)) %>% 
  filter(cap <= 1.360817,
         com <= 64.81667) %>% 
  ungroup()

projected_average_heatmap(plans, map, schools_idx, commute_times, "middle")
current_commute_heatmap(plans, map, schools_idx, commute_times, "middle")
