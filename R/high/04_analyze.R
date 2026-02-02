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

cutoff <- plans %>%
  filter(draw == "high_scenario5") %>%
  pull(disrupt_count) %>%
  unique()
plans_test <- plans %>%
  filter(school_outside_zone <= 0, middle_split_feeders <= 6) %>%
  group_by(draw) %>% 
  mutate(cap = max(capacity_util),
         com = max(max_commute)) %>% 
  filter(cap <= 1.618551,
         com <= 65.505) %>% 
  ungroup()

projected_average_heatmap(plans, map, schools_idx, commute_times, "high")
current_commute_heatmap(plans, map, schools_idx, commute_times, "high")
comparison_boxplots(plans %>% subset_sampled(), map, schools_idx, commute_times, "high")
