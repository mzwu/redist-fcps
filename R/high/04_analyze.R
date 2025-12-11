plans <- add_summary_stats(plans, map, map$high25, schools_idx, commute_times, "high")
validate_analysis(plans, map)
summary(plans)
