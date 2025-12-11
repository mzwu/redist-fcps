plans <- add_summary_stats(plans, map, map$middle25, schools_idx, commute_times, "middle")
validate_analysis(plans, map)
summary(plans)