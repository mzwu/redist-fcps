plans <- add_summary_stats(plans, map, map$elem25, schools_idx, commute_times, "elem")
validate_analysis(plans, map)
summary(plans)