# make constraint object
constr <- redist_constr(map) %>%
  add_constr_phase_commute(
    strength = 1,
    current = map$high25,
    schools = schools_idx,
    commute_times = commute_times
  ) %>%
  add_constr_incumbency(
    strength = 10,
    incumbents = schools_idx
  ) %>%
  add_constr_custom(strength = 1, function(plan, distr){
    # custom constraint for capacity utilization
    # get vector of district assignments for this plan
    # get row numbers of redist_map that are in this district
    rows_in_district <- which(plan == distr)

    # get the index of schools_idx of the element that is in the district map rows
    idx_in_schools <- which(schools_idx %in% rows_in_district)[1]

    # get school name
    school_name <- ffx_hs$OBJECTID[idx_in_schools]

    # calculate the proportion of total population that this school would theoretically have
    target_ratio <- ffx_high$prop_capacity[ffx_high$object_id_school == school_name][1L]
    if (is.na(target_ratio)) return(1000)
    actual_ratio <- sum(map$pop[which(plan == distr)]) / sum(map$pop)
    error <- 0.01

    if (abs(target_ratio - actual_ratio) <= error) {
      return(0)
    } else {
      return(1000)
    }
    }
  )

# simulate plans
plans <- redist_smc(map, nsims = 2500,
                    counties = middle25,
                    constr = constr)


# remotes::install_github("philipwosull/redist@gredist", INSTALL_opts="--preclean")

# constr <- redist_constr(map) %>%
#   add_constr_phase_commute(
#     strength = 1,
#     current = map$high25,
#     schools = schools_idx,
#     commute_times = commute_times,
#     total_pop = pop,
#     score_districts_only = TRUE
#   ) %>%
#   add_constr_incumbency(
#     strength = 10,
#     incumbents = schools_idx
#   ) %>%
#   add_constr_custom(strength = 1, function(plan, distr){
#     # custom constraint for capacity utilization
#     # get vector of district assignments for this plan
#     # get row numbers of redist_map that are in this district
#     rows_in_district <- which(plan == distr)
# 
#     # get the index of schools_idx of the element that is in the district map rows
#     idx_in_schools <- which(schools_idx %in% rows_in_district)[1]
# 
#     # get school name
#     school_name <- ffx_hs$OBJECTID[idx_in_schools]
# 
#     # calculate the proportion of total population that this school would theoretically have
#     target_ratio <- ffx_high$prop_capacity[ffx_high$object_id_school == school_name][1L]
#     if (is.na(target_ratio)) return(1000)
#     actual_ratio <- sum(map$pop[which(plan == distr)]) / sum(map$pop)
#     error <- 0.01
# 
#     if (abs(target_ratio - actual_ratio) <= error) {
#       return(0)
#     } else {
#       return(1000)
#     }
#   }
#   )
# 
# plans <- redist_smc(
#   map,
#   nsims = 5000, runs = 2L,
#   counties = middle25,
#   constraints = constr,
#   sampling_space = "linking_edge",
#   ms_params = list(frequency = 1L, mh_accept_per_smc = 12),
#   split_params = list(splitting_schedule = "any_valid_sizes"),
#   verbose = T
# )

plans <- match_numbers(plans, "high25")

# keep only plans where each school is in a distinct district
plans <- drop_duplicate_schools(plans, schools_idx)

write_rds(plans, here("data-raw/high25/plans/plans_hs_noconstr.rds"), compress = "gz")
