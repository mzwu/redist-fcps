# make constraint object
constr <- redist_constr(map) %>%
  # add_constr_phase_commute(
  #   strength = 1,
  #   current = map$middle25,
  #   schools = schools_idx,
  #   commute_times = commute_times
  # ) %>%
  add_constr_incumbency(
    strength = 999999,
    incumbents = schools_idx
  # ) %>%
  # add_constr_capacity(
  #   strength = 1,
  #   schools_idx,
  #   schools_capacity
  )

plans <- redist_smc(map, nsims = 2500,
                    counties = elem25,
                    constr = constr
                    )

# GSMC VERSION
# constr <- redist_constr(map) %>%
#   add_constr_phase_commute(
#     strength = 0.1,
#     current = map$middle25,
#     schools = schools_idx,
#     commute_times = commute_times,
#     only_districts = TRUE
#   )
# 
# plans <- redist_smc(
#   map,
#   nsims = 2500, runs = 1L,
#   #constraints = constr,
#   pop_temper = 0.1,
#   sampling_space = redist:::FOREST_SPACE_SAMPLING,
#   ms_params = list(frequency = 1L, mh_accept_per_smc = 13),
#   split_params = list(splitting_schedule = "any_valid_sizes"),
#   split_method = "unif_valid",
#   verbose = T
# )

plans <- match_numbers(plans, "middle25")

# keep only plans where each school is in a distinct district
# plans <- drop_duplicate_schools(plans, schools_idx)

write_rds(plans, here("data-raw/middle25/plans_ms_noconstr.rds"), compress = "gz")
