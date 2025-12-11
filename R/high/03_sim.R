constr <- redist_constr(map) %>%
  add_constr_phase_commute(
    strength = 1,
    current = map$high25,
    schools = schools_idx,
    commute_times = commute_times,
    only_districts = TRUE
  ) %>%
  add_constr_incumbency(
    strength = 999999,
    incumbents = schools_idx,
    only_districts = TRUE
  ) %>%
  add_constr_split_feeders(
    strength = 1,
    lower = map$middle25,
    schools = schools_idx,
    only_districts = TRUE
  ) %>%
  add_constr_capacity(
    strength = 1,
    schools = schools_idx,
    schools_capacity = schools_capacity,
    only_districts = TRUE
  )

plans <- redist_smc(
  map,
  nsims = 2500, runs = 1L,
  ncores = 40,
  #counties = tractce20,
  #constraints = constr,
  pop_temper = 0.05,
  sampling_space = "linking_edge",
  ms_params = list(frequency = 1L, mh_accept_per_smc = 10),
  split_params = list(splitting_schedule = "any_valid_sizes"),
  verbose = T
)

plans <- match_numbers(plans, "high25")

# keep only plans where each school is in a distinct district
plans <- drop_duplicate_schools(plans, schools_idx)

write_rds(plans, here("data-raw/high25/plans/plans_ms_noconstr.rds"), compress = "gz")
