constr <- redist_constr(map) %>%
  # add_constr_phase_commute(
  #   strength = 1,
  #   current = map$elem25,
  #   schools = schools_idx,
  #   commute_times = commute_times
  # ) %>%
  add_constr_incumbency(
    strength = 99,
    incumbents = schools_idx
  # ) %>%
  # add_constr_capacity(
  #   strength = 1,
  #   schools = schools_idx,
  #   schools_capacity = schools_capacity
  )

plans <- redist_mergesplit(
  map,
  nsims = 2500,
  ncores = 60,
  constraints = constr,
  init_plan = map$init_plan,
  verbose = T
)

# plans <- redist_smc(
#   map,
#   nsims = 2500, runs = 1L,
#   ncores = 60,
#   counties = tractce20,
#   constraints = constr,
#   pop_temper = 0.05,
#   sampling_space = "linking_edge",
#   ms_params = list(frequency = 1L, mh_accept_per_smc = 10),
#   split_params = list(splitting_schedule = "any_valid_sizes"),
#   verbose = T
# )

plans <- plans %>%
  add_reference(map$elem25, "elem25")

plans <- match_numbers(plans, "elem25")

# keep only plans where each school is in a distinct district
#plans <- drop_duplicate_schools(plans, schools_idx)

write_rds(plans, here("data-raw/elem25/plans/plans_es_noconstr_incumbent99.rds"), compress = "gz")
