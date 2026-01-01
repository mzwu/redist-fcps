nsims <- 2500
nruns <- 1L

constr <- redist_constr(map) %>%
  # add_constr_phase_commute(
  #   strength = 10,
  #   current = map$elem25,
  #   commute_times = commute_times,
  #   only_districts = TRUE
  # ) %>%
  add_constr_plan_incumbency(
    strength = 5,
    incumbents = schools_idx
  # ) %>%
  # add_constr_capacity(
  #   strength = 10,
  #   schools = schools_idx,
  #   schools_capacity = schools_capacity,
  #   only_districts = TRUE
  )

set.seed(2025)
plans <- redist_smc(
  map,
  nsims = nsims,
  runs = nruns,
  ncores = 64,
  #counties = tractce20,
  constraints = constr,
  #pop_temper = 0.01,
  #sampling_space = "linking_edge",
  sampling_space = "graph_plan",
  ms_params = list(frequency = 1L, mh_accept_per_smc = 10),
  split_params = list(splitting_schedule = "any_valid_sizes"),
  verbose = TRUE
)

# plans <- plans %>%
#   add_reference(map$elem25, "elem25")

plans <- match_numbers(plans, "elem25")

# TODO: thin samples

# TODO: keep only plans where each school is in a distinct district
#plans <- drop_duplicate_schools(plans, schools_idx)

write_rds(plans, here("data-raw/elem25/plans/plans_es_inc5_poptol0.7.rds"), compress = "gz")
