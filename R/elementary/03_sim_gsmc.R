# install redist_gsmc before running this

nsims <- 2500
nruns <- 1L

constr <- redist_constr(map) %>%
add_constr_commute(
  strength = 1,
  current = map$elem_current,
  commute_times = commute_times,
  only_districts = TRUE
  ) %>%
add_constr_plan_incumbency(
  strength = 4,
  incumbents = schools_idx
  ) %>%
  add_constr_capacity(
    strength = 10,
    schools = schools_idx,
    schools_capacity = schools_capacity,
    only_districts = TRUE
  )

set.seed(2025)
plans <- redist_smc(
  map,
  nsims = nsims,
  runs = nruns,
  ncores = 64,
  constraints = constr,
  #pop_temper = 0.01,
  sampling_space = "linking_edge",
  ms_params = list(frequency = 1L, mh_accept_per_smc = 10),
  split_params = list(splitting_schedule = "any_valid_sizes"),
  verbose = TRUE
)

# keep only plans where each school is in a distinct district
plans <- drop_duplicate_schools(plans, schools_idx)

# add reference plans
plans <- plans %>%
  add_reference(map$elem_current, "elem_current") %>%
  add_reference(map$elem_scenario5, "elem_scenario5") %>%
  add_reference(map$elem_scenario4, "elem_scenario4") %>%
  add_reference(map$elem_scenario3, "elem_scenario3") %>%
  add_reference(map$elem_scenario2, "elem_scenario2")

write_rds(plans, here("data-raw/elem/plans/plans_es_gsmc_com1_inc4_cap10_pop0.66.rds"), compress = "gz")
  