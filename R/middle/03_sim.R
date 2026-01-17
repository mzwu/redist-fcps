# install redist_gsmc before running this

nsims <- 2500
nruns <- 1L

constr <- redist_constr(map) %>%
  add_constr_commute(
    strength = 1,
    current = map$middle_current,
    commute_times = commute_times
  ) %>%
  add_constr_incumbency(
    strength = 1,
    incumbents = schools_idx
  ) %>%
  add_constr_split_feeders(
    strength = 1,
    lower = map$elem_scenario5,
    schools = schools_idx
  ) %>%
  add_constr_capacity(
    strength = 1,
    schools = schools_idx,
    schools_capacity = schools_capacity
  )

plans <- redist_smc(
  map,
  nsims = nsims, runs = nruns,
  ncores = 60,
  counties = map$elem_scenario5,
  constraints = constr,
  #pop_temper = 0.01,
  sampling_space = "linking_edge",
  ms_params = list(frequency = 1L, mh_accept_per_smc = 10),
  split_params = list(splitting_schedule = "any_valid_sizes"),
  verbose = T
)

# thin plans
n_thin <- 2500
thin_draws <- sample(unique(as.integer(plans$draw)), n_thin)
plans <- plans %>%
  filter(as.integer(draw) %in% thin_draws)
plans$draw <- factor(
  as.integer(plans$draw),
  labels = seq_along(levels(plans$draw))
)
plans <- plans %>%
  add_reference(map$middle_scenario5, "middle_scenario5") %>%
  add_reference(map$middle_scenario4, "middle_scenario4") %>%
  add_reference(map$middle_scenario3, "middle_scenario3") %>%
  add_reference(map$middle_scenario2, "middle_scenario2")

write_rds(plans, here("data-raw/middle/plans/plans_ms.rds"))
