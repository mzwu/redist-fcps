# install redist_gsmc before running this

nsims <- 1000
nruns <- 1L
set.seed(2025)

constr <- redist_constr(map) %>%
  add_constr_commute(
    strength = 1,
    current = map$middle_current,
    commute_times = commute_times,
    only_districts = TRUE
  ) %>%
  add_constr_incumbency(
    strength = 11,
    incumbents = schools_idx
  ) %>%
  add_constr_capacity(
    strength = 1,
    schools = schools_idx,
    schools_capacity = schools_capacity,
    only_districts = TRUE
  )

simulate_plans <- function(map, draws, nsims, nruns) {
  for (i in 1:length(draws)) {
    starter_name <- paste0("elem_starter", i)
    
    plans <- redist_smc(
      map,
      nsims = nsims, runs = nruns,
      ncores = 64,
      counties = map[[starter_name]],
      constraints = constr,
      #pop_temper = 0.01,
      sampling_space = "linking_edge",
      ms_params = list(frequency = 1L, mh_accept_per_smc = 10),
      split_params = list(splitting_schedule = "any_valid_sizes"),
      verbose = T
    ) %>%
      subset_sampled()
    
    # calculate split feeders
    plans <- add_split_feeder_stats(plans, map, starter_name, "middle")
    
    path <- paste0("data-raw/middle/plans/plans_ms_", i, ".rds")
    write_rds(plans, here(path), compress = "gz")
  }
}

simulate_plans(map, draws_init, nsims, nruns)

# combine 3 sets of plans
plans1 <- read_rds(here("data-raw/middle/plans/plans_ms_1.rds"))
plans2 <- read_rds(here("data-raw/middle/plans/plans_ms_2.rds"))
plans3 <- read_rds(here("data-raw/middle/plans/plans_ms_3.rds"))
plans <- rbind(plans1,
               plans2 %>% subset_sampled() %>% mutate(draw = factor(as.integer(draw) + 1 * nsims)),
               plans3 %>% subset_sampled() %>% mutate(draw = factor(as.integer(draw) + 2 * nsims)))


# thin plans
# n_thin <- 2500
# thin_draws <- sample(unique(as.integer(plans$draw)), n_thin)
# plans <- plans %>%
#   filter(as.integer(draw) %in% thin_draws)
# plans$draw <- factor(
#   as.integer(plans$draw),
#   labels = seq_along(levels(plans$draw))
# )

plans_ref <- plans %>% filter(draw == "ref") %>%
  add_reference(map$middle_current, "middle_current") %>%
  add_reference(map$middle_scenario5, "middle_scenario5") %>%
  add_reference(map$middle_scenario4, "middle_scenario4") %>%
  add_reference(map$middle_scenario3, "middle_scenario3") %>%
  add_reference(map$middle_scenario2, "middle_scenario2") %>%
  add_split_feeder_stats(map, "elem_scenario5", "middle")

plans <- rbind(plans_ref, plans)

write_rds(plans, here("data-raw/middle/plans/plans_ms_com1_inc11_split0_cap1_pop0.2.rds"), compress = "gz")