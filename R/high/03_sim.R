nsims <- 2500
nruns <- 1L

simulate_plans <- function(map, draws, nsims, nruns) {
  for (i in 1:length(draws)) {
    starter_name <- paste0("middle_starter", i)
    
    constr <- redist_constr(map) %>%
      # add_constr_commute(
      #   strength = 1,
      #   current = map$high25,
      #   commute_times = commute_times,
      #   only_districts = TRUE
      # ) %>%
      add_constr_incumbency(
        strength = 1,
        incumbents = schools_idx,
        #only_districts = TRUE
        # ) %>%
        # add_constr_split_feeders(
        #   strength = 1,
        #   lower = map[[starter_name]],
        #   schools = schools_idx,
        #   only_districts = TRUE
        # ) %>%
        # add_constr_capacity(
        #   strength = 1,
        #   schools = schools_idx,
        #   schools_capacity = schools_capacity,
        #   only_districts = TRUE
      )
    plans <- redist_smc(
      map,
      nsims = nsims, runs = nruns,
      ncores = 60,
      counties = starter_name,
      #constraints = constr,
      #pop_temper = 0.01,
      #sampling_space = "linking_edge",
      #ms_params = list(frequency = 1L, mh_accept_per_smc = 10),
      #split_params = list(splitting_schedule = "any_valid_sizes"),
      verbose = T
    )
    
    # keep only plans where each school is in a distinct district
    # plans <- drop_duplicate_schools(plans, schools_idx)
    
    # TODO: calculate split feeders here?
    
    path <- paste0("data-raw/high/plans/plans_hs_", i, ".rds")
    write_rds(plans, here(path), compress = "gz")
  }
}

simulate_plans(map, draws5, nsims, nruns)

# combine 3 sets of plans
plans1 <- read_rds(here("data-raw/high/plans/plans_hs_1.rds"))
plans2 <- read_rds(here("data-raw/high/plans/plans_hs_2.rds"))
plans3 <- read_rds(here("data-raw/high/plans/plans_hs_3.rds"))
plans <- rbind(plans1,
               plans2 %>% subset_sampled() %>% mutate(draw = factor(as.integer(draw) + 1 * nsims)),
               plans3 %>% subset_sampled() %>% mutate(draw = factor(as.integer(draw) + 2 * nsims)))

# thin plans
n_thin <- 2500
thin_draws <- sample(unique(as.integer(plans$draw)), n_thin)
plans <- plans %>%
  filter(as.integer(draw) %in% thin_draws)
plans$draw <- factor(plans$draw, levels = sort(unique(plans$draw)))
plans <- plans %>%
  add_reference(map$high_scenario5, "high_scenario5") %>%
  add_reference(map$high_scenario4, "high_scenario4") %>%
  add_reference(map$high_scenario3, "high_scenario3") %>%
  add_reference(map$high_scenario2, "high_scenario2")

write_rds(plans, here("data-raw/high/plans/plans_hs.rds"))
