# install redist_gsmc before running this

nsims <- 1000
nruns <- 1L
set.seed(2025)

simulate_plans <- function(map, draws, nsims, nruns) {
  for (i in 1:length(draws)) {
    starter_name <- paste0("middle_starter", i)
    
    constr <- redist_constr(map) %>%
      # add_constr_commute(
      #   strength = 1,
      #   current = map$high_current,
      #   commute_times = commute_times,
      #   only_districts = TRUE
      # ) %>%
      add_constr_incumbency(
        strength = 15,
        incumbents = schools_idx
      ) %>%
      add_constr_split_feeders(
        strength = 4,
        lower = map[[starter_name]],
        schools = schools_idx,
        only_districts = TRUE
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
    plans <- add_split_feeder_stats(plans, map, starter_name, "high")
    
    path <- paste0("data-raw/high/plans/plans_hs_", i, ".rds")
    write_rds(plans, here(path), compress = "gz")
  }
}

simulate_plans(map, draws_init, nsims, nruns)

# combine 3 sets of plans
plans1 <- read_rds(here("data-raw/high/plans/plans_hs_1.rds"))
plans2 <- read_rds(here("data-raw/high/plans/plans_hs_2.rds"))
plans3 <- read_rds(here("data-raw/high/plans/plans_hs_3.rds"))
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
  add_reference(map$high_current, "high_current") %>%
  add_reference(map$high_scenario5, "high_scenario5") %>%
  add_reference(map$high_scenario4, "high_scenario4") %>%
  add_reference(map$high_scenario3, "high_scenario3") %>%
  add_reference(map$high_scenario2, "high_scenario2") %>%
  add_split_feeder_stats(map, "middle_scenario5", "high")

plans <- rbind(plans_ref, plans)

write_rds(plans, here(paste0("data-raw/high/plans/plans_hs_com0_inc15_split4_cap0_pop0.25.rds")))
