nsims <- 2500
nruns <- 1L

constr <- redist_constr(map) %>%
  # add_constr_phase_commute(
  #   strength = 1,
  #   current = map$middle25,
  #   schools = schools_idx,
  #   commute_times = commute_times
  # ) %>%
  add_constr_incumbency(
    strength = 99,
    incumbents = schools_idx
  # ) %>%
  # add_constr_split_feeders(
  #   strength = 1,
  #   lower = map$elem25,
  #   schools = schools_idx
  # ) %>%
  # add_constr_capacity(
  #   strength = 1,
  #   schools = schools_idx,
  #   schools_capacity = schools_capacity
  )

simulate_plans <- function(map, draws, nsims, nruns) {
  for (i in 1:length(draws)) {
    starter_name <- paste0("elem", i)
    plans <- redist_smc(
      map,
      nsims = nsims, runs = nruns,
      ncores = 60,
      counties = starter_name,
      constraints = constr,
      #pop_temper = 0.01,
      #sampling_space = "linking_edge",
      #ms_params = list(frequency = 1L, mh_accept_per_smc = 10),
      #split_params = list(splitting_schedule = "any_valid_sizes"),
      verbose = T
    )
    
    plans <- match_numbers(plans, "middle25")
    
    # keep only plans where each school is in a distinct district
    plans <- drop_duplicate_schools(plans, schools_idx)
    
    path <- paste0("data-raw/middle25/plans/plans_ms_", i, ".rds")
    write_rds(plans, here(path), compress = "gz")
  }
}

simulate_plans(map, draws5, nsims, nruns)

# combine 3 sets of plans
plans1 <- read_rds(here("data-raw/middle25/plans/plans_ms_1.rds"))
plans2 <- read_rds(here("data-raw/middle25/plans/plans_ms_2.rds"))
plans3 <- read_rds(here("data-raw/middle25/plans/plans_ms_3.rds"))
N <- 2500
plans <- rbind(plans1,
               plans2 %>% subset_sampled() %>% mutate(draw = factor(as.integer(draw) + 1 * N)),
               plans3 %>% subset_sampled() %>% mutate(draw = factor(as.integer(draw) + 2 * N)))

write_rds(plans, here("data-raw/middle25/plans/plans_ms.rds"))
