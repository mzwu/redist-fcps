# install redist_gsmc before running this

nsims <- 1000
nruns <- 1L
set.seed(2025)

init_plans <- read_rds(here("data-raw/middle/plans/plans_ms_com1_inc11_split0_cap1_pop0.2.rds"))
set.seed(2025)
plans_init <- init_plans %>%
  filter(!(str_detect(draw, "scenario") | str_detect(draw, "current"))) %>%
  filter(draw %in% sample(unique(draw), nstarter))
draws_init <- as.numeric(match(levels(plans_init$draw), init_plans$draw %>% unique()))
map <- add_starter_plans(map, init_plans, draws_init, "init")

scorer <- function(map, schools_idx, commute_times, schools_capacity) {
  fn <- function(plans) {
    inc <- colMeans(matrix(school_outside_zone(plans, schools_idx), nrow = 26))
    com <- colMeans(matrix(max_commute(plans, map, commute_times), nrow = 26))
    cap <- colMeans(matrix(capacity_util(plans, schools_capacity), nrow = 26))
    
    10 * inc + com + 10 * cap
  }
  class(fn) <- c("redist_scorer", "function")
  fn
}

simulate_plans <- function(map, draws, nsims, nruns) {
  for (i in 1:length(draws)) {
    starter_name <- paste0("init_starter", i)
    
    plans <- redist_shortburst(
      map,
      score_fn = scorer(map, schools_idx, commute_times, schools_capacity),
      max_bursts = 15000,
      maximize = FALSE,
      return_all = FALSE,
      init_plan = map[[starter_name]],
      #pop_temper = 0.01,
      verbose = T
    ) %>%
      subset_sampled()
    
    # calculate split feeders
    plans <- add_split_feeder_stats(plans, map, starter_name, "middle")
    
    path <- paste0("data-raw/middle/plans/plans_ms_sb_", i, ".rds")
    write_rds(plans, here(path), compress = "gz")
  }
}

simulate_plans(map, draws_init, nsims, nruns)

# combine 3 sets of plans
plans1 <- read_rds(here("data-raw/middle/plans/plans_ms_sb_1.rds"))
plans2 <- read_rds(here("data-raw/middle/plans/plans_ms_sb_2.rds"))
plans3 <- read_rds(here("data-raw/middle/plans/plans_ms_sb_3.rds"))
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

plans <- plans %>%
  add_reference(map$middle_current, "middle_current") %>%
  add_reference(map$middle_scenario5, "middle_scenario5") %>%
  add_reference(map$middle_scenario4, "middle_scenario4") %>%
  add_reference(map$middle_scenario3, "middle_scenario3") %>%
  add_reference(map$middle_scenario2, "middle_scenario2") %>%
  add_split_feeder_stats(map, "elem_scenario5", "middle")

write_rds(plans, here(paste0("data-raw/middle/plans/plans_ms_sb_pop0.2.rds")), compress = "gz")
