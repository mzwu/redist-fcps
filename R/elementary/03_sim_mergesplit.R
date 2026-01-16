# install redist_original/commit-mergesplit before running this

nsims <- 2e5
nstarter <- 3

init_plans <- read_rds(here("data-raw/elem/plans/plans_es_gsmc_com1_inc4_cap10_pop0.66.rds"))
set.seed(2025)
plans5 <- init_plans %>%
  filter(!(draw %in% c("elem_scenario2", "elem_scenario3", "elem_scenario4", "elem_scenario5"))) %>%
  filter(draw %in% sample(unique(draw), nstarter))
draws5 <- as.numeric(match(levels(plans5$draw), init_plans$draw %>% unique()))
map <- add_starter_plans(map, init_plans, draws5, "init")

constr <- redist_constr(map) %>%
  add_constr_commute(
    strength = 1,
    current = map$elem_current,
    commute_times = commute_times
  ) %>%
  add_constr_incumbency(
    strength = 9,
    incumbents = schools_idx
  ) %>%
  add_constr_capacity(
    strength = 35,
    schools = schools_idx,
    schools_capacity = schools_capacity
  )


simulate_plans <- function(map, draws, nsims, nruns) {
  for (i in 1:length(draws)) {
    starter_name <- paste0("init_starter", i)
    plans <- redist_mergesplit(
      map,
      nsims = nsims,
      init_plan = map[[starter_name]],
      init_name = FALSE,
      constraints = constr,
      #pop_temper = 0.01,
      verbose = T
    )
    
    path <- paste0("data-raw/elem/plans/plans_es_", i, ".rds")
    write_rds(plans, here(path), compress = "gz")
  }
}

simulate_plans(map, draws5, nsims, nruns)

# combine 3 sets of plans
plans1 <- read_rds(here("data-raw/elem/plans/plans_es_1.rds"))
plans2 <- read_rds(here("data-raw/elem/plans/plans_es_2.rds"))
plans3 <- read_rds(here("data-raw/elem/plans/plans_es_3.rds"))
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
  add_reference(map$elem_scenario5, "elem_scenario5") %>%
  add_reference(map$elem_scenario4, "elem_scenario4") %>%
  add_reference(map$elem_scenario3, "elem_scenario3") %>%
  add_reference(map$elem_scenario2, "elem_scenario2")

write_rds(plans, here("data-raw/elem/plans/plans_es_mcmc_com1_inc9_cap35_pop0.66.rds"), compress = "gz")
