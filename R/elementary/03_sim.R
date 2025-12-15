# Unique ID for each row, will use later to reconnect pieces
map$row_id <- 1:nrow(map)

nsims <- 1e4
sa_region <- 0.99
sa <- 0.95

# 140 districts is too large of a problem
# so, use SMC by cluster/region

########################################################################
# Region 1: Herndon Pyramid
herndon_map <- map %>% filter(region == 1)

# setup for cluster constraint
map <- map %>%
  mutate(cluster_edge = ifelse(row_id %in% herndon_map$row_id, 1, 0))
z <- geomander::seam_geom(map$adj, map, admin = "cluster_edge", seam = c(0, 1))
z <- z[z$cluster_edge == 1, ]
border_idxs <- which(herndon_map$row_id %in% z$row_id)

# Herndon school indices
school_blocks <- st_contains(herndon_map, schools_info)
herndon_map$school <- lengths(school_blocks) > 0
herndon_schools <- which(herndon_map$school == TRUE)

# Herndon commute times
herndon_elem25 <- unique(unlist(school_blocks[herndon_map$school]))
herndon_commute <- commute_times[, herndon_elem25]
herndon_capacity <- capacity[herndon_elem25, ]

constr <- redist_constr(herndon_map) %>%
  # add_constr_phase_commute(
  #   strength = 1,
  #   current = map$elem25,
  #   schools = schools_idx,
  #   commute_times = commute_times
  # ) %>%
  add_constr_incumbency(
    strength = 20,
    incumbents = herndon_schools
    ) %>%
  add_constr_capacity(
    strength = 1,
    schools = herndon_schools,
    schools_capacity = herndon_capacity$capacity
  ) %>%
  add_constr_custom(strength = 10, function(plan, distr) {
    ifelse(any(plan[border_idxs] == 0), 0, 1)
  })

n_steps <- herndon_elem25 %>% length() - 1

# TODO: set seeds
herndon_plans <- redist_smc(
  herndon_map,
  nsims = nsims, 
  #runs = 2L,
  ncores = 60,
  n_steps = n_steps,
  seq_alpha = sa_region,
  constraints = constr, 
  #pop_temper = 0.01, 
  verbose = TRUE
)

plans <- plans %>%
  add_reference(map$elem25, "elem25")

plans <- match_numbers(plans, "elem25")

# keep only plans where each school is in a distinct district
#plans <- drop_duplicate_schools(plans, schools_idx)

write_rds(plans, here("data-raw/elem25/plans/plans_es_noconstr_incumbent99.rds"), compress = "gz")
