# unique ID for each row, will use later to reconnect pieces
map$row_id <- 1:nrow(map)

nsims <- 1e4
sa_region <- 0.99
sa <- 0.95

# 140 districts is too large of a problem
# solution: use SMC by cluster/region
# note: if switch to gsmc, use only_districts=TRUE in constraints

########################################################################
# Region 1: Herndon, Langley, Madison, Oakton, and South Lakes Pyramids
region1_map <- map %>% filter(region == 1)

# setup for cluster constraint
map <- map %>%
  mutate(cluster_edge = ifelse(row_id %in% region1_map$row_id, 1, 0))
z <- geomander::seam_geom(map$adj, map, admin = "cluster_edge", seam = c(0, 1))
z <- z[z$cluster_edge == 1, ]
border_idxs <- which(region1_map$row_id %in% z$row_id)

# Region 1 school indices
school_blocks <- st_contains(region1_map, schools_info)
region1_map$school <- lengths(school_blocks) > 0
region1_schools <- which(region1_map$school == TRUE)

# Region 1 commute times
region1_elem25 <- unique(unlist(school_blocks[region1_map$school]))
region1_capacity <- capacity[region1_elem25, ]

constr <- redist_constr(region1_map) %>%
  add_constr_phase_commute(
    strength = 1,
    current = region1_map$elem25,
    commute_times = commute_times
  ) %>%
  add_constr_incumbency(
    strength = 100,
    incumbents = region1_schools
    ) %>%
  add_constr_capacity(
    strength = 2,
    schools = region1_schools,
    schools_capacity = region1_capacity$capacity
  ) %>%
  add_constr_custom(strength = 10, function(plan, distr) {
    ifelse(any(plan[border_idxs] == 0), 0, 1)
  })

n_steps <- region1_elem25 %>% length() - 1

set.seed(2025)
region1_plans <- redist_smc(
  region1_map,
  nsims = nsims, 
  #runs = 2L,
  ncores = 60,
  n_steps = n_steps,
  seq_alpha = sa_region,
  constraints = constr, 
  pop_temper = 0.01, 
  verbose = TRUE
)

region1_plans <- drop_duplicate_schools_regions(region1_plans, region1_schools)

write_rds(region1_plans, here("data-raw/elem25/plans/region1_plans.rds"), compress = "gz")

########################################################################
# Region 2: Falls Church, Justice, and McLean Pyramids
region2_map <- map %>% filter(region == 2)

# setup for cluster constraint
map <- map %>%
  mutate(cluster_edge = ifelse(row_id %in% region2_map$row_id, 1, 0))
z <- geomander::seam_geom(map$adj, map, admin = "cluster_edge", seam = c(0, 1))
z <- z[z$cluster_edge == 1, ]
border_idxs <- which(region2_map$row_id %in% z$row_id)

# Region 2 school indices
school_blocks <- st_contains(region2_map, schools_info)
region2_map$school <- lengths(school_blocks) > 0
region2_schools <- which(region2_map$school == TRUE)

# Region 2 commute times
region2_elem25 <- unique(unlist(school_blocks[region2_map$school]))
region2_capacity <- capacity[region2_elem25, ]

constr <- redist_constr(region2_map) %>%
  add_constr_phase_commute(
    strength = 1,
    current = region2_map$elem25,
    commute_times = commute_times
  ) %>%
  add_constr_incumbency(
    strength = 150,
    incumbents = region2_schools
  ) %>%
  add_constr_capacity(
    strength = 2,
    schools = region2_schools,
    schools_capacity = region2_capacity$capacity
  ) %>%
  add_constr_custom(strength = 10, function(plan, distr) {
    ifelse(any(plan[border_idxs] == 0), 0, 1)
  })

n_steps <- region2_elem25 %>% length() - 1

set.seed(2025)
region2_plans <- redist_smc(
  region2_map,
  nsims = nsims, 
  #runs = 2L,
  ncores = 60,
  n_steps = n_steps,
  seq_alpha = sa_region,
  constraints = constr, 
  pop_temper = 0.01, 
  verbose = TRUE
)

region2_plans <- drop_duplicate_schools_regions(region2_plans, region2_schools)

write_rds(region2_plans, here("data-raw/elem25/plans/region2_plans.rds"), compress = "gz")

########################################################################
# Region 3: Edison, Mount Vernon, and West Potomac Pyramids
region3_map <- map %>% filter(region == 3)

# setup for cluster constraint
map <- map %>%
  mutate(cluster_edge = ifelse(row_id %in% region3_map$row_id, 1, 0))
z <- geomander::seam_geom(map$adj, map, admin = "cluster_edge", seam = c(0, 1))
z <- z[z$cluster_edge == 1, ]
border_idxs <- which(region3_map$row_id %in% z$row_id)

# Region 3 school indices
school_blocks <- st_contains(region3_map, schools_info)
region3_map$school <- lengths(school_blocks) > 0
region3_schools <- which(region3_map$school == TRUE)

# Region 3 commute times
region3_elem25 <- unique(unlist(school_blocks[region3_map$school]))
region3_capacity <- capacity[region3_elem25, ]

constr <- redist_constr(region3_map) %>%
  # add_constr_phase_commute(
  #   strength = 1,
  #   current = region3_map$elem25,
  #   commute_times = commute_times
  # ) %>%
  add_constr_incumbency(
    strength = 1000,
    incumbents = region3_schools
  ) %>%
  add_constr_capacity(
    strength = 2,
    schools = region3_schools,
    schools_capacity = region3_capacity$capacity
  ) %>%
  add_constr_custom(strength = 10, function(plan, distr) {
    ifelse(any(plan[border_idxs] == 0), 0, 1)
  })

n_steps <- region3_elem25 %>% length() - 1

set.seed(2025)
region3_plans <- redist_smc(
  region3_map,
  nsims = nsims, 
  #runs = 2L,
  ncores = 60,
  n_steps = n_steps,
  seq_alpha = sa_region,
  constraints = constr, 
  pop_temper = 0.01, 
  verbose = TRUE
)

region3_plans <- drop_duplicate_schools_regions(region3_plans, region3_schools)

write_rds(region3_plans, here("data-raw/elem25/plans/region3_plans.rds"), compress = "gz")

########################################################################
# Region 4: Centreville, Lake Braddock, Robinson, South County, and West Springfield Pyramids
region4_map <- map %>% filter(region == 4)

# setup for cluster constraint
map <- map %>%
  mutate(cluster_edge = ifelse(row_id %in% region4_map$row_id, 1, 0))
z <- geomander::seam_geom(map$adj, map, admin = "cluster_edge", seam = c(0, 1))
z <- z[z$cluster_edge == 1, ]
border_idxs <- which(region4_map$row_id %in% z$row_id)

# Region 4 school indices
school_blocks <- st_contains(region4_map, schools_info)
region4_map$school <- lengths(school_blocks) > 0
region4_schools <- which(region4_map$school == TRUE)

# Region 4 commute times
region4_elem25 <- unique(unlist(school_blocks[region4_map$school]))
region4_capacity <- capacity[region4_elem25, ]

constr <- redist_constr(region4_map) %>%
  add_constr_phase_commute(
    strength = 1,
    current = region4_map$elem25,
    commute_times = commute_times
  ) %>%
  add_constr_incumbency(
    strength = 100,
    incumbents = region4_schools
  ) %>%
  add_constr_capacity(
    strength = 2,
    schools = region4_schools,
    schools_capacity = region4_capacity$capacity
  ) %>%
  add_constr_custom(strength = 10, function(plan, distr) {
    ifelse(any(plan[border_idxs] == 0), 0, 1)
  })

n_steps <- region4_elem25 %>% length() - 1

set.seed(2025)
region4_plans <- redist_smc(
  region4_map,
  nsims = nsims, 
  #runs = 2L,
  ncores = 60,
  n_steps = n_steps,
  seq_alpha = sa_region,
  constraints = constr, 
  pop_temper = 0.01, 
  verbose = TRUE
)

region4_plans <- drop_duplicate_schools_regions(region4_plans, region4_schools)

write_rds(region4_plans, here("data-raw/elem25/plans/region4_plans.rds"), compress = "gz")

########################################################################
# Region 5: Chantilly, Fairfax, Marshall, Westfield, and Woodson Pyramids
region5_map <- map %>% filter(region == 5)

# setup for cluster constraint
map <- map %>%
  mutate(cluster_edge = ifelse(row_id %in% region5_map$row_id, 1, 0))
z <- geomander::seam_geom(map$adj, map, admin = "cluster_edge", seam = c(0, 1))
z <- z[z$cluster_edge == 1, ]
border_idxs <- which(region5_map$row_id %in% z$row_id)

# Region 1 school indices
school_blocks <- st_contains(region5_map, schools_info)
region5_map$school <- lengths(school_blocks) > 0
region5_schools <- which(region5_map$school == TRUE)

# Region 1 commute times
region5_elem25 <- unique(unlist(school_blocks[region5_map$school]))
region5_capacity <- capacity[region5_elem25, ]

constr <- redist_constr(region5_map) %>%
  add_constr_phase_commute(
    strength = 1,
    current = region5_map$elem25,
    commute_times = commute_times
  ) %>%
  add_constr_incumbency(
    strength = 100,
    incumbents = region5_schools
  ) %>%
  add_constr_capacity(
    strength = 2,
    schools = region5_schools,
    schools_capacity = region5_capacity$capacity
  ) %>%
  add_constr_custom(strength = 10, function(plan, distr) {
    ifelse(any(plan[border_idxs] == 0), 0, 1)
  })

n_steps <- region5_elem25 %>% length() - 1

set.seed(2025)
region5_plans <- redist_smc(
  region5_map,
  nsims = nsims, 
  #runs = 2L,
  ncores = 60,
  n_steps = n_steps,
  seq_alpha = sa_region,
  constraints = constr, 
  pop_temper = 0.01, 
  verbose = TRUE
)

region5_plans <- drop_duplicate_schools_regions(region5_plans, region5_schools)

write_rds(region5_plans, here("data-raw/elem25/plans/region5_plans.rds"), compress = "gz")

########################################################################
# Region 6: Annandale, Hayfield, and Lewis Pyramids
region6_map <- map %>% filter(region == 6)

# setup for cluster constraint
map <- map %>%
  mutate(cluster_edge = ifelse(row_id %in% region6_map$row_id, 1, 0))
z <- geomander::seam_geom(map$adj, map, admin = "cluster_edge", seam = c(0, 1))
z <- z[z$cluster_edge == 1, ]
border_idxs <- which(region6_map$row_id %in% z$row_id)

# Region 1 school indices
school_blocks <- st_contains(region6_map, schools_info)
region6_map$school <- lengths(school_blocks) > 0
region6_schools <- which(region6_map$school == TRUE)

# Region 1 commute times
region6_elem25 <- unique(unlist(school_blocks[region6_map$school]))
region6_capacity <- capacity[region6_elem25, ]

constr <- redist_constr(region6_map) %>%
  add_constr_phase_commute(
    strength = 1,
    current = region6_map$elem25,
    commute_times = commute_times
  ) %>%
  add_constr_incumbency(
    strength = 100,
    incumbents = region6_schools
  ) %>%
  add_constr_capacity(
    strength = 2,
    schools = region6_schools,
    schools_capacity = region6_capacity$capacity
  ) %>%
  add_constr_custom(strength = 10, function(plan, distr) {
    ifelse(any(plan[border_idxs] == 0), 0, 1)
  })

n_steps <- region6_elem25 %>% length() - 1

set.seed(2025)
region6_plans <- redist_smc(
  region6_map,
  nsims = nsims, 
  #runs = 2L,
  ncores = 60,
  n_steps = n_steps,
  seq_alpha = sa_region,
  constraints = constr, 
  pop_temper = 0.01, 
  verbose = TRUE
)

region6_plans <- drop_duplicate_schools_regions(region6_plans, region6_schools)

write_rds(region6_plans, here("data-raw/elem25/plans/region6_plans.rds"), compress = "gz")

########################################################################
# Combine Clusters
region1_plans$dist_keep <- ifelse(region1_plans$district == 0, FALSE, TRUE)
region2_plans$dist_keep <- ifelse(region2_plans$district == 0, FALSE, TRUE)
region3_plans$dist_keep <- ifelse(region3_plans$district == 0, FALSE, TRUE)
region4_plans$dist_keep <- ifelse(region4_plans$district == 0, FALSE, TRUE)
region5_plans$dist_keep <- ifelse(region5_plans$district == 0, FALSE, TRUE)
region6_plans$dist_keep <- ifelse(region6_plans$district == 0, FALSE, TRUE)

elem_plan_list <- list(list(map = region1_map, plans = region1_plans),
                       list(map = region2_map, plans = region2_plans),
                       list(map = region3_map, plans = region3_plans),
                       list(map = region4_map, plans = region4_plans),
                       list(map = region5_map, plans = region5_plans),
                       list(map = region6_map, plans = region6_plans))

prep_mat <- prep_particles(map = map, map_plan_list = elem_plan_list,
                           uid = row_id, dist_keep = dist_keep, nsims = nsims*2)

## Check contiguity
if (FALSE) {
  test_vec <- sapply(seq_len(ncol(prep_mat)), function(i) {
    cat(i, "\n")
    z <- map %>%
      mutate(ex_dist = ifelse(prep_mat[, i] == 0, 1, 0))
    
    z <- geomander::check_contiguity(adj = z$adj, group = z$ex_dist)
    
    length(unique(z$component[z$group == 1]))
  })
  
  table(test_vec)/nsims
}

constr <- redist_constr(map) %>%
  add_constr_phase_commute(
    strength = 1,
    current = map$elem25,
    schools = schools_idx,
    commute_times = commute_times
  ) %>%
  add_constr_incumbency(
    strength = 100,
    incumbents = schools_idx
  ) %>%
  add_constr_capacity(
    strength = 2,
    schools = schools_idx,
    schools_capacity = schools_capacity
  ) %>%
  add_constr_custom(strength = 10, function(plan, distr) {
    ifelse(any(plan[border_idxs] == 0), 0, 1)
  })

set.seed(2025)
plans <- redist_smc(
  map,
  nsims = nsims * 2,
  #runs = 2L,
  #counties = tractce20,
  constraints = constr,
  init_particles = prep_mat,
  #pop_temper = 0.01,
  seq_alpha = sa,
  verbose = TRUE
)

plans <- plans %>%
  add_reference(map$elem25, "elem25")

plans <- match_numbers(plans, "elem25")

# TODO: thin samples

# TODO: keep only plans where each school is in a distinct district
#plans <- drop_duplicate_schools(plans, schools_idx)

write_rds(plans, here("data-raw/elem25/plans/plans_es.rds"), compress = "gz")
