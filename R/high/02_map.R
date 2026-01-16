# get high school geometries
ffx_hs <- read_sf("data/School_Facilities/School_Facilities.shp") %>%
  filter(SCHOOL_TYP == "HS")

# get school capacity data
capacity <- read_csv("data/school_capacities.csv") %>%
  filter(type == "HS") %>%
  mutate(utilization = membership / capacity,
         prop_capacity = capacity / sum(capacity)) %>%
  arrange(object_id_school) %>%
  mutate(high25 = vctrs::vec_group_id(object_id_school))

# get rid of schools that don't have attendance areas
ffx_hs <- ffx_hs %>%
  filter(OBJECTID %in% capacity$object_id_school)

# sample 5 middle plans as starter plans
nstarter <- 1
middle_plans <- read_rds(here("data-raw/middle/plans/plans_ms_1.rds"))
set.seed(2025)
plans5 <- middle_plans %>%
  filter(!(draw %in% c("middle_scenario2", "middle_scenario3", "middle_scenario4", "middle_scenario5"))) %>%
  filter(draw %in% sample(unique(draw), nstarter))
draws5 <- as.numeric(match(levels(plans5$draw), middle_plans$draw %>% unique()))
ffx_shp <- add_starter_plans(ffx_shp, middle_plans, draws5, "middle")

# make redist_map
map <- redist_map(ffx_shp, pop_tol = 0.9,
                  existing_plan = high_current, adj = ffx_shp$adj)
attr(map, "analysis_name") <- "HS_25_S4"
attr(map, "shp") <- ffx_shp

# get school row indices of map in ascending ID order
schools_idx <- get_schools_idx(ffx_hs, map)

# get school capacities in ascending ID order
schools_capacity <- capacity %>%
  select(capacity, high25) %>%
  arrange(high25) %>%
  pull(capacity)

if (!file.exists(here("data-raw/high/commute_times_hs.rds"))) {
  # calculate commute times
  commute_times <- get_commute_matrix(ffx_shp, schools_idx, profile = "car", 
                                      server = "http://127.0.0.1:5000", 
                                      src_chunk = 100, dst_chunk = 140)
  
  write_rds(commute_times, here("data-raw/high/commute_times_hs.rds"), compress = "gz")
} else {
  commute_times <- read_rds(here("data-raw/high/commute_times_hs.rds"))
}
