# get middle school point geometries
ffx_ms <- read_sf("data/School_Facilities/School_Facilities.shp") %>%
  filter(SCHOOL_TYP == "MS")

# get school capacity data
capacity <- read_csv("data/school_capacities.csv") %>%
  filter(type == "MS") %>%
  mutate(utilization = membership / capacity,
         prop_capacity = capacity / sum(capacity)) %>%
  arrange(object_id_school) %>%
  mutate(middle25 = vctrs::vec_group_id(object_id_school))

# get rid of schools that don't have attendance areas
ffx_ms <- ffx_ms %>%
  filter(OBJECTID %in% capacity$object_id_school)

# sample 5 elementary plans as starter plans
nstarter <- 1
elem_plans <- read_rds(here("data-raw/elem/plans/plans_es.rds"))
set.seed(2025)
plans5 <- elem_plans %>%
  filter(!(draw %in% c("elem_scenario2", "elem_scenario3", "elem_scenario4", "elem_scenario5"))) %>%
  filter(draw %in% sample(unique(draw), nstarter))
draws5 <- as.numeric(match(levels(plans5$draw), elem_plans$draw %>% unique()))
ffx_shp <- add_starter_plans(ffx_shp, elem_plans, draws5, "elem")

# make redist_map
map <- redist_map(ffx_shp, pop_tol = 0.5,
                  existing_plan = middle_current, adj = ffx_shp$adj)
attr(map, "analysis_name") <- "MS_25_S4"
attr(map, "shp") <- ffx_shp

# get school row indices of map in ascending ID order
schools_idx <- get_schools_idx(ffx_ms, map)

# get school capacities in ascending ID order
schools_capacity <- capacity %>%
  select(capacity, middle25) %>%
  arrange(middle25) %>%
  pull(capacity)

if (!file.exists(here("data-raw/middle/commute_times_ms.rds"))) {
  # calculate commute times
  commute_times <- get_commute_matrix(ffx_shp, schools_idx, profile = "car", 
                                      server = "http://127.0.0.1:5000", 
                                      src_chunk = 100, dst_chunk = 140)
  
  write_rds(commute_times, here("data-raw/middle/commute_times_ms.rds"), compress = "gz")
} else {
  commute_times <- read_rds(here("data-raw/middle/commute_times_ms.rds"))
}