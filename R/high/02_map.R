# get high school geometries
ffx_schools <- read_sf("data/School_Facilities/School_Facilities.shp")
ffx_hs <- ffx_schools %>%
  filter(SCHOOL_TYP == "HS")

# get school capacity data
capacity <- read_csv("data/school_capacities.csv") %>%
  filter(type == "HS") %>%
  mutate(utilization = membership / capacity,
         prop_capacity = capacity / sum(capacity))

# get rid of schools that don't have attendance areas
ffx_hs <- ffx_hs %>%
  filter(OBJECTID %in% capacity$object_id_school)

# make redist_map
map <- redist_map(ffx_shp, pop_tol = 0.9,
                  existing_plan = high25, adj = ffx_shp$adj)
attr(map, "analysis_name") <- "HS_25"
attr(map, "shp") <- ffx_shp

# get school row indices of map and capacities
schools_info <- get_schools_info(ffx_hs, map, ffx_shp, capacity, "high")
schools_idx <- schools_info$map_idx
schools_capacity <- schools_info$capacity

if (!file.exists(here("data-raw/high25/commute_times_hs.rds"))) {
  # calculate commute times
  commute_times <- get_commute_matrix(ffx_shp, schools_idx, profile = "car", 
                                      server = "http://127.0.0.1:5000", 
                                      src_chunk = 100, dst_chunk = 140)
  
  write_rds(commute_times, here("data-raw/high25/commute_times_hs.rds"), compress = "gz")
} else {
  commute_times <- read_rds(here("data-raw/high25/commute_times_hs.rds"))
}

# TODO: take random sample of middle plans as starter plans