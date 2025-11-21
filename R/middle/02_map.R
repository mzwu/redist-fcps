# get middle school geometries
ffx_schools <- read_sf("data/School_Facilities/School_Facilities.shp")
ffx_ms <- ffx_schools %>%
  filter(SCHOOL_TYP == "MS")

# get school capacity data
capacity <- read_csv("data/school_capacities.csv") %>%
  filter(type == "MS") %>%
  mutate(utilization = membership / capacity,
         prop_capacity = capacity / sum(capacity))

# get rid of schools that don't have attendance areas
ffx_ms <- ffx_ms %>%
  filter(OBJECTID %in% capacity$object_id_school)

# make redist_map
map <- redist_map(ffx_shp, pop_tol = 0.5,
                  existing_plan = middle25, adj = ffx_shp$adj)
attr(map, "analysis_name") <- "MS_25"
attr(map, "shp") <- ffx_shp

# get school row indices of map and capacities
schools_info <- get_schools_info(ffx_ms, map, ffx_shp, capacity, "middle")
schools_idx <- schools_info$map_idx
schools_capacity <- schools_info$capacity

if (!file.exists(here("data-raw/middle25/commute_times_ms.rds"))) {
  # calculate commute times
  commute_times <- get_commute_matrix(ffx_shp, schools_idx - 1L, profile = "car", 
                                      server = "http://127.0.0.1:5000", 
                                      src_chunk = 100, dst_chunk = 140)
  
  write_rds(commute_times, here("data-raw/middle25/commute_times_ms.rds"), compress = "gz")
} else {
  commute_times <- read_rds(here("data-raw/middle25/commute_times_ms.rds"))
}

# TODO: take random sample of elementary plans as starter plans
