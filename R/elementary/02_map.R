# get elementary school geometries
ffx_schools <- read_sf("data/School_Facilities/School_Facilities.shp")
ffx_es <- ffx_schools %>%
  filter(SCHOOL_TYP == "ES")

# get school capacity data
capacity <- read_csv("data/school_capacities.csv") %>%
  filter(type == "ES") %>%
  mutate(utilization = membership / capacity,
         prop_capacity = capacity / sum(capacity))

# combine attendance area geometry + capacity data
ffx_elem <- read_sf("data/Elementary_School_Attendance_Areas/Elementary_School_Attendance_Areas.shp") %>%
  merge(capacity, by.x = "OBJECTID", by.y = "object_id_area")

# get rid of schools that don't have attendance areas
ffx_es <- ffx_es %>%
  filter(OBJECTID %in% ffx_elem$object_id_school)

# make redist_map
map <- redist_map(ffx_shp, pop_tol = 0.1,
                  existing_plan = elem25, adj = ffx_shp$adj)
attr(map, "analysis_name") <- "ES_25"
attr(map, "shp") <- ffx_shp
attr(map, "districting_scheme") <- "single"

# get school row indices of map
schools_idx <- get_schools_idx(ffx_es, map)

if (!file.exists(here("data-raw/elem25/commute_times_es.rds"))) {
  # calculate commute times
  commute_times <- get_commute_matrix(ffx_shp, schools_idx - 1L, profile = "car", 
                                      server = "http://127.0.0.1:5000", 
                                      src_chunk = 100, dst_chunk = 140)
  
  write_rds(commute_times, here("data-raw/elem25/commute_times_es.rds"), compress = "gz")
} else {
  commute_times <- read_rds(here("data-raw/elem25/commute_times_es.rds"))
}