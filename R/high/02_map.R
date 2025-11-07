# get high school geometries
ffx_schools <- read_sf("data/School_Facilities/School_Facilities.shp")
ffx_hs <- ffx_schools %>%
  filter(SCHOOL_TYP == "HS")

# get school capacity data
capacity <- read_csv("data/school_capacities.csv") %>%
  filter(type == "HS") %>%
  mutate(utilization = membership / capacity,
         prop_capacity = capacity / sum(capacity))

# combine attendance area geometry + capacity data
ffx_high <- read_sf("data/High_School_Attendance_Areas/High_School_Attendance_Areas.shp") %>%
  merge(capacity, by.x = "OBJECTID", by.y = "object_id_area")

# get rid of schools that don't have attendance areas
ffx_hs <- ffx_hs %>%
  filter(OBJECTID %in% ffx_high$object_id_school)

# make redist_map
map <- redist_map(ffx_shp, pop_tol = 0.05,
                  existing_plan = high25, adj = ffx_shp$adj)
attr(map, "analysis_name") <- "HS_25"
attr(map, "shp") <- ffx_shp

# get school row indices of map
schools_idx <- get_schools_idx(ffx_hs, map)

# calculate commute times
commute_times <- get_commute_matrix(ffx_shp, schools_idx - 1L, profile = "car", 
                                    server = "http://127.0.0.1:5000", 
                                    src_chunk = 100, dst_chunk = 140)
