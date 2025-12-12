# get elementary school geometries
ffx_schools <- read_sf("data/School_Facilities/School_Facilities.shp")
ffx_es <- ffx_schools %>%
  filter(SCHOOL_TYP == "ES")

# get school capacity data
capacity <- read_csv("data/school_capacities.csv") %>%
  filter(type == "ES") %>%
  mutate(utilization = membership / capacity,
         prop_capacity = capacity / sum(capacity))

# get rid of schools that don't have attendance areas
ffx_es <- ffx_es %>%
  filter(OBJECTID %in% capacity$object_id_school)

# make redist_map
# current ES capacity min/max is 384/1066 so pop_tol=(1066-384)/1066
map <- redist_map(ffx_shp, pop_tol = 1.1,
                  existing_plan = elem25, adj = ffx_shp$adj)
attr(map, "analysis_name") <- "ES_25"
attr(map, "shp") <- ffx_shp

# get school row indices of map and capacities
schools_info <- get_schools_info(ffx_es, map, ffx_shp, capacity, "elem")
schools_idx <- schools_info$map_idx
schools_capacity <- schools_info$capacity

if (!file.exists(here("data-raw/elem25/commute_times_es.rds"))) {
  # calculate commute times
  commute_times <- get_commute_matrix(ffx_shp, schools_idx, profile = "car", 
                                      server = "http://127.0.0.1:5000", 
                                      src_chunk = 100, dst_chunk = 140)
  
  write_rds(commute_times, here("data-raw/elem25/commute_times_es.rds"), compress = "gz")
} else {
  commute_times <- read_rds(here("data-raw/elem25/commute_times_es.rds"))
}