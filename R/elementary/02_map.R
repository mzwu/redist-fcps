# get elementary school point geometries
ffx_es <- read_sf("data/School_Facilities/School_Facilities.shp") %>%
  filter(SCHOOL_TYP == "ES")

# get school capacity data
capacity <- read_csv("data/school_capacities.csv") %>%
  filter(type == "ES") %>%
  mutate(utilization = membership / capacity,
         prop_capacity = capacity / sum(capacity)) %>%
  arrange(object_id_school) %>%
  mutate(elem25 = vctrs::vec_group_id(object_id_school))

# get rid of schools that don't have attendance areas
ffx_es <- ffx_es %>%
  filter(OBJECTID %in% capacity$object_id_school)

# make redist_map
# current ES capacity min/max is 384/1066 so 
# average distance is (1066-384)/1066=0.64
# add some margin of error to pop_tol
map <- redist_map(ffx_shp, pop_tol = 0.9,
                  existing_plan = elem_scenario4, adj = ffx_shp$adj)
attr(map, "analysis_name") <- "ES_25_S4"
attr(map, "shp") <- ffx_shp

# get school row indices of map in ascending ID order
schools_idx <- get_schools_idx(ffx_es, map)

# get school capacities in ascending ID order
schools_capacity <- capacity %>%
  select(capacity, elem25) %>%
  arrange(elem25) %>%
  pull(capacity)

# get/calculate commute times from each block to each school block
if (!file.exists(here("data-raw/elem/commute_times_es.rds"))) {
  commute_times <- get_commute_matrix(ffx_shp, schools_idx, profile = "car", 
                                      server = "http://127.0.0.1:5000", 
                                      src_chunk = 100, dst_chunk = 140)
  
  write_rds(commute_times, here("data-raw/elem/commute_times_es.rds"), compress = "gz")
} else {
  commute_times <- read_rds(here("data-raw/elem/commute_times_es.rds"))
}