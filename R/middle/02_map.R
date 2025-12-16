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

# sample 5 elementary plans as starter plans
elem_plans <- read_rds(here("data-raw/elem25/plans/plans_es.rds"))
plans5 <- elem_plans %>%
  filter(draw != "elem25") %>%
  filter(draw %in% sample(unique(draw), 5))
draws5 <- as.numeric(match(levels(plans5$draw), elem_plans$draw %>% unique()))
ffx_shp <- add_starter_plans(ffx_shp, elem_plans, draws5, "elem")

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
  commute_times <- get_commute_matrix(ffx_shp, schools_idx, profile = "car", 
                                      server = "http://127.0.0.1:5000", 
                                      src_chunk = 100, dst_chunk = 140)
  
  write_rds(commute_times, here("data-raw/middle25/commute_times_ms.rds"), compress = "gz")
} else {
  commute_times <- read_rds(here("data-raw/middle25/commute_times_ms.rds"))
}