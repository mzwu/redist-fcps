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
map <- redist_map(ffx_shp, pop_tol = 0.64,
                  existing_plan = elem25, adj = ffx_shp$adj)
attr(map, "analysis_name") <- "ES_25"
attr(map, "shp") <- ffx_shp

# lenient pop tolerance because we'll impose capacity constraint later
total_pop <- sum(map$pop)
ndists <- map$elem25 %>% unique() %>% length()
attr(map, "pop_bounds") <- c(1, total_pop / ndists, total_pop - ndists + 1)

# get school row indices of map and capacities
schools_info <- get_schools_info(ffx_es, map, ffx_shp, capacity, "elem")

# fix Fort Belvoir Primary school to be in the block adjacent because it's currently in the same block as Fort Belvoir Upper
new_row <- which(map$geoid20 == "510594219002006")
schools_info$map_idx[schools_info$name == "Fort Belvoir Primary"] <- new_row

# temporarily reassign regions so that region cluster sims are contiguous
map$region[map$geoid20 %in% c("516003003001004")] <- 5
map$region[map$geoid20 %in% c("510594617004007", "510594617004005", 
                              "510594617004006", "510594617004004")] <- 1
map$region[map$geoid20 %in% c("510594605041008","510594605032027","510594606002007",
                              "510594605041010","510594606001005","510594605032016",
                              "510594606002012","510594605042011","510594605041006",
                              "510594606002011","510594605032018","510594605032028",
                              "510594605041007","510594605041009","510594605041014",
                              "510594606001001","510594605032013","510594605041013",
                              "510594605032006","510594605041005","510594605041011",
                              "510594616052000","510594605042005","510594616062006",
                              "510594606001000","510594616053002","510594606002010",
                              "510594605042010","510594605041004","510594606002008",
                              "510594605032029","510594606002006","510594616062015",
                              "510594606001003","510594616062011","510594606002009",
                              "510594605032030","510594605032020","510594616062007",
                              "510594605041000","510594616062009","510594605032019",
                              "510594605032011","510594606002005","510594605032021",
                              "510594605041012","510594616062010","510594606002004",
                              "510594606002003","510594605032017","510594606001006",
                              "510594605041001","510594605032031","510594605041002",
                              "510594605041003","510594605032014","510594605032012")] <- 2
map$region[map$geoid20 %in% c("510594302021004", "510594318022002", 
                              "510594302031004", "510594302021005")] <- 4
map$region[map$geoid20 %in% c("510594224032002", "510594327011005")] <- 6
map$region[map$elem25 %in% c(44, 81, 85, 101, 139)] <- 2
map$region[map$geoid20 %in% c("510594601001007", "510594601001006", 
                              "510594601001001", "510594601001004", 
                              "510594601001003", "510594819003012", 
                              "510594601001008", "510594601001009", 
                              "510594601002008", "510594601001010", 
                              "510594602001001", "510594601002007", 
                              "510594602001000", "510594601002012")] <- 1

# get data
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