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
total_pop <- sum(map$pop)
ndists <- map$elem25 %>% unique() %>% length()
attr(map, "pop_bounds") <- c(1, (total_pop + 1) / 2, total_pop - ndists + 1)

# get school row indices of map and capacities
schools_info <- get_schools_info(ffx_es, map, ffx_shp, capacity, "elem")

# fix Fort Belvoir Primary school to be in the block adjacent because it's currently in the same block as Fort Belvoir Upper
new_row <- which(map$geoid20 == "510594219002006")
schools_info$map_idx[schools_info$name == "Fort Belvoir Primary"] <- new_row

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

# create initial plan that has 1 school per district
init_plan <- rep(0, nrow(map))
# assign each school to a distinct district that corresponds to their actual assignment
init_plan[schools_info$map_idx] <- schools_info$elem25
# BFS
bfs_assign <- function(map, start, assignments) {
  visited <- c(start)
  queue <- c(start)
  plan <- c(assignments)
  
  while (length(queue) > 0) {
    node <- queue[1]
    queue <- queue[-1]
    
    visited <- c(visited, node)
      
    # get all adjacent neighbors
    neighbors <- as.integer(unlist(map$adj[[node]])) + 1
      
    # assign unassigned neighbors to the same district as the node we discovered them from
    unassigned_nbrs <- setdiff(neighbors, visited)
    plan[unassigned_nbrs] <- plan[node]
      
    # add to queue
    queue <- c(queue, unassigned_nbrs)
  }
  
  plan
}

init_plan <- bfs_assign(map, schools_info$map_idx, init_plan)
map$init_plan <- init_plan

map[c(3065, 4776, 2017),]$init_plan <- 116
map[c(364),]$init_plan <- 91
map[c(101),]$init_plan <- 84
map[c(7105, 5472),]$init_plan <- 108
map[c(791, 5441, 3446),]$init_plan <- 15
map[c(8887, 3884, 7082, 3205, 3561, 4933, 3808, 2941, 8245, 6168, 7421, 4180, 4020),]$init_plan <- 44
