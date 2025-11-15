shp_path <- "data/ffx_blocks.rds"

if (!file.exists(here(shp_path))) {
  cli_process_start("Preparing {.strong Fairfax County} shapefile")
  
  # read in Fairfax County block data
  # Fairfax County, Fairfax City, Falls Church
  ffx_blocks <- read_sf("data/tl_2024_51_tabblock20/tl_2024_51_tabblock20.shp") %>%
    filter(COUNTYFP20 %in% c("059", "600", "610"))
  
  # read in Fairfax County block group data
  ffx_bg <- read_sf("data/tl_2024_51_bg/tl_2024_51_bg.shp") %>%
    filter(COUNTYFP %in% c("059", "600", "610"))
  ffx_blocks <- ffx_blocks %>%
    mutate(blockgrp = ffx_bg$BLKGRPCE[
      geo_match(ffx_blocks, ffx_bg, method = "area")])
  
  # read in Fairfax County tract data
  ffx_tract <- read_sf("data/tl_2024_51_tract/tl_2024_51_tract.shp") %>%
    filter(COUNTYFP %in% c("059", "600", "610"))
  ffx_blocks <- ffx_blocks %>%
    mutate(tract = ffx_tract$TRACTCE[
      geo_match(ffx_blocks, ffx_tract, method = "area")]) #%>%
    #group_by(tract) %>%
    #summarize(POP20 = sum(POP20),
              #geometry = st_union(geometry))
  
  # read in Elementary School Attendance Area data
  ffx_elementary <- read_sf("data/Elementary_School_Attendance_Areas/Elementary_School_Attendance_Areas.shp")
  ffx_blocks <- ffx_blocks %>%
    mutate(elem25 = ffx_elementary$OBJECTID[
      geo_match(ffx_blocks, ffx_elementary, method = "area")])
  
  # read in Middle School Attendance Area data
  ffx_middle <- read_sf("data/Middle_School_Attendance_Areas/Middle_School_Attendance_Areas.shp")
  ffx_blocks <- ffx_blocks %>%
    mutate(middle25 = ffx_middle$OBJECTID[
      geo_match(ffx_blocks, ffx_middle, method = "area")])
  
  # read in High School Attendance Area data
  ffx_high <- read_sf("data/High_School_Attendance_Areas/High_School_Attendance_Areas.shp")
  ffx_blocks <- ffx_blocks %>%
    mutate(high25 = ffx_high$OBJECTID[
      geo_match(ffx_blocks, ffx_high, method = "area")])
  
  # clean up columns
  ffx_blocks <- ffx_blocks %>%
    janitor::clean_names()
  
  # simplifies geometry for faster processing, plotting, and smaller shapefiles
  if (requireNamespace("rmapshaper", quietly = TRUE)) {
    ffx_blocks <- rmapshaper::ms_simplify(ffx_blocks, keep = 0.05,
                                        keep_shapes = TRUE) %>%
      suppressWarnings()
  }
  ffx_blocks <- ffx_blocks %>% 
    rename(pop = pop20)
  
  # create adjacency graph
  ffx_blocks <- ffx_blocks %>%
    st_make_valid(model = "semi-open")
  ffx_blocks$adj <- redist.adjacency(ffx_blocks)
  
  # save shapefile
  ffx_shp <- ffx_blocks
  write_rds(ffx_shp, here(shp_path), compress = "gz")
  cli_process_done()
} else {
  ffx_shp <- read_rds(here(shp_path))
  cli_alert_success("Loaded {.strong Fairfax County} shapefile")
}