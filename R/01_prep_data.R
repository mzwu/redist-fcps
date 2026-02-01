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
      geo_match(ffx_blocks, ffx_tract, method = "area")])
  
  # map school names to IDs
  es_name_to_id <- read_csv("data/school_capacities.csv") %>%
    filter(type == "ES") %>%
    select(type, name, object_id_school) %>%
    arrange(object_id_school) %>%
    mutate(elem25 = vctrs::vec_group_id(object_id_school))
  ms_name_to_id <- read_csv("data/school_capacities.csv") %>%
    filter(type == "MS") %>%
    select(type, name, object_id_school) %>%
    arrange(object_id_school) %>%
    mutate(middle25 = vctrs::vec_group_id(object_id_school))
  hs_name_to_id <- read_csv("data/school_capacities.csv") %>%
    filter(type == "HS") %>%
    select(type, name, object_id_school) %>%
    arrange(object_id_school) %>%
    mutate(high25 = vctrs::vec_group_id(object_id_school))
  
  # add current boundaries
  es_current <- st_read("data/current_boundaries/current_es.geojson") %>%
    mutate(name = str_sub(NAME, 1, -4)) %>%
    left_join(es_name_to_id, by = "name")
  ms_current <- st_read("data/current_boundaries/current_ms.geojson") %>%
    mutate(name = str_sub(NAME, 1, -4)) %>%
    left_join(ms_name_to_id, by = "name")
  hs_current <- st_read("data/current_boundaries/current_hs.geojson") %>%
    mutate(name = str_sub(NAME, 1, -4)) %>%
    left_join(hs_name_to_id, by = "name")
  
  # we leave out scenario 1 because that addresses attendance islands and
  # SMC is guaranteed to eliminate all attendance islands
  
  # add Elementary School scenario boundaries
  es_scenario_2 <- st_read("data/scenario_boundaries/scenario_2_es.geojson") %>%
    mutate(name = str_sub(name, 1, -4)) %>%
    left_join(es_name_to_id, by = "name")
  es_scenario_3 <- st_read("data/scenario_boundaries/scenario_3_es.geojson") %>%
    mutate(name = str_sub(name, 1, -4)) %>%
    left_join(es_name_to_id, by = "name")
  es_scenario_4 <- st_read("data/scenario_boundaries/scenario_4_es.geojson") %>%
    mutate(name = str_sub(name, 1, -4)) %>%
    left_join(es_name_to_id, by = "name")
  es_scenario_5 <- st_read("data/scenario_boundaries/scenario_5_es.geojson") %>%
    mutate(name = str_sub(name, 1, -4)) %>%
    left_join(es_name_to_id, by = "name")
  
  # add Middle School scenario boundaries
  ms_scenario_2 <- st_read("data/scenario_boundaries/scenario_2_ms.geojson") %>%
    mutate(name = str_sub(name, 1, -4)) %>%
    left_join(ms_name_to_id, by = "name")
  ms_scenario_3 <- st_read("data/scenario_boundaries/scenario_3_ms.geojson") %>%
    mutate(name = str_sub(name, 1, -4)) %>%
    left_join(ms_name_to_id, by = "name")
  ms_scenario_4 <- st_read("data/scenario_boundaries/scenario_4_ms.geojson") %>%
    mutate(name = str_sub(name, 1, -4)) %>%
    left_join(ms_name_to_id, by = "name")
  ms_scenario_5 <- st_read("data/scenario_boundaries/scenario_5_ms.geojson") %>%
    mutate(name = str_sub(name, 1, -4)) %>%
    left_join(ms_name_to_id, by = "name")
  
  # add High School scenario boundaries
  hs_scenario_2 <- st_read("data/scenario_boundaries/scenario_2_hs.geojson") %>%
    mutate(name = str_sub(name, 1, -4)) %>%
    left_join(hs_name_to_id, by = "name")
  hs_scenario_3 <- st_read("data/scenario_boundaries/scenario_3_hs.geojson") %>%
    mutate(name = str_sub(name, 1, -4)) %>%
    left_join(hs_name_to_id, by = "name")
  hs_scenario_4 <- st_read("data/scenario_boundaries/scenario_4_hs.geojson") %>%
    mutate(name = str_sub(name, 1, -4)) %>%
    left_join(hs_name_to_id, by = "name")
  hs_scenario_5 <- st_read("data/scenario_boundaries/scenario_5_hs.geojson") %>%
    mutate(name = str_sub(name, 1, -4)) %>%
    left_join(hs_name_to_id, by = "name")
  
  ffx_blocks <- ffx_blocks %>%
    mutate(
      elem_current = es_current$elem25[
        geo_match(ffx_blocks, es_current, method = "area")],
      elem_scenario2 = es_scenario_2$elem25[
        geo_match(ffx_blocks, es_scenario_2, method = "area")],
      elem_scenario3 = es_scenario_3$elem25[
        geo_match(ffx_blocks, es_scenario_3, method = "area")],
      elem_scenario4 = es_scenario_4$elem25[
        geo_match(ffx_blocks, es_scenario_4, method = "area")],
      elem_scenario5 = es_scenario_5$elem25[
        geo_match(ffx_blocks, es_scenario_5, method = "area")],
      middle_current = ms_current$middle25[
        geo_match(ffx_blocks, ms_current, method = "area")],
      middle_scenario2 = ms_scenario_2$middle25[
        geo_match(ffx_blocks, ms_scenario_2, method = "area")],
      middle_scenario3 = ms_scenario_3$middle25[
        geo_match(ffx_blocks, ms_scenario_3, method = "area")],
      middle_scenario4 = ms_scenario_4$middle25[
        geo_match(ffx_blocks, ms_scenario_4, method = "area")],
      middle_scenario5 = ms_scenario_5$middle25[
        geo_match(ffx_blocks, ms_scenario_5, method = "area")],
      high_current = hs_current$high25[
        geo_match(ffx_blocks, hs_current, method = "area")],
      high_scenario2 = hs_scenario_2$high25[
        geo_match(ffx_blocks, hs_scenario_2, method = "area")],
      high_scenario3 = hs_scenario_3$high25[
        geo_match(ffx_blocks, hs_scenario_3, method = "area")],
      high_scenario4 = hs_scenario_4$high25[
        geo_match(ffx_blocks, hs_scenario_4, method = "area")],
      high_scenario5 = hs_scenario_5$high25[
        geo_match(ffx_blocks, hs_scenario_5, method = "area")]
    )
  
  # read in Fairfax County region data
  ffx_regions <- read_sf("data/School_Regions/School_Regions.shp")
  ffx_blocks <- ffx_blocks %>%
    mutate(region = ffx_regions$REGION[
      geo_match(ffx_blocks, ffx_regions, method = "area")])
  
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