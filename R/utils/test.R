# TEST ON TRACTS + HIGH SCHOOLS

ffx_shp %>%
  ggplot() +
  geom_sf()

ffx_schools <- read_sf("data/School_Facilities/School_Facilities.shp")
ffx_hs <- ffx_schools %>%
  filter(SCHOOL_TYP == "HS")

# add lon/lat columns
ffx_hs <- ffx_hs %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])


# make redist_map
map <- redist_map(ffx_shp, pop_tol = 0.05,
                  existing_plan = high25, adj = ffx_shp$adj)
attr(map, "analysis_name") <- "HS_25"

# get row IDs of school locations in redist_map
hs_sf <- st_as_sf(ffx_hs, coords = c("lon", "lat"), crs = 4326)
hs_sf <- st_transform(hs_sf, st_crs(map))
map_rowid <- map |> mutate(tract_row = row_number())

schools_idx <- hs_sf %>%
  st_join(
    map_rowid["tract_row"],
    join = sf::st_within
  )

# to account for R 1-indexing and C++ 0-indexing
schools_idx_vec <- schools_idx$tract_row |> as.integer() - 1L
schools_idx_vec <- schools_idx_vec[!is.na(schools_idx_vec)]

# make redist_constr
constr <- redist_constr(map) %>%
  add_constr_phase_commute(
    strength = 1,
    current = map$high25,
    schools_idx = schools_idx_vec
  )

# simulate plans
plans <- redist_smc(map, nsims = 100, runs = 1L,
                    counties = middle25, 
                    constr = constr, pop_temper = 0.01)
# plans <- match_numbers(plans, "HS_25")

# visualization
old_hs_areas <- ffx_shp %>%
  group_by(high25) %>%
  summarise(geometry = st_union(geometry))

# tracts with old HS attendance areas
ffx_shp %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = old_hs_areas,
          fill = NA, 
          color = "black", 
          lwd = 2)

# add summary stats
# plans_stats <- add_summary_stats(plans, map)
old_plan <- plans %>%
  filter(draw == "high25")
old_stats <- old_plan %>%
  mutate(comp_polsby = redistmetrics::comp_polsby(old_plan, map)) %>%
  arrange(comp_polsby)
sim_plans <- plans %>%
  filter(draw != "high25")
sim_stats <- sim_plans %>%
  mutate(comp_polsby = redistmetrics::comp_polsby(sim_plans, map)) %>%
  arrange(draw, comp_polsby)
p <- sim_stats %>%
  ggplot(aes(x = district, y = comp_polsby, group = district)) +
  geom_boxplot() +
  geom_point(data = old_stats, color = "orange",
             size = 3, pch = 23, fill = "orange") +
  labs(x = "Ordered Districts by Compactness \n (Polsby-Popper)",
       y = "Polsby-Popper Compactness") +
  # stat_summary(fun = median, geom = "line", group = 1, col = "black", lwd = 1.5, alpha = 0.8) +
  theme_bw()

# compactness histogram
p2 <- plot(sim_plans, comp_polsby, geom = "boxplot") + labs(title = "Compactness: Polsby-Popper") + theme_bw()

# osrm test
library(sf)
library(httr)
library(jsonlite)

osrm_table_safe <- function(src, dst, profile = "driving",
                            server = "http://127.0.0.1:5000",
                            measure = "duration") {
  stopifnot(measure %in% c("duration","distance"))
  
  src <- sf::st_transform(src, 4326)
  dst <- sf::st_transform(dst, 4326)
  
  to_str <- function(g) {
    paste(apply(sf::st_coordinates(g), 1, \(xy) paste0(xy[1], ",", xy[2])), collapse = ";")
  }
  src_str <- to_str(src)
  dst_str <- to_str(dst)
  
  n_src <- nrow(src)
  n_dst <- nrow(dst)
  
  # 0-based indices into the COMBINED list: "src;dst"
  src_idx <- paste0(seq(0, n_src - 1), collapse = ";")
  dst_idx <- paste0(seq(n_src, n_src + n_dst - 1), collapse = ";")
  
  base <- sub("/+$", "", trimws(server))
  url  <- sprintf("%s/table/v1/%s/%s;%s?sources=%s&destinations=%s",
                  base, profile, src_str, dst_str, src_idx, dst_idx)
  
  resp <- httr::GET(url)
  httr::stop_for_status(resp)
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  j <- jsonlite::fromJSON(txt)
  
  out <- list()
  if (!is.null(j$durations)) {
    dur <- j$durations
    out$duration <- if (is.list(dur)) {
      do.call(rbind, lapply(dur, function(row) as.numeric(row)))
    } else {
      as.matrix(dur)
    }
  }
  if (!is.null(j$distances)) {
    dsts <- j$distances
    out$distance <- if (is.list(dsts)) {
      do.call(rbind, lapply(dsts, function(row) as.numeric(row)))
    } else {
      as.matrix(dsts)
    }
  }
  out
}

options(osrm.server = "http://127.0.0.1:5000")
src <- st_as_sf(data.frame(id="A", lon=-77.0365, lat=38.8977),
                coords=c("lon","lat"), crs=4326)
dst <- st_as_sf(data.frame(id="B", lon=-77.4371, lat=37.5407),
                coords=c("lon","lat"), crs=4326)

tab <- osrm_table_safe(src, dst, profile="driving", server=getOption("osrm.server"))
tab$duration  # seconds

# score testing
redistmetrics::phase_commute(plans, ffx_shp, 
                             current = map$high25,
                             schools_idx = schools_idx_vec)
