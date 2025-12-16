#' Get a map between official school area IDs and normalized (1-ndist) IDs
#'
#' @param shp a shapefile object with both official school area ID and normalized ID
#' @param level either elem, middle, or high
#'
#' @return a data frame with 2 columns, one with the official school district ID and the other with a normalized ID
get_schools_id_map <- function(shp, level) {
  shp %>%
    st_drop_geometry() %>%
    select(
      paste0(level, "25_id"),
      paste0(level, "25")
    ) %>%
    unique()
}

#' Get the indices of rows in the map that correspond to locations of schools, and capacity of each school
#' in order of increasing school area ID
#'
#' @param schools an `sf` object with a `geom_point` column denoting school locations
#' @param map a `redist_map` object
#' @param shp a shapefile object with both official school area ID and normalized ID
#' @param capacity a dataframe containing the capacity, name, and ID (mapping to schools) of each school
#' @param level either elem, middle, or high
#'
#' @return a vector containing the map indices of each school
#' @export
get_schools_info <- function(schools, map, shp, capacity, level) {
  # get longitude and latitude for each school
  schools <- schools %>%
    mutate(lon = st_coordinates(.)[,1],
           lat = st_coordinates(.)[,2])
  
  # get row IDs of school locations in redist_map
  schools_sf <- st_as_sf(schools, coords = c("lon", "lat"), crs = 4326)
  schools_sf <- st_transform(schools_sf, st_crs(map))
  map_rowid <- map |> mutate(tract_row = row_number())
  
  # add row number and capacity data
  schools_info <- schools_sf %>%
    st_join(
      map_rowid["tract_row"],
      join = sf::st_within
    ) %>%
    merge(
      capacity, 
      by.x="OBJECTID", 
      by.y="object_id_school"
    )
  
  # get normalized school area ID
  schools_id_map <- get_schools_id_map(shp, level)
  schools_info <- schools_info %>%
    merge(
      schools_id_map,
      by.x = "object_id_area",
      by.y = paste0(level, "25_id")
    )
  
  # select relevant rows in ascending ID order
  schools_info <- schools_info %>%
    select(
      map_idx = tract_row,
      paste0(level, "25"),
      type,
      name,
      capacity,
      geometry
    ) %>%
    arrange(.data[[paste0(level, "25")]])
  
  # 1-indexed
  schools_info$map_idx <- schools_info$map_idx |> as.integer()
  schools_info <- schools_info %>%
    filter(!is.na(map_idx))
  
  schools_info
}

#' Filter down to only plans where each school is assigned to a distinct district
#'
#' @param plans a `redist_plans` object
#' @param schools_idx a vector containing the map indices of each school
#'
#' @return a filtered down `redist_plans` object
#' @export
drop_duplicate_schools <- function(plans, schools_idx) {
  # get plans matrix
  mat <- as.matrix(plans)
  
  # rows with schools only
  school_assign <- mat[schools_idx, , drop = FALSE]
  
  # mark plans where any district repeats among the school tracts
  has_conflict <- apply(school_assign, 2, function(x) {
    x2 <- na.omit(x)
    any(duplicated(x2))
  })
  
  # keep only valid plans (no conflicts)
  keep_plan <- which(!has_conflict) - 1 # first plan is enacted
  
  plans %>%
    filter((draw %in% c("elem25", "middle25", "high25")) | (draw %in% keep_plan))
}

#' Filter down to only plans where each school is assigned to a distinct district - region version
#'
#' @param plans a `redist_plans` object
#' @param schools_idx a vector containing the map indices of each school
#'
#' @return a filtered down `redist_plans` object
#' @export
drop_duplicate_schools_regions <- function(plans, schools_idx) {
  # get plans matrix
  mat <- as.matrix(plans)
  
  # rows with schools only
  school_assign <- mat[schools_idx, , drop = FALSE]
  
  # mark plans where any district repeats among the school tracts
  has_conflict <- apply(school_assign, 2, function(x) {
    x2 <- x[x != 0 & !is.na(x)]
    any(duplicated(x2))
  })
  
  # keep only valid plans (no conflicts)
  keep_plan <- which(!has_conflict)
  
  plans %>%
    filter((draw %in% c("elem25", "middle25", "high25")) | (draw %in% keep_plan))
}

#' Add starter lower level plans to serve as counties
#'
#' @param shp shapefile object
#' @param lower_plans `redist_plans` object with all plans
#' @param draws specific draws to add as starters
#' @param level "elem" or "middle" describing the lower level
#'
#' @return a filtered down `redist_plans` object
#' @export
add_starter_plans <- function(shp, lower_plans, draws, level) {
  for (i in 1:length(draws)) {
    shp <- shp %>%
      mutate("{level}{i}" := get_plans_matrix(lower_plans)[,draws[[i]]+1])
  }
  shp
}

#' Add summary statistics to the simulated plans
#'
#' @param plans a `redist_plans` object
#' @param map a `redist_map` object
#' @param current enacted plan for level of current sims
#' @param schools indices of school locations in map
#' @param commute_times matrix of commute times from blocks to schools
#' @param level string describing level of current sims, "elem" or "middle" or "high"
#' @param ... additional summary statistics to compute
#'
#' @return a modified `redist_plans` object
#' @export
add_summary_stats <- function(plans, map, current, schools, commute_times, level, ...) {
  plans <- plans %>%
    mutate(
      plan_dev = plan_parity(map),
      comp_polsby = comp_polsby(plans, map),
      phase_commute = phase_commute(plans, map, current = current,
                                    schools = schools,
                                    commute_times = commute_times),
      max_commute = max_commute(plans, map, schools = schools,
                                commute_times = commute_times)
    )
  
  if (level == "middle") {
    plans <- plans |>
      dplyr::mutate(elem_splits = redistmetrics::splits_admin(plans, map, elem25))
  } else if (level == "high") {
    plans <- plans |>
      dplyr::mutate(middle_splits = redistmetrics::splits_admin(plans, map, middle25))
  }
  
  plans
}

#' Create a plot validating an analysis
#'
#' @param plans a `redist_plans` object
#' @param map a `redist_map` object
#'
#' @return the output path, invisibly
#' @export
validate_analysis <- function(plans, map) {
  p_weights <- plot(plans) + theme_bw()
  
  plan_div <- data.frame(plan_div = plans_diversity(plans, n_max = 150))
  p_div <- plan_div %>% 
    ggplot(aes(x = plan_div)) +
    geom_histogram(bins = 40) +
    labs(x = "VI distance", title = "Plan diversity") +
    theme_bw()
  
  p_dev <- hist(plans, plan_dev, bins = 40) + labs(title = "Population deviation") + theme_bw()
  
  p_comp <- plot(plans, comp_polsby, geom = "boxplot") + labs(title = "Compactness: Polsby-Popper") + theme_bw()
  
  p_commute <- plot(plans, phase_commute, geom = "boxplot") + labs(title = "Phase-In Commute Disruption") + theme_bw()
  
  p_max_commute <- plot(plans, max_commute, geom = "boxplot") + labs(title = "Max Commute") + theme_bw()
  
  if ("elem_splits" %in% names(plans)) {
    p_split1 <- hist(plans, elem_splits) + labs(title = "Elementary feeder splits") + theme_bw()
  } else p_split1 <- patchwork::plot_spacer()
  if ("middle_splits" %in% names(plans)) {
    p_split2 <- hist(plans, middle_splits) + labs(title = "Middle feeder splits") + theme_bw()
  } else p_split2 <- patchwork::plot_spacer()
  
  enacted_summary <- plans %>%
    filter(draw == attr(map, "existing_col")) %>%
    select(district, comp_polsby, phase_commute, max_commute) %>%
    mutate(
      district_label = str_pad(district, width = 2, pad = '0'),
      compact_rank = rank(comp_polsby),
      commute_rank = rank(phase_commute),
      max_commute_rank = rank(max_commute),
    )
  
  p_comp <- p_comp +
    geom_text(data = enacted_summary,
              aes(
                x = compact_rank,
                label = district_label
              ),
              vjust = 3,
              y = Inf,
              size = 2.5,
              fontface = "bold",
              lineheight = 0.8,
              alpha = 0.8,
              color = "red")
  
  draws <- sample(levels(subset_sampled(plans)$draw), 3)
  p_ex1 <- redist.plot.plans(plans, draws[1], map)
  p_ex2 <- redist.plot.plans(plans, draws[2], map)
  p_ex3 <- redist.plot.plans(plans, draws[3], map)
  
  layout <- "
AAABBB
CCCCCC
DDDDDD
EEEEEE
FFFFFF
GGGHHH
IIJJKK
IIJJKK"
  
  p <- patchwork::wrap_plots(A = p_weights, B = p_div, C = p_dev, 
                             D = p_comp, E = p_commute, F = p_max_commute,
                             G = p_split1, H = p_split2,
                             I = p_ex1, J = p_ex2, K = p_ex3, design = layout) +
    patchwork::plot_annotation(title = str_c(plans$draw[1], " Validation")) +
    patchwork::plot_layout(guides = "collect")
  
  out_path <- here(str_glue("data-raw/{plans$draw[1]}/validation/validation_{format(Sys.time(), '%Y%m%d_%H%M')}.png"))
  ggsave(out_path, plot = p, height = 15, width = 10)
  if (rstudioapi::isAvailable()) rstudioapi::viewer(out_path)
  invisible(out_path)
}

#' Plot heatmap of each tract's average sim - enacted commute distance to assigned school
#'
#' @param plans a `redist_plans` object
#' @param map a `redist_map` object
#' @param schools_idx a vector containing the map indices of each school
#' @param commute_times a matrix containing commute distances between each tract
#'
#' @return a heatmap plot
#' @export
projected_average_heatmap <- function(plans, map, schools_idx, commute_times, level) {
  if (level == "elem") {
    tracts <- map %>%
      mutate(same_area = map(elem25, ~ as.integer(which(map$elem25 == .x))),
             school_row = map_int(
               same_area,
               ~ { hits <- which(schools_idx %in% .x)
               if (length(hits)) hits[1] else NA_integer_ }
             ),
             current_commute = commute_times[cbind(row_number(), school_row)]) %>%
      select(-same_area)
  }
  else if (level == "middle") {
    tracts <- map %>%
      mutate(same_area = map(middle25, ~ as.integer(which(map$middle25 == .x))),
             school_row = map_int(
               same_area,
               ~ { hits <- which(schools_idx %in% .x)
               if (length(hits)) hits[1] else NA_integer_ }
             ),
             current_commute = commute_times[cbind(row_number(), school_row)]) %>%
      select(-same_area)
  }
  else if (level == "high") {
    tracts <- map %>%
      mutate(same_area = map(high25, ~ as.integer(which(map$high25 == .x))),
             school_row = map_int(
               same_area,
               ~ { hits <- which(schools_idx %in% .x)
               if (length(hits)) hits[1] else NA_integer_ }
             ),
             current_commute = commute_times[cbind(row_number(), school_row)]) %>%
      select(-same_area)
  }
  
  # compute average commute time across simulated plans
  mat <- as.matrix(plans)
  n_tracts <- nrow(mat)
  n_plans <- ncol(mat)
  tract_commutes <- matrix(NA, nrow = n_tracts, ncol = n_plans)
  for (p in 1:n_plans) {
    districts <- mat[, p]
    school_districts <- mat[schools_idx, p]
    school_cols <- match(districts, school_districts)
    tract_commutes[, p] <- commute_times[cbind(1:n_tracts, school_cols)]
  }
  avg_commute_per_tract <- rowMeans(tract_commutes, na.rm = TRUE)
  tracts$avg_sim_commute <- avg_commute_per_tract
  
  p_tracts <- tracts %>%
    ggplot() +
    geom_sf(aes(fill = avg_sim_commute - current_commute)) +
    scale_fill_viridis_c("Average Simulated - \nCurrent Commute \n(seconds)") +
    theme_bw() +
    geom_sf(data = ffx_hs)
  
  p_tracts
}