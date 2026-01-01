#' Create map/translation between official school area IDs and normalized (1 to 
#' ndist) IDs
#'
#' @param shp a shapefile object with both official school area ID (level25_id) 
#'        and normalized ID (level25)
#' @param level a string of either "elem", "middle", or "high"
#'
#' @return a data frame with 2 columns, one with the official school district ID 
#'         (level25_id) and the other with a normalized ID (level25)
get_schools_id_map <- function(shp, level) {
  shp %>%
    st_drop_geometry() %>%
    select(
      paste0(level, "25_id"),
      paste0(level, "25")
    ) %>%
    unique()
}

#' Get the indices of rows in the map that correspond to locations of schools, 
#' and capacity of each school in order of increasing school area ID
#'
#' @param schools an `sf` object with a `geom_point` column denoting school locations
#' @param map a `redist_map` object
#' @param shp a shapefile object with both official school area ID and normalized ID
#' @param capacity a dataframe containing the capacity, name, and ID 
#'                 (school and area ID) of each school
#' @param level a string of either "elem", "middle", or "high"
#'
#' @return an `sf` object containing the normalized ID, type (ES/MS/HS), name, capacity,
#'          map_idx (the row number in map that contains the school, 1-indexed)
#' @export
get_schools_info <- function(schools, map, shp, capacity, level) {
  # get longitude and latitude for each school
  schools <- schools %>%
    mutate(lon = st_coordinates(.)[,1],
           lat = st_coordinates(.)[,2])
  
  # get row IDs of school locations in map
  schools_sf <- st_as_sf(schools, coords = c("lon", "lat"), crs = 4326)
  schools_sf <- st_transform(schools_sf, st_crs(map))
  map_rowid <- map |> mutate(block_row = row_number())
  
  # add row number and capacity data
  schools_info <- schools_sf %>%
    st_join(
      map_rowid["block_row"],
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
      map_idx = block_row,
      paste0(level, "25"),
      type,
      name,
      capacity,
      geometry
    ) %>%
    arrange(.data[[paste0(level, "25")]])
  
  # ensure valid integers
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
  
  # mark plans where any district repeats among the school blocks
  has_conflict <- apply(school_assign, 2, function(x) {
    x2 <- na.omit(x)
    any(duplicated(x2))
  })
  
  # determine valid plans (no conflicts)
  # first plan is enacted so index i is draw i-1
  keep_plan <- which(!has_conflict) - 1
  
  # keep valid plans and enacted plan
  plans %>%
    filter((draw %in% c("elem25", "middle25", "high25")) | (draw %in% keep_plan))
}

#' Filter down to only plans where each school is assigned to a distinct 
#' district - region version
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
  
  # mark plans where any district repeats among the school blocks
  has_conflict <- apply(school_assign, 2, function(x) {
    x2 <- x[x != 0 & !is.na(x)]
    any(duplicated(x2))
  })
  
  # determine valid plans (no conflicts)
  keep_plan <- which(!has_conflict)
  
  # keep valid plans
  plans %>%
    filter(draw %in% keep_plan)
}

#' Add starter lower level plans to serve as counties
#'
#' @param shp a shapefile object
#' @param lower_plans a `redist_plans` object with all lower level plans
#' @param draws a vector of integers specifying which draws to add as starters
#' @param level a string of "elem" or "middle" describing the lower level
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
#' @param current enacted plan for the current level of sims
#' @param schools a vector of indices of school locations in map
#' @param commute_times a matrix of commute times from blocks to schools
#' @param capacity a vector of enrollment capacities for each school
#' @param level string describing level of current sims, "elem" or "middle" or "high"
#' @param ... additional summary statistics to compute
#'
#' @return a modified `redist_plans` object
#' @export
add_summary_stats <- function(plans, map, current, schools, commute_times, capacity, level, ...) {
  plans <- plans %>%
    mutate(
      plan_dev = plan_parity(map),
      comp_edge = comp_frac_kept(plans, map),
      comp_polsby = comp_polsby(plans, map),
      phase_commute = phase_commute(plans, map, 
                                    current = current,
                                    commute_times = commute_times),
      max_commute = max_commute(plans, map, 
                                commute_times = commute_times),
      capacity_util = capacity_util(plans, 
                                    schools_capacity = capacity, 
                                    pop = map$pop),
      school_outside_zone = school_outside_zone(plans,
                                                schools = schools),
      attendance_islands = attendance_islands(plans, map)
    )
  
  # lower level attendance area splits and split feeder counts
  if (level == "middle") {
    plans <- plans |>
      dplyr::mutate(elem_splits = redistmetrics::splits_admin(plans, map, elem25)) |>
      dplyr::mutate(elem_split_feeders = split_feeders(plans,
                                                       lower = map$elem25,
                                                       pop = map$pop))
  } else if (level == "high") {
    plans <- plans |>
      dplyr::mutate(middle_splits = redistmetrics::splits_admin(plans, map, middle25)) |>
      dplyr::mutate(middle_split_feeders = split_feeders(plans,
                                                         lower = map$middle25,
                                                         pop = map$pop))
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
    labs(x = "VI Distance", title = "Plan Diversity") +
    theme_bw()
  
  p_dev <- hist(plans, plan_dev, bins = 40) + labs(title = "Population Deviation") + theme_bw()
  
  p_comp <- plot(plans, comp_polsby, geom = "boxplot") + labs(title = "Compactness: Polsby-Popper") + theme_bw()
  
  p_comp_frac <- hist(plans, comp_edge, bins = 40) + labs(title = "Compactness: Fraction Kept") + theme_bw()
  
  p_commute <- plot(plans, phase_commute, geom = "boxplot") + labs(title = "Phase-In Commute Disruption") + theme_bw()
  
  p_max_commute <- plot(plans, max_commute, geom = "boxplot") + labs(title = "Max Commute") + theme_bw()
  
  p_capacity <- plot(plans, capacity_util, geom = "boxplot") + labs(title = "Capacity Utilization") + theme_bw()
  
  p_outside_zone <- hist(plans, school_outside_zone, bins = 5) + labs(title = "Schools Outside Zone") + theme_bw()
  
  p_island <- hist(plans, attendance_islands, bins = 5) + labs(title = "Attendance Islands") + theme_bw()
  
  if ("elem_split_feeders" %in% names(plans)) {
    p_split <- hist(plans, elem_split_feeders) + labs(title = "Elementary Split Feeders") + theme_bw()
  } else if ("middle_split_feeders" %in% names(plans)) {
    p_split <- hist(plans, middle_split_feeders) + labs(title = "Middle Split Feeders") + theme_bw()
  } else {
    p_split <- patchwork::plot_spacer()
  }
  
  enacted_summary <- plans %>%
    filter(draw == attr(map, "existing_col")) %>%
    select(district, comp_polsby, phase_commute, max_commute, capacity_util) %>%
    mutate(
      district_label = str_pad(district, width = 2, pad = '0'),
      compact_rank = rank(comp_polsby),
      commute_rank = rank(phase_commute),
      max_commute_rank = rank(max_commute),
      capacity_rank = rank(capacity_util)
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
  
  # NOTE: all labels are stacked on top of each other because
  # enacted plan has phase commute = 0 for every district
  p_commute <- p_commute +
    geom_text(data = enacted_summary,
              aes(
                x = commute_rank,
                label = district_label
              ),
              vjust = 3,
              y = Inf,
              size = 2.5,
              fontface = "bold",
              lineheight = 0.8,
              alpha = 0.8,
              color = "red")
  
  p_max_commute <- p_max_commute +
    geom_text(data = enacted_summary,
              aes(
                x = max_commute_rank,
                label = district_label
              ),
              vjust = 3,
              y = Inf,
              size = 2.5,
              fontface = "bold",
              lineheight = 0.8,
              alpha = 0.8,
              color = "red")
  
  p_capacity <- p_capacity +
    geom_text(data = enacted_summary,
              aes(
                x = capacity_rank,
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
AAABBBCCC
DDDDDDEEE
FFFFFFGGG
HHHHHHIII
JJJJJJKKK
LLLMMMNNN"
  
  p <- patchwork::wrap_plots(A = p_weights, B = p_div, C = p_dev, 
                             D = p_comp, E = p_comp_frac, F = p_commute,
                             G = p_split, H = p_max_commute, 
                             I = p_outside_zone, J = p_capacity, K = p_island, 
                             L = p_ex1, M = p_ex2, N = p_ex3, design = layout) +
    patchwork::plot_annotation(title = str_c(plans$draw[1], " Validation")) +
    patchwork::plot_layout(guides = "collect")
  
  out_path <- here(str_glue("data-raw/{plans$draw[1]}/validation/validation_{format(Sys.time(), '%Y%m%d_%H%M')}.png"))
  ggsave(out_path, plot = p, height = 15, width = 15)
  if (rstudioapi::isAvailable()) rstudioapi::viewer(out_path)
  invisible(out_path)
}

#' Plot heatmap of each block's average sim - enacted commute distance to assigned school
#'
#' @param plans a `redist_plans` object
#' @param map a `redist_map` object
#' @param schools_idx a vector containing the map indices of each school
#'                    (ordered by normalized ID)
#' @param commute_times a matrix containing commute distances between each block
#'                      and each school
#' @param level a string of "elem", "middle", or "high" denoting the current level
#'
#' @return a heatmap plot
#' @export
projected_average_heatmap <- function(plans, map, schools_idx, commute_times, level) {
  # compute commute times for each block in enacted plan
  if (level == "elem") {
    blocks <- map %>%
      mutate(
        current_commute = commute_times[row_number(), schools_idx[elem25]]
      )
  }
  else if (level == "middle") {
    blocks <- map %>%
      mutate(
        current_commute = commute_times[row_number(), schools_idx[middle25]]
      )
  }
  else if (level == "high") {
    blocks <- map %>%
      mutate(
        current_commute = commute_times[row_number(), schools_idx[high25]]
      )
  }
  
  # compute average commute time across simulated plans
  mat <- as.matrix(plans)
  n_blocks <- nrow(mat)
  n_plans <- ncol(mat)
  block_commutes <- matrix(NA, nrow = n_blocks, ncol = n_plans)
  for (p in 1:n_plans) {
    districts <- mat[, p]
    block_commutes[, p] <- commute_times[cbind(1:n_blocks, districts)]
  }
  avg_commute_per_block <- rowMeans(block_commutes, na.rm = TRUE)
  blocks$avg_sim_commute <- avg_commute_per_block
  
  p_blocks <- blocks %>%
    ggplot() +
    geom_sf(aes(fill = avg_sim_commute - current_commute)) +
    scale_fill_viridis_c("Average Simulated - \nCurrent Commute \n(seconds)") +
    theme_bw()
  
  p_blocks
}

#' Prep Particles for Partial SMC
#'
#' @param map primary map of entire state
#' @param map_plan_list A list of lists, where each inner list is named  'map' for
#' each `redist_map` and 'plans' for each `redist_plans`
#' @param uid unique id column identifying each row. Must be unique and the same
#' across `map` and each `redist_map` in `map_plan_list`
#' @param dist_keep column in each plans object identifying which districts to keep
#' @param nsims The number of samples to draw.
#'
#' @md
#' @return matrix of particles
#' @export
prep_particles <- function(map, map_plan_list, uid, dist_keep, nsims) {
  m_init <- matrix(0L, nrow = nrow(map), ncol = nsims)
  
  # set up id correspondence ----
  ids <- rlang::eval_tidy(rlang::enquo(uid), map)
  map_rows <- lapply(map_plan_list, function(x) {
    match(rlang::eval_tidy(rlang::enquo(uid), x$map), ids)
  })
  
  plans_m_l <- lapply(map_plan_list, function(x) {
    redist::get_plans_matrix(subset_sampled(x$plans))
  })
  
  keep_l <- lapply(map_plan_list, function(x) {
    keeps <- as.logical(rlang::eval_tidy(rlang::enquo(dist_keep), x$plans))
    if (is.null(keeps)) {
      keeps <- rep(TRUE, x$plans |> subset_sampled() |> nrow())
    }
    x$plans |>
      redist::subset_sampled() |>
      as_tibble() |>
      select(draw, district) |>
      filter(keeps) |>
      mutate(draw = as.integer(draw))
  })
  
  adds <- c(
    0,
    sapply(keep_l, function(x) {
      z <- x$draw[1]
      x |> filter(draw == z) |> nrow()
    }) |> cumsum()
  )
  
  plans_m_l <- lapply(seq_along(plans_m_l), function(i) {
    m <- plans_m_l[[i]]
    keep <- keep_l[[i]]
    for (j in seq_len(ncol(m))) {
      keep_j <- keep |>
        filter(draw == j) |>
        pull(district)
      m[!(m[, j] %in% keep_j), j] <- 0L
      m[, j] <- match(m[, j], sort(unique(m[, j]))) - as.integer(any(m[, j] == 0))
      m[, j] <- ifelse(m[, j] == 0, m[, j], m[, j] + adds[i])
    }
    m
  })
  
  for (i in seq_len(length(plans_m_l))) {
    n <- nsims/ncol(plans_m_l[[i]])
    m_init[map_rows[[i]], ] <- rep_cols(plans_m_l[[i]], n)
  }
  
  m_init
}

# Helpers
rep_col <- function(col, n) {
  matrix(rep(col, n), ncol = n, byrow = FALSE)
}
rep_cols <- function(mat, n) {
  do.call("cbind", lapply(seq_len(ncol(mat)), \(x) rep_col(mat[, x], n)))
}