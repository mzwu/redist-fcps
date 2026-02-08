#' Get the indices of rows in the map that correspond to locations of schools, 
#' and capacity of each school in order of increasing school area ID
#'
#' @param schools an `sf` object with a `geom_point` column denoting school locations
#' @param map a `redist_map` object
#'
#' @return a list of the row number in map that contains each school, 1-indexed
#' @export
get_schools_idx <- function(schools, map) {
  # get longitude and latitude for each school
  schools <- schools %>%
    mutate(lon = st_coordinates(.)[,1],
           lat = st_coordinates(.)[,2])
  
  # get row IDs of school locations in map
  schools_sf <- st_as_sf(schools, coords = c("lon", "lat"), crs = 4326)
  schools_sf <- st_transform(schools_sf, st_crs(map))
  map_rowid <- map |> mutate(block_row = row_number())
  
  # add row number
  schools_info <- schools_sf %>%
    st_join(
      map_rowid["block_row"],
      join = sf::st_within
    )
  
  # select relevant rows in ascending ID order
  schools_info <- schools_info %>%
    select(map_idx = block_row, OBJECTID) %>%
    arrange(OBJECTID)
  
  # ensure valid integers
  schools_info$map_idx <- schools_info$map_idx |> as.integer()
  schools_info <- schools_info %>%
    filter(!is.na(map_idx))
  
  schools_info$map_idx
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
  mat <- as.matrix(plans %>% subset_sampled())
  
  # rows with schools only
  school_assign <- mat[schools_idx, , drop = FALSE]
  
  # mark plans where any district repeats among the school blocks
  has_conflict <- apply(school_assign, 2, function(x) {
    x2 <- na.omit(x)
    any(duplicated(x2))
  })
  
  # determine valid plans (no conflicts)
  keep_plan <- which(!has_conflict)
  
  # keep valid plans and enacted plan
  plans %>%
    filter(str_detect(draw, "scenario") | str_detect(draw, "current") | (draw %in% keep_plan))
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
      mutate("{level}_starter{i}" := get_plans_matrix(lower_plans)[,draws[[i]]+1])
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
      attendance_islands = attendance_islands(plans, map),
      disrupt_count = disrupt_count(plans, map, current)
    )
  
  plans
}

#' Add split feeder statistics to the simulated plans
#'
#' @param plans a `redist_plans` object
#' @param map a `redist_map` object
#' @param starter_name enacted plan for the lower level of sims
#' @param level string describing level of current sims, "elem" or "middle" or "high"
#' @param ... additional summary statistics to compute
#'
#' @return a modified `redist_plans` object
#' @export
add_split_feeder_stats <- function(plans, map, starter_name, level, ...) {
  # lower level attendance area splits and split feeder counts
  if (level == "middle") {
    plans <- plans |>
      dplyr::mutate(elem_split_feeders = split_feeders(plans,
                                                       lower = map[[starter_name]],
                                                       pop = map$pop))
  } else if (level == "high") {
    plans <- plans |>
      dplyr::mutate(middle_split_feeders = split_feeders(plans,
                                                         lower = map[[starter_name]],
                                                         pop = map$pop))
  }
  
  plans
}

#' Create a plot validating an analysis
#'
#' @param plans a `redist_plans` object
#' @param map a `redist_map` object
#' @param level string describing level of current sims, "elem" or "middle" or "high"
#'
#' @return the output path, invisibly
#' @export
validate_analysis <- function(plans, map, level) {
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
  
  p_max_commute <- plot(plans, max_commute, geom = "boxplot") + labs(title = "Maximum Commute (min)") + theme_bw()
  
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
  
  if (level != "elem") {
  
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
  }
  
  draws <- sample(levels(subset_sampled(plans)$draw), 3)
  p_ex1 <- redist.plot.plans(plans, draws[1], map)
  p_ex2 <- redist.plot.plans(plans, draws[2], map)
  p_ex3 <- redist.plot.plans(plans, draws[3], map)
  
  layout <- "
AAABBBCCC
DDDDDDEEE
FFFFFFGGG
HHHHHHIII
JJJKKKLLL"
  
  if (level == "elem") {
    title <- "Elementary School"
  } else if (level == "middle") {
    title <- "Middle School"
  } else {
    title <- "High School"
  }
  
  p <- patchwork::wrap_plots(A = p_div, B = p_dev, C = p_island, 
                             D = p_comp, E = p_comp_frac, F = p_max_commute,
                             G = p_outside_zone, H = p_capacity, I = p_split,
                             J = p_ex1, K = p_ex2, L = p_ex3, design = layout) +
    patchwork::plot_annotation(title = str_c(title, " Validation")) +
    patchwork::plot_layout(guides = "collect")
  
  out_path <- here(str_glue("data-raw/{level}/validation/validation_{format(Sys.time(), '%Y%m%d_%H%M')}.png"))
  ggsave(out_path, plot = p, height = 15, width = 15)
  if (rstudioapi::isAvailable()) rstudioapi::viewer(out_path)
  invisible(out_path)
}

#' Plot heatmap of each block's commute distance to currently assigned school
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
current_commute_heatmap <- function(plans, map, schools_idx, commute_times, level) {
  # compute commute times for each block in enacted plan
  if (level == "elem") {
    blocks <- map %>%
      mutate(
        current_commute = commute_times[cbind(row_number(), elem_current)]
      )
    title <- "Elementary School: Current Commute"
  }
  else if (level == "middle") {
    blocks <- map %>%
      mutate(
        current_commute = commute_times[cbind(row_number(), middle_current)]
      )
    title <- "Middle School: Current Commute"
  }
  else if (level == "high") {
    blocks <- map %>%
      mutate(
        current_commute = commute_times[cbind(row_number(), high_current)]
      )
    title <- "High School: Current Commute"
  }
  
  p_blocks <- blocks %>%
    ggplot() +
    geom_sf(aes(fill = current_commute / 60)) +
    labs(title = title) +
    scale_fill_viridis_c("Current Commute \n(min)") +
    theme_bw()
  
  p_blocks
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
        current_commute = commute_times[cbind(row_number(), elem_current)]
      )
    title <- "Elementary School: Simulated Commute Change"
  }
  else if (level == "middle") {
    blocks <- map %>%
      mutate(
        current_commute = commute_times[cbind(row_number(), middle_current)]
      )
    title <- "Middle School: Simulated Commute Change"
  }
  else if (level == "high") {
    blocks <- map %>%
      mutate(
        current_commute = commute_times[cbind(row_number(), high_current)]
      )
    title <- "High School: Simulated Commute Change"
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
    geom_sf(aes(fill = (avg_sim_commute - current_commute) / 60)) +
    labs(
      title = title
    ) +
    scale_fill_viridis_c("Average Simulated - \nCurrent Commute \n(min)") +
    theme_bw()
  
  p_blocks
}

#' Plot double box plots of enacted commute distribution vs simulated commute distribution
#'
#' @param plans a `redist_plans` object
#' @param plans_sb a `redist_plans` object generated using shortburst
#' @param map a `redist_map` object
#' @param schools_idx a vector containing the map indices of each school
#'                    (ordered by normalized ID)
#' @param commute_times a matrix containing commute distances between each block
#'                      and each school
#' @param level a string of "elem", "middle", or "high" denoting the current level
#'
#' @return a heatmap plot
#' @export
comparison_boxplots <- function(plans, plans_sb, map, schools_idx, commute_times, level) {
  # compute commute times for each block in enacted plan
  if (level == "elem") {
    blocks <- map %>%
      mutate(
        current_commute = commute_times[cbind(row_number(), elem_current)],
        scenario2_commute = commute_times[cbind(row_number(), elem_scenario2)],
        scenario3_commute = commute_times[cbind(row_number(), elem_scenario3)],
        scenario4_commute = commute_times[cbind(row_number(), elem_scenario4)],
        scenario5_commute = commute_times[cbind(row_number(), elem_scenario5)]
      )
    title <- "Elementary School: Commute Times Across Regions"
  }
  else if (level == "middle") {
    blocks <- map %>%
      mutate(
        current_commute = commute_times[cbind(row_number(), middle_current)],
        scenario2_commute = commute_times[cbind(row_number(), middle_scenario2)],
        scenario3_commute = commute_times[cbind(row_number(), middle_scenario3)],
        scenario4_commute = commute_times[cbind(row_number(), middle_scenario4)],
        scenario5_commute = commute_times[cbind(row_number(), middle_scenario5)]
      )
    title <- "Middle School: Commute Times Across Regions"
  }
  else if (level == "high") {
    blocks <- map %>%
      mutate(
        current_commute = commute_times[cbind(row_number(), high_current)],
        scenario2_commute = commute_times[cbind(row_number(), high_scenario2)],
        scenario3_commute = commute_times[cbind(row_number(), high_scenario3)],
        scenario4_commute = commute_times[cbind(row_number(), high_scenario4)],
        scenario5_commute = commute_times[cbind(row_number(), high_scenario5)]
      )
    title <- "High School: Commute Times Across Regions"
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
  
  mat <- as.matrix(plans_sb)
  n_blocks <- nrow(mat)
  n_plans <- ncol(mat)
  block_commutes <- matrix(NA, nrow = n_blocks, ncol = n_plans)
  for (p in 1:n_plans) {
    districts <- mat[, p]
    block_commutes[, p] <- commute_times[cbind(1:n_blocks, districts)]
  }
  avg_commute_per_block <- rowMeans(block_commutes, na.rm = TRUE)
  blocks$avg_sb_commute <- avg_commute_per_block
  
  # expand by duplicating rows according to the population of that block
  blocks_expanded <- blocks[rep(seq_len(nrow(blocks)), pmax(blocks$pop, 1)), ]
  
  blocks_long <- blocks_expanded %>%
    pivot_longer(
      cols = c(
        current_commute,
        scenario2_commute,
        scenario3_commute,
        scenario4_commute,
        scenario5_commute,
        avg_sim_commute,
        avg_sb_commute
      ),
      names_to = "scenario",
      values_to = "commute"
    )
  
  blocks_long <- blocks_long %>%
    mutate(
      scenario = factor(
        scenario,
        levels = c(
          "current_commute",
          "scenario2_commute",
          "scenario3_commute",
          "scenario4_commute",
          "scenario5_commute",
          "avg_sim_commute",
          "avg_sb_commute"
        ),
        labels = c(
          "Current",
          "Scenario 2",
          "Scenario 3",
          "Scenario 4",
          "Scenario 5",
          "Simulated",
          "Shortburst"
        )
      )
    )
  
  p_blocks <- blocks_long %>%
    ggplot(aes(
      x = factor(region),
      y = commute / 60,
      color = scenario
    )) +
    geom_boxplot(
      position = position_dodge(width = 0.8),
      width = 0.6,
      fill = NA
    ) +
    labs(
      x = "Region",
      y = "Commute Time (min)",
      color = "Commute Times",
      title = title
    ) +
    scale_color_discrete(
      labels = c(
        current_commute   = "Current",
        scenario2_commute = "Scenario 2",
        scenario3_commute = "Scenario 3",
        scenario4_commute = "Scenario 4",
        scenario5_commute = "Scenario 5",
        avg_sim_commute   = "Simulated",
        avg_sb_commute    = "Shortburst"
      ),
      name = "Commute Times"
    ) +
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