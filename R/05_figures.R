region_map <- map %>%
  ggplot(aes(fill = factor(region))) +
  geom_sf() +
  labs(
    fill = "Region"
  ) +
  theme_void()
ggsave(
  filename = here("figures/region_map.png"),
  plot = region_map,
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)

# LOAD ELEMENTARY MAP

plans <- read_rds(here("data-raw/elem/plans/plans_es_mcmc_com1_inc12_cap10_pop0.66.rds"))

plans <- add_summary_stats(
  plans, map, 
  current = map$elem_current, 
  schools = schools_idx, 
  commute_times = commute_times, 
  capacity = schools_capacity,
  level = "elem"
)

elem_comp <- plot(plans, comp_polsby, geom = "boxplot") + 
  labs(x = "Ordered Attendance Area", y = "Polsby-Popper") + 
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 4, angle = 90)
  )
ggsave(
  filename = here("figures/elem_comp.png"),
  plot = elem_comp,
  width = 10,
  height = 4,
  units = "in",
  dpi = 300
)

elem_max_commute <- plot(plans, max_commute, geom = "boxplot") + 
  labs(x = "Ordered Attendance Area", y = "Maximum Commute (min)") + 
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 4, angle = 90)
  )
ggsave(
  filename = here("figures/elem_max_commute.png"),
  plot = elem_max_commute,
  width = 10,
  height = 4,
  units = "in",
  dpi = 300
)

elem_polsby <- plans %>%
  ggplot(aes(x = comp_polsby, y = max_commute)) +
  geom_point(size=1) +
  labs(
    x = "Compactness: Polsby-Popper",
    y = "Maximum Commute (min)"
  ) +
  theme_bw()
ggsave(
  filename = here("figures/elem_polsby.png"),
  plot = elem_polsby,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

elem_edge <- plans %>%
  ggplot(aes(x = comp_edge, y = max_commute)) +
  geom_point(size=1) +
  labs(
    x = "Compactness: Fraction of Edges Kept",
    y = "Maximum Commute (min)"
  ) +
  theme_bw()
ggsave(
  filename = here("figures/elem_edge.png"),
  plot = elem_edge,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

elem_commute_heatmap <- projected_average_heatmap(
  plans, 
  map, 
  schools_idx, 
  commute_times, 
  "elem"
)
ggsave(
  filename = here("figures/elem_commute_heatmap.png"),
  plot = elem_commute_heatmap,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

elem_cap_heatmap <- capacity_improvement_heatmap(
  plans,
  map,
  schools_idx,
  schools_capacity,
  "elem"
)
ggsave(
  filename = here("figures/elem_cap_heatmap.png"),
  plot = elem_cap_heatmap,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

plans_sb <- read_rds(here("data-raw/elem/plans_es_sb_pop0.66.rds"))

elem_commute_boxplots <- comparison_boxplots(
  plans %>% subset_sampled(), 
  plans_sb %>% subset_sampled(), 
  map, schools_idx, commute_times, "elem"
)
ggsave(
  filename = here("figures/elem_commute_boxplots.png"),
  plot = elem_commute_boxplots,
  width = 10,
  height = 4,
  units = "in",
  dpi = 300
)

elem_cap_boxplots <- capacity_boxplots(
  plans %>% subset_sampled(),
  plans_sb %>% subset_sampled(),
  map, schools_idx, schools_capacity, "elem"
)
ggsave(
  filename = here("figures/elem_cap_boxplots.png"),
  plot = elem_cap_boxplots,
  width = 10,
  height = 4,
  units = "in",
  dpi = 300
)

# LOAD MIDDLE MAP

plans <- read_rds(here("data-raw/middle/plans/plans_ms_com1_inc11_split0_cap1_pop0.2.rds"))

plans <- add_summary_stats(
  plans, map, 
  current = map$middle_current, 
  schools = schools_idx, 
  commute_times = commute_times, 
  capacity = schools_capacity,
  level = "middle"
)

middle_comp <- plot(plans, comp_polsby, geom = "boxplot") + 
  labs(x = "Ordered Attendance Area", y = "Polsby-Popper") + 
  theme_bw()
ggsave(
  filename = here("figures/middle_comp.png"),
  plot = middle_comp,
  width = 10,
  height = 4,
  units = "in",
  dpi = 300
)

middle_max_commute <- plot(plans, max_commute, geom = "boxplot") + 
  labs(x = "Ordered Attendance Area", y = "Maximum Commute (min)") + 
  theme_bw()
ggsave(
  filename = here("figures/middle_max_commute.png"),
  plot = middle_max_commute,
  width = 10,
  height = 4,
  units = "in",
  dpi = 300
)

middle_polsby <- plans %>%
  ggplot(aes(x = comp_polsby, y = max_commute)) +
  geom_point(size=1) +
  labs(
    x = "Compactness: Polsby-Popper",
    y = "Maximum Commute (min)"
  ) +
  theme_bw()
ggsave(
  filename = here("figures/middle_polsby.png"),
  plot = middle_polsby,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

middle_edge <- plans %>%
  ggplot(aes(x = comp_edge, y = max_commute)) +
  geom_point(size=1) +
  labs(
    x = "Compactness: Fraction of Edges Kept",
    y = "Maximum Commute (min)"
  ) +
  theme_bw()
ggsave(
  filename = here("figures/middle_edge.png"),
  plot = middle_edge,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

middle_commute_heatmap <- projected_average_heatmap(
  plans, 
  map, 
  schools_idx, 
  commute_times, 
  "middle"
)
ggsave(
  filename = here("figures/middle_commute_heatmap.png"),
  plot = middle_commute_heatmap,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

middle_commute_current <- current_commute_heatmap(
  plans, 
  map, 
  schools_idx, 
  commute_times, 
  "middle"
)
ggsave(
  filename = here("figures/middle_commute_current.png"),
  plot = middle_commute_current,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

middle_cap_heatmap <- capacity_improvement_heatmap(
  plans,
  map,
  schools_idx,
  schools_capacity,
  "middle"
)
ggsave(
  filename = here("figures/middle_cap_heatmap.png"),
  plot = middle_cap_heatmap,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

middle_split_feeder_heatmap <- split_feeder_heatmap(
  plans %>% subset_sampled(),
  map, "middle"
)
ggsave(
  filename = here("figures/middle_split_feeder_heatmap.png"),
  plot = middle_split_feeder_heatmap,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

middle_split_feeder_current <- split_feeder_current(
  map, "middle"
)
ggsave(
  filename = here("figures/middle_split_feeder_current.png"),
  plot = middle_split_feeder_current,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

middle_split_feeder_proposed <- split_feeder_proposed(
  map, "middle"
)
ggsave(
  filename = here("figures/middle_split_feeder_proposed.png"),
  plot = middle_split_feeder_proposed,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

plans_sb <- read_rds(here("data-raw/middle/plans/plans_ms_sb_pop0.2.rds"))

middle_commute_boxplots <- comparison_boxplots(
  plans %>% subset_sampled(), 
  plans_sb %>% subset_sampled(), 
  map, schools_idx, commute_times, "middle"
)
ggsave(
  filename = here("figures/middle_commute_boxplots.png"),
  plot = middle_commute_boxplots,
  width = 10,
  height = 4,
  units = "in",
  dpi = 300
)

middle_cap_boxplots <- capacity_boxplots(
  plans %>% subset_sampled(),
  plans_sb %>% subset_sampled(),
  map, schools_idx, schools_capacity, "middle"
)
ggsave(
  filename = here("figures/middle_cap_boxplots.png"),
  plot = middle_cap_boxplots,
  width = 10,
  height = 4,
  units = "in",
  dpi = 300
)

# LOAD HIGH MAP

plans <- read_rds(here("data-raw/high/plans/plans_hs_com0_inc15_split4_cap0_pop0.25.rds"))

plans <- add_summary_stats(
  plans, map, 
  current = map$high_current, 
  schools = schools_idx, 
  commute_times = commute_times, 
  capacity = schools_capacity,
  level = "high"
)

high_comp <- plot(plans, comp_polsby, geom = "boxplot") + 
  labs(x = "Ordered Attendance Area", y = "Polsby-Popper") + 
  theme_bw()
ggsave(
  filename = here("figures/high_comp.png"),
  plot = high_comp,
  width = 10,
  height = 4,
  units = "in",
  dpi = 300
)

high_max_commute <- plot(plans, max_commute, geom = "boxplot") + 
  labs(x = "Ordered Attendance Area", y = "Maximum Commute (min)") + 
  theme_bw()
ggsave(
  filename = here("figures/high_max_commute.png"),
  plot = high_max_commute,
  width = 10,
  height = 4,
  units = "in",
  dpi = 300
)

high_polsby <- plans %>%
  ggplot(aes(x = comp_polsby, y = max_commute)) +
  geom_point(size=1) +
  labs(
    x = "Compactness: Polsby-Popper",
    y = "Maximum Commute (min)"
  ) +
  theme_bw()
ggsave(
  filename = here("figures/high_polsby.png"),
  plot = high_polsby,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

high_edge <- plans %>%
  ggplot(aes(x = comp_edge, y = max_commute)) +
  geom_point(size=1) +
  labs(
    x = "Compactness: Fraction of Edges Kept",
    y = "Maximum Commute (min)"
  ) +
  theme_bw()
ggsave(
  filename = here("figures/high_edge.png"),
  plot = high_edge,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

high_commute_heatmap <- projected_average_heatmap(
  plans, 
  map, 
  schools_idx, 
  commute_times, 
  "high"
)
ggsave(
  filename = here("figures/high_commute_heatmap.png"),
  plot = high_commute_heatmap,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

high_commute_current <- current_commute_heatmap(
  plans, 
  map, 
  schools_idx, 
  commute_times, 
  "high"
)
ggsave(
  filename = here("figures/high_commute_current.png"),
  plot = high_commute_current,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

high_cap_heatmap <- capacity_improvement_heatmap(
  plans,
  map,
  schools_idx,
  schools_capacity,
  "high"
)
ggsave(
  filename = here("figures/high_cap_heatmap.png"),
  plot = high_cap_heatmap,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

high_split_feeder_heatmap <- split_feeder_heatmap(
  plans %>% subset_sampled(),
  map, "high"
)
ggsave(
  filename = here("figures/high_split_feeder_heatmap.png"),
  plot = high_split_feeder_heatmap,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

high_split_feeder_current <- split_feeder_current(
  map, "high"
)
ggsave(
  filename = here("figures/high_split_feeder_current.png"),
  plot = high_split_feeder_current,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

high_split_feeder_proposed <- split_feeder_proposed(
  map, "high"
)
ggsave(
  filename = here("figures/high_split_feeder_proposed.png"),
  plot = high_split_feeder_proposed,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

plans_sb <- read_rds(here("data-raw/high/plans/plans_hs_sb_pop0.25.rds"))

high_commute_boxplots <- comparison_boxplots(
  plans %>% subset_sampled(), 
  plans_sb %>% subset_sampled(), 
  map, schools_idx, commute_times, "high"
)
ggsave(
  filename = here("figures/high_commute_boxplots.png"),
  plot = high_commute_boxplots,
  width = 10,
  height = 4,
  units = "in",
  dpi = 300
)

high_cap_boxplots <- capacity_boxplots(
  plans %>% subset_sampled(),
  plans_sb %>% subset_sampled(),
  map, schools_idx, schools_capacity, "high"
)
ggsave(
  filename = here("figures/high_cap_boxplots.png"),
  plot = high_cap_boxplots,
  width = 10,
  height = 4,
  units = "in",
  dpi = 300
)
