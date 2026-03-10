region_map <- map %>%
  ggplot(aes(fill = factor(region))) +
  geom_sf() +
  labs(
    fill = "Region"
  ) +
  scale_fill_brewer(palette = "Accent") +
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

elem_current_map <- redist.plot.plans(
  plans,
  "elem_current",
  map
) +
  theme(plot.title = element_blank())
ggsave(
  filename = here("figures/elem_current_map.png"),
  plot = elem_current_map,
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)

elem_proposed_map <- redist.plot.plans(
  plans,
  "elem_scenario5",
  map
) +
  theme(plot.title = element_blank())
ggsave(
  filename = here("figures/elem_proposed_map.png"),
  plot = elem_proposed_map,
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)

plans <- add_summary_stats(
  plans, map, 
  current = map$elem_current, 
  schools = schools_idx, 
  commute_times = commute_times, 
  capacity = schools_capacity,
  level = "elem"
)

plans <- plans %>%
  mutate(draw = recode(draw,
                       elem_scenario2 = "Scenario 2",
                       elem_scenario3 = "Scenario 3",
                       elem_scenario4 = "Scenario 4",
                       elem_scenario5 = "Scenario 5",
                       elem_current = "Current"
  ))

elem_frac_edges <- hist(plans, comp_edge, bins = 40) + 
  labs(x = "Fraction of Edges Kept", y = "Fraction of Plans") + 
  theme_bw()
ggsave(
  filename = here("figures/elem_frac_edges.png"),
  plot = elem_frac_edges,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
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
  filter(draw %in% sample(unique(draw), 10)) %>%
  ggplot(aes(x = comp_polsby, y = max_commute)) +
  geom_point(size=1) +
  geom_smooth(method = "loess", se = TRUE) +
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

middle_current_map <- redist.plot.plans(
  plans,
  "middle_current",
  map
) +
  theme(plot.title = element_blank())
ggsave(
  filename = here("figures/middle_current_map.png"),
  plot = middle_current_map,
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)

middle_proposed_map <- redist.plot.plans(
  plans,
  "middle_scenario5",
  map
) +
  theme(plot.title = element_blank())
ggsave(
  filename = here("figures/middle_proposed_map.png"),
  plot = middle_proposed_map,
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)

plans <- add_summary_stats(
  plans, map, 
  current = map$middle_current, 
  schools = schools_idx, 
  commute_times = commute_times, 
  capacity = schools_capacity,
  level = "middle"
)

plans <- plans %>%
  mutate(draw = recode(draw,
                       middle_scenario2 = "Scenario 2",
                       middle_scenario3 = "Scenario 3",
                       middle_scenario4 = "Scenario 4",
                       middle_scenario5 = "Scenario 5",
                       middle_current = "Current"
  ))

middle_split <- hist(plans, elem_split_feeders) + 
  labs(x = "Number of Split Feeders", 
       y = "Fraction of Plans") + 
  theme_bw()
ggsave(
  filename = here("figures/middle_split.png"),
  plot = middle_split,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)

middle_frac_edges <- hist(plans, comp_edge, bins = 40) + 
  labs(x = "Fraction of Edges Kept", y = "Fraction of Plans") + 
  theme_bw()
ggsave(
  filename = here("figures/middle_frac_edges.png"),
  plot = middle_frac_edges,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
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
  filter(draw %in% sample(unique(draw), 10)) %>%
  ggplot(aes(x = comp_polsby, y = max_commute)) +
  geom_point(size=1) +
  geom_smooth(method = "loess", se = TRUE) +
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

plans_sb <- read_rds(here("data-raw/middle/plans_ms_sb_pop0.2.rds"))

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

high_current_map <- redist.plot.plans(
  plans,
  "high_current",
  map
) +
  theme(plot.title = element_blank())
ggsave(
  filename = here("figures/high_current_map.png"),
  plot = high_current_map,
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)

high_proposed_map <- redist.plot.plans(
  plans,
  "high_scenario5",
  map
) +
  theme(plot.title = element_blank())
ggsave(
  filename = here("figures/high_proposed_map.png"),
  plot = high_proposed_map,
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)

plans <- add_summary_stats(
  plans, map, 
  current = map$high_current, 
  schools = schools_idx, 
  commute_times = commute_times, 
  capacity = schools_capacity,
  level = "high"
)

plans <- plans %>%
  mutate(draw = recode(draw,
                       high_scenario2 = "Scenario 2",
                       high_scenario3 = "Scenario 3",
                       high_scenario4 = "Scenario 4",
                       high_scenario5 = "Scenario 5",
                       high_current = "Current"
  ))

high_split <- hist(plans, middle_split_feeders) + 
  labs(x = "Number of Split Feeders", 
       y = "Fraction of Plans") + 
  theme_bw()
ggsave(
  filename = here("figures/high_split.png"),
  plot = high_split,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)

high_frac_edges <- hist(plans, comp_edge, bins = 40) + 
  labs(x = "Fraction of Edges Kept", y = "Fraction of Plans") + 
  theme_bw()
ggsave(
  filename = here("figures/high_frac_edges.png"),
  plot = high_frac_edges,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
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
  filter(draw %in% sample(unique(draw), 10)) %>%
  ggplot(aes(x = comp_polsby, y = max_commute)) +
  geom_point(size=1) +
  geom_smooth(method = "loess", se = TRUE) +
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

plans_sb <- read_rds(here("data-raw/high/plans_hs_sb_pop0.25.rds"))

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


### OTHER ANALYSIS
x <- plans %>%
  subset_sampled() %>%
  group_by(district) %>%
  summarise(
    min = min(max_commute, na.rm = TRUE),
    Q1 = quantile(max_commute, 0.25, na.rm = TRUE),
    median = median(max_commute, na.rm = TRUE),
    Q3 = quantile(max_commute, 0.75, na.rm = TRUE),
    max = max(max_commute, na.rm = TRUE),
    mean = mean(max_commute, na.rm = TRUE),
    sd = sd(max_commute, na.rm = TRUE),
    n = n()
  )
plans %>% filter(draw == "high_current") %>% pull(max_commute) %>% max()
max(x$median)
x <- plans %>%
  subset_sampled() %>%
  group_by(district) %>%
  summarise(
    min = min(comp_polsby, na.rm = TRUE),
    Q1 = quantile(comp_polsby, 0.25, na.rm = TRUE),
    median = median(comp_polsby, na.rm = TRUE),
    Q3 = quantile(comp_polsby, 0.75, na.rm = TRUE),
    max = max(comp_polsby, na.rm = TRUE),
    mean = mean(comp_polsby, na.rm = TRUE),
    sd = sd(comp_polsby, na.rm = TRUE),
    n = n()
  )
plans %>% filter(draw == "high_current") %>% pull(comp_polsby) %>% max()
max(x$median)

commute_box_stats <- blocks_long %>%
  group_by(region, scenario) %>%
  summarise(
    min = min(commute, na.rm = TRUE),
    Q1 = quantile(commute, 0.25, na.rm = TRUE),
    median = median(commute, na.rm = TRUE),
    Q3 = quantile(commute, 0.75, na.rm = TRUE),
    max = max(commute, na.rm = TRUE),
    mean = mean(commute, na.rm = TRUE),
    sd = sd(commute, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

cap_box_stats <- blocks_long %>%
  group_by(region, scenario) %>%
  summarise(
    min = min(capacity, na.rm = TRUE),
    Q1 = quantile(capacity, 0.25, na.rm = TRUE),
    median = median(capacity, na.rm = TRUE),
    Q3 = quantile(capacity, 0.75, na.rm = TRUE),
    max = max(capacity, na.rm = TRUE),
    mean = mean(capacity, na.rm = TRUE),
    sd = sd(capacity, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

blocks %>%
  ggplot() +
  geom_sf(aes(fill = avg_split_feeder)) +
  scale_fill_viridis_c("Average Simulated \nSplit Feeder") +
  theme_void() +
  geom_sf_text(
    data = subset(blocks, avg_split_feeder > 0.15),
    aes(label = avg_split_feeder)
  )
