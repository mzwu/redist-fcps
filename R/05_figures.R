region_map <- map %>%
  ggplot(aes(fill = factor(region))) +
  geom_sf() +
  labs(title = "Pyramid Regions") +
  scale_fill_viridis_d(name = "Region") +
  theme_bw()
ggsave(
  filename = here("figures/region_map.png"),
  plot = region_map,
  width = 6,
  height = 8,
  units = "in",
  dpi = 300
)

# LOAD ELEMENTARY MAP

plans <- read_rds(here("data-raw/elem/plans/plans_es_mcmc_com1_inc12_cap10_pop0.66.rds"))

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
  height = 8,
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
  width = 9,
  height = 6,
  units = "in",
  dpi = 300
)

# LOAD MIDDLE MAP

plans <- read_rds(here("data-raw/middle/plans/plans_ms_com1_inc11_split0_cap1_pop0.2.rds"))

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
  height = 8,
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
  height = 8,
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
  width = 9,
  height = 6,
  units = "in",
  dpi = 300
)

# LOAD HIGH MAP

plans <- read_rds(here("data-raw/high/plans/plans_hs_com0_inc15_split4_cap0_pop0.25.rds"))

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
  height = 8,
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
  height = 8,
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
  width = 9,
  height = 6,
  units = "in",
  dpi = 300
)
