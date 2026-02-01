map %>%
  ggplot(aes(fill = factor(region))) +
  geom_sf() +
  scale_fill_viridis_d(name = "Region") +
  theme_bw()
