# save map adjacency matrix
n <- nrow(map)
adj_mat <- matrix(0L, nrow = n, ncol = n)
for (u in seq_len(n)) {
  neighbors <- as.integer(map$adj[[u]])
  neighbors <- neighbors[!is.na(neighbors)]
  adj_mat[u, neighbors + 1] <- 1L
}
adj_mat <- pmax(adj_mat, t(adj_mat))
write.table(adj_mat, here("data/adjacency_matrix.csv"), sep = ",", row.names = FALSE, col.names = FALSE)

# save populations for each block
pop <- map$pop
write.table(pop, here("data/blocks_pop.csv"), sep = ",", row.names = FALSE, col.names = FALSE)

# save commute times for each school level
commute_es <- read_rds(here("data-raw/elem/commute_times_es.rds"))
write.table(commute_es, here("data/commute_es.csv"), sep = ",", row.names = FALSE, col.names = FALSE)

commute_ms <- read_rds(here("data-raw/middle/commute_times_ms.rds"))
write.table(commute_ms, here("data/commute_ms.csv"), sep = ",", row.names = FALSE, col.names = FALSE)

commute_hs <- read_rds(here("data-raw/high/commute_times_hs.rds"))
write.table(commute_hs, here("data/commute_hs.csv"), sep = ",", row.names = FALSE, col.names = FALSE)

# save school indices for each school level
schools_es <- get_schools_idx(ffx_es, map)
write.table(schools_es, here("data/schools_es.csv"), sep = ",", row.names = FALSE, col.names = FALSE)

schools_ms <- get_schools_idx(ffx_ms, map)
write.table(schools_ms, here("data/schools_ms.csv"), sep = ",", row.names = FALSE, col.names = FALSE)

schools_hs <- get_schools_idx(ffx_hs, map)
write.table(schools_hs, here("data/schools_hs.csv"), sep = ",", row.names = FALSE, col.names = FALSE)

# save capacities for each school level
capacity_es <- read_csv("data/school_capacities.csv") %>%
  filter(type == "ES") %>%
  mutate(utilization = membership / capacity,
         prop_capacity = capacity / sum(capacity)) %>%
  arrange(object_id_school) %>%
  mutate(elem25 = vctrs::vec_group_id(object_id_school)) %>%
  select(capacity, elem25) %>%
  arrange(elem25) %>%
  pull(capacity)
write.table(capacity_es, here("data/capacity_es.csv"), sep = ",", row.names = FALSE, col.names = FALSE)

capacity_ms <- read_csv("data/school_capacities.csv") %>%
  filter(type == "MS") %>%
  mutate(utilization = membership / capacity,
         prop_capacity = capacity / sum(capacity)) %>%
  arrange(object_id_school) %>%
  mutate(middle25 = vctrs::vec_group_id(object_id_school)) %>%
  select(capacity, middle25) %>%
  arrange(middle25) %>%
  pull(capacity)
write.table(capacity_ms, here("data/capacity_ms.csv"), sep = ",", row.names = FALSE, col.names = FALSE)

capacity_hs <- read_csv("data/school_capacities.csv") %>%
  filter(type == "HS") %>%
  mutate(utilization = membership / capacity,
         prop_capacity = capacity / sum(capacity)) %>%
  arrange(object_id_school) %>%
  mutate(high25 = vctrs::vec_group_id(object_id_school)) %>%
  select(capacity, high25) %>%
  arrange(high25) %>%
  pull(capacity)
write.table(capacity_hs, here("data/capacity_hs.csv"), sep = ",", row.names = FALSE, col.names = FALSE)

# save lower level plans
# make sure to load middle map
plan_es <- as.integer(map$elem_starter1)
y <- model.matrix(~ factor(plan_es) - 1)
write.table(y, here("data/lower_es_1.csv"), sep = ",", row.names = FALSE, col.names = FALSE)
plan_es <- as.integer(map$elem_starter2)
y <- model.matrix(~ factor(plan_es) - 1)
write.table(y, here("data/lower_es_2.csv"), sep = ",", row.names = FALSE, col.names = FALSE)
plan_es <- as.integer(map$elem_starter3)
y <- model.matrix(~ factor(plan_es) - 1)
write.table(y, here("data/lower_es_3.csv"), sep = ",", row.names = FALSE, col.names = FALSE)

# make sure to load high map
plan_ms <- as.integer(map$middle_starter1)
y <- model.matrix(~ factor(plan_ms) - 1)
write.table(y, here("data/lower_ms_1.csv"), sep = ",", row.names = FALSE, col.names = FALSE)
plan_ms <- as.integer(map$middle_starter2)
y <- model.matrix(~ factor(plan_ms) - 1)
write.table(y, here("data/lower_ms_2.csv"), sep = ",", row.names = FALSE, col.names = FALSE)
plan_ms <- as.integer(map$middle_starter3)
y <- model.matrix(~ factor(plan_ms) - 1)
write.table(y, here("data/lower_ms_3.csv"), sep = ",", row.names = FALSE, col.names = FALSE)