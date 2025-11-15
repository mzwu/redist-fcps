# make constraint object
constr <- redist_constr(map) %>%
  add_constr_phase_commute(
    strength = 1,
    current = map$middle25,
    schools = schools_idx,
    commute_times = commute_times
  ) %>%
  add_constr_incumbency(
    strength = 10,
    incumbents = schools_idx
  ) %>%
  add_constr_custom(strength = 1, function(plan, distr){
    # custom constraint for capacity utilization
    # get vector of district assignments for this plan
    # get row numbers of redist_map that are in this district
    rows_in_district <- which(plan == distr)
    
    # get the index of schools_idx of the element that is in the district map rows
    idx_in_schools <- which(schools_idx %in% rows_in_district)[1]
    
    # get school name
    school_name <- ffx_ms$OBJECTID[idx_in_schools]
    
    # calculate the proportion of total population that this school would theoretically have
    target_ratio <- ffx_middle$prop_capacity[ffx_middle$object_id_school == school_name][1L]
    if (is.na(target_ratio)) return(1000)
    actual_ratio <- sum(map$pop[which(plan == distr)]) / sum(map$pop)
    error <- 0.01
    
    if (abs(target_ratio - actual_ratio) <= error) {
      return(0)
    } else {
      return(1000)
    }
  }
  )

plans <- redist_smc(map, nsims = 2500,
                    counties = elem25,
                    #constr = constr
                    )

plans <- match_numbers(plans, "middle25")

# keep only plans where each school is in a distinct district
plans <- drop_duplicate_schools(plans, schools_idx)

write_rds(plans, here("data-raw/middle25/plans_ms_noconstr.rds"), compress = "gz")
