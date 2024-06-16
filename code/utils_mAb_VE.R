
###############################
# some processing of dataset
###############################

process_df <- function(df.cc){
  
  # add/recode some variables 
  
  # month when tested and age when tested 
  df <- df %>% 
    mutate(month_when_tested = as.character(month(collection_date, label = T))) %>% 
    mutate(month_when_tested = ifelse(month_when_tested == "Sep", "Oct", month_when_tested)) %>%
    # age when tested (6m interval)
    mutate(age_at_test_in_months_cat = case_when(
      age_at_test_in_months < 6 ~ "under 6m",
      age_at_test_in_months >= 6 & age_at_test_in_months < 12 ~ "6-12m",
      age_at_test_in_months >= 12 ~ "above 1 yo" 
    )) %>%
    # age when tested (3m interval)
    mutate(age_at_test_in_months_cat_2 = case_when(
      age_at_test_in_months < 3 ~ "under 3m",
      age_at_test_in_months >= 3 & age_at_test_in_months < 6 ~ "3-6m",
      age_at_test_in_months >= 6 & age_at_test_in_months < 9 ~ "6-9m",
      age_at_test_in_months >= 9 & age_at_test_in_months < 12 ~ "9-12m",
      age_at_test_in_months >= 12 ~ "above 1 yo" 
    )) %>%
    mutate(risk_factor_atleastone = case_when(risk_factor_atleastone == 1 ~ "yes",
                                              risk_factor_atleastone == 0 ~ "no")) %>% 
    mutate(across(c(risk_factor_anemia, 
                    risk_factor_blood,
                    risk_factor_pulmonary,
                    risk_factor_liver,
                    risk_factor_cardiac,
                    risk_factor_asthma,
                    risk_factor_immunodeficiency,
                    risk_factor_down,
                    risk_factor_wheezing,
                    risk_factor_environmental
                    #risk_factor_small_for_gestage # this has values of 1 0 NA
    ), ~ ifelse(is.na(.), "no", "yes"))) %>%
    mutate(age_at_test_in_months_cat = factor(age_at_test_in_months_cat,
                                              levels = c("under 6m",
                                                         "6-12m",
                                                         "above 1 yo"))) %>%
    mutate(age_at_test_in_months_cat_2 = factor(age_at_test_in_months_cat_2,
                                                levels = c("under 3m",
                                                           "3-6m",
                                                           "6-9m",
                                                           "9-12m",
                                                           "above 1 yo"))) %>% 
    mutate(race_ethnicity = factor(race_ethnicity,
                                   levels = c("Hispanic",
                                              "White non-Hispanic",
                                              "Black non-Hispanic",
                                              "Other non-Hispanic",
                                              "unknown")))
  
  # birth weight in grams (originally ounce)
  df <- df %>%  mutate(birth_weight = Birth.Wgt * 28.3)
  
  # classify time since mab to testing 
  df <- df %>% 
    mutate(days_btw_mab_collection_cat = 
             case_when(days_btw_mab_collection < 0 & days_btw_mab_collection > -60 ~ "0-2 months", 
                       days_btw_mab_collection <= -60 & days_btw_mab_collection > -120 ~ "2-4 months",
                       days_btw_mab_collection <= -120 ~ "4 months +",
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat = factor(days_btw_mab_collection_cat,
                                                levels = c("no mAb",
                                                           "0-2 months",
                                                           "2-4 months",
                                                           "4 months +"))) %>%
    mutate(days_btw_mab_collection_cat_2 = 
             case_when(days_btw_mab_collection < 0 & days_btw_mab_collection > -90 ~ "0-3 months", 
                       days_btw_mab_collection <= -90 & days_btw_mab_collection > -190 ~ "3-6 months",
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat_2 = factor(days_btw_mab_collection_cat_2,
                                                  levels = c("no mAb",
                                                             "0-3 months",
                                                             "3-6 months")))
  
  
  # group month_when_tested into fewer groups
  df <- df %>% 
    mutate(month_when_tested_cat = case_when(
      collection_date >= as.Date("2023-9-30") & collection_date <= as.Date("2023-11-30") ~ "Oct-Nov",
      collection_date >= as.Date("2023-12-1") & collection_date <= as.Date("2024-1-31") ~ "Dec-Jan",
      collection_date >= as.Date("2024-2-1") & collection_date <= as.Date("2024-3-31") ~ "Feb-Mar",
      collection_date >= as.Date("2024-4-1")  ~ "April and after"
    )) %>% 
    mutate(month_when_tested_cat = factor(month_when_tested_cat,
                                          levels = c("Oct-Nov", "Dec-Jan", "Feb-Mar","April and after")))
  
  
  # dosage 
  df <- df %>% 
    mutate(rsv_mab_dose = case_when(rsv_mab_detailed == "100mg before collection" ~ "100mg",
                                    rsv_mab_detailed == "50mg before collection" ~ "50mg",
                                    TRUE ~ "no mAb"))
}


###############################
# Functions for VE estimation
###############################

## unmatched  model (unstratified)
regress_unmatch <- function(df.ve,
                            confounders){
  
  # get n of cases and controls for unadjusted analysis
  n.cases.mab <- df.ve %>% filter(case_control == 1 & rsv_mab == 1) %>% nrow()
  n.cases.nomab <- df.ve %>% filter(case_control == 1 & rsv_mab == 0) %>% nrow()
  n.controls.mab <- df.ve %>% filter(case_control == 0 & rsv_mab == 1) %>% nrow()
  n.controls.nomab <- df.ve %>% filter(case_control == 0 & rsv_mab == 0) %>% nrow()

  # get n of cases and controls for adjusted analysis
  n.cases.mab.adj <-  df.ve[, c("case_control", "rsv_mab", confounders)] %>% 
    drop_na() %>% filter(case_control == 1 & rsv_mab == 1) %>% nrow()
  n.cases.nomab.adj <-  df.ve[, c("case_control",  "rsv_mab", confounders)] %>% 
    drop_na() %>% filter(case_control == 1 & rsv_mab == 0) %>% nrow()
  n.controls.mab.adj <- df.ve[, c("case_control",  "rsv_mab", confounders)] %>% 
    drop_na() %>% filter(case_control == 0 & rsv_mab == 1) %>% nrow()
  n.controls.nomab.adj <- df.ve[, c("case_control",  "rsv_mab", confounders)] %>% 
    drop_na() %>% filter(case_control == 0 & rsv_mab == 0) %>% nrow()
  
  
  # unadjusted
  formula <- as.formula("case_control ~ rsv_mab") 
  model <- glm(formula, data = df.ve, family = "binomial") 
  ve.median.unadj <- 1 - exp(model$coefficients[2])
  ve.ub.unadj <- 1 - exp(confint(model)[2,][1])
  ve.lb.unadj <- 1 - exp(confint(model)[2,][2])

  # adjusted
  formula <- as.formula(paste(c("case_control ~ rsv_mab", confounders), collapse = " + ")) 
  model <- glm(formula, data = df.ve, family = "binomial") 
  ve.median.adj <- 1 - exp(model$coefficients[2])
  ve.ub.adj <- 1 - exp(confint(model)[2,][1])
  ve.lb.adj <- 1 - exp(confint(model)[2,][2])
  
  # make a table
  ve.results <- data.frame(
    n_cases_mab = paste0(n.cases.mab, " (", n.cases.mab.adj, ")"),
    n_cases_nomab = paste0(n.cases.nomab, " (", n.cases.nomab.adj, ")"),
    n_controls_mab = paste0(n.controls.mab, " (", n.controls.mab.adj, ")"),
    n_controls_nomab = paste0(n.controls.nomab, " (", n.controls.nomab.adj, ")"),
    ve_unadj = paste0(round(ve.median.unadj*100,1), " (", round(ve.lb.unadj*100,1), "-", round(ve.ub.unadj*100,1), ")"),
    ve_adj = paste0(round(ve.median.adj*100,1), " (", round(ve.lb.adj*100,1), "-", round(ve.ub.adj*100,1), ")")
  ) 
  
  # row.names(ve.results) <- NULL
  
  return(ve.results)
}



### Function to convert VE to plots 





########################################################
# unused functions 
########################################################


# filter out subgroup and calculate VE of each subgroup
regress_unmatch_stratified <- function(df.ve,
                                       stratify_by,
                                       confounders){
  
  df.glm <- df.ve %>% filter(!is.na(get(stratify_by)))
  confounders <- confounders[confounders %in% stratify_by == F]
  
  # remove subgroups with too few records
  if(stratify_by == "risk_factor_gestage"){
    df.glm <- df.glm %>% filter(risk_factor_gestage != "post term")
  }
  
  groups <- unique(df.glm[ , stratify_by])
  # subset each subgroup
  for(i in 1:length(groups)){
    
    # filter out this subgroup
    df.glm <- df.ve %>% filter(get(stratify_by) == groups[i])
    
    
    # run unadjusted glm
    model.unadj <- glm(case_control ~ rsv_mab, data = df.glm, family = "binomial")
    
    # get unadjuested estimates for this group
    ve.median.unadj <- 1 - exp(model.unadj$coefficients[2])
    ve.ub.unadj <- 1 - exp(confint(model.unadj)[2,][1])
    ve.lb.unadj <- 1 - exp(confint(model.unadj)[2,][2])
    
    # run adjusted glm
    if(groups[i] == "preterm"){confounders <- confounders[confounders %in% "risk_factor_atleastone" == F]}
    formula <- as.formula(paste(c("case_control ~ rsv_mab", 
                                  confounders), collapse = " + "))
    model.adj <- glm(formula, data = df.glm, family = "binomial")
    
    # get unadjuested estimates for this group
    ve.median.adj <- 1 - exp(model.adj$coefficients[2])
    ve.ub.adj <- 1 - exp(confint(model.adj)[,1][2])
    ve.lb.adj <- 1 - exp(confint(model.adj)[,2][2])
    
    # get the number of case and controls
    n.cases <- df.glm %>% filter(case_control ==1 ) %>% nrow()
    n.controls <- df.glm %>% filter(case_control == 0) %>% nrow 
    n.cases.eff <- df.glm[, c("case_control", confounders)] %>% filter(case_control == 1) %>%
      filter(across(everything(), ~ !is.na(.))) %>% nrow()
    n.controls.eff <- df.glm[, c("case_control", confounders)] %>% filter(case_control == 0) %>%
      filter(across(everything(), ~ !is.na(.))) %>% nrow()
    
    # combine results 
    df.results.temp <- data.frame(
      n_overall = n.cases + n.controls,
      n_overall_eff = n.cases.eff + n.controls.eff,
      n_cases = n.cases,
      n_cases_eff = n.cases.eff,
      n_controls = n.controls, 
      n_controls_eff = n.controls.eff,
      ve_median_unadj = ve.median.unadj,
      ve_lb_unadj = ve.lb.unadj,
      ve_ub_unadj = ve.ub.unadj,
      ve_median_adj = ve.median.adj,
      ve_lb_adj = ve.lb.adj,
      ve_ub_adj = ve.ub.adj
    ) %>%
      mutate(stratify_by = stratify_by,
             strata = groups[i])
    
    if(i == 1){df.results <- df.results.temp}
    if(i != 1){df.results <- rbind(df.results, df.results.temp)}
    
  }
  
  row.names(df.results) <- NULL
  return(df.results)
  
}



regress_match_stratified <- function(df.ve, 
                                     stratify_by,
                                     confounders){
  
  df.glm <- df.ve %>% filter(!is.na(get(stratify_by)))
  confounders <- confounders[confounders %in% stratify_by == F]
  
  # remove subgroups with too few records
  if(stratify_by == "risk_factor_gestage"){
    df.glm <- df.glm %>% filter(risk_factor_gestage != "post term")
  }
  
  groups <- unique(df.glm[ , stratify_by])
  # subset each subgroup
  for(i in 1:length(groups)){
    
    # filter out this subgroup
    df.glm <- df.ve %>% filter(get(stratify_by) == groups[i])
    
    
    # run unadjusted glm
    model.unadj <- glm(case_control ~ rsv_mab + strata(id_case), data = df.glm, family = "binomial")
    
    # get unadjuested estimates for this group
    ve.median.unadj <- 1 - exp(model.unadj$coefficients[2])
    ve.ub.unadj <- 1 - exp(confint(model.unadj)[2,][1])
    ve.lb.unadj <- 1 - exp(confint(model.unadj)[2,][2])
    
    
    
    # run adjusted glm
    if(groups[i] == "preterm"){confounders <- confounders[confounders %in% "risk_factor_atleastone" == F]}
    formula <- as.formula(paste(c("case_control ~ rsv_mab + strata(id_case)", 
                                  confounders), collapse = " + "))
    model.adj <- glm(formula, data = df.glm, family = "binomial")
    
    # get unadjuested estimates for this group
    ve.median.adj <- 1 - exp(model.adj$coefficients[2])
    ve.ub.adj <- 1 - exp(confint(model.adj)[,1][2])
    ve.lb.adj <- 1 - exp(confint(model.adj)[,2][2])
    
    # get the number of case and controls
    n.cases <- df.glm %>% filter(case_control ==1 ) %>% nrow()
    n.controls <- df.glm %>% filter(case_control == 0) %>% nrow 
    n.cases.eff <- df.glm[, c("case_control", confounders)] %>% filter(case_control == 1) %>%
      filter(across(everything(), ~ !is.na(.))) %>% nrow()
    n.controls.eff <- df.glm[, c("case_control", confounders)] %>% filter(case_control == 0) %>%
      filter(across(everything(), ~ !is.na(.))) %>% nrow()
    
    # combine results 
    df.results.temp <- data.frame(
      n_overall = n.cases + n.controls,
      n_overall_eff = n.cases.eff + n.controls.eff,
      n_cases = n.cases,
      n_cases_eff = n.cases.eff,
      n_controls = n.controls, 
      n_controls_eff = n.controls.eff,
      ve_median_unadj = ve.median.unadj,
      ve_lb_unadj = ve.lb.unadj,
      ve_ub_unadj = ve.ub.unadj,
      ve_median_adj = ve.median.adj,
      ve_lb_adj = ve.lb.adj,
      ve_ub_adj = ve.ub.adj
    ) %>%
      mutate(stratify_by = stratify_by,
             strata = groups[i])
    
    if(i == 1){df.results <- df.results.temp}
    if(i != 1){df.results <- rbind(df.results, df.results.temp)}
    
  }
  
  row.names(df.results) <- NULL
  return(df.results)
  
}













