
###############################
# Functions for VE estimation
###############################

## unmatched  model (unstratified)
regress_unmatch <- function(df.ve,
                            adjusted = "no",
                            confounders){
  
  # get n of cases and controls
  n.cases <- df.ve %>% filter(case_control == 1) %>% nrow()
  n.controls <- df.ve %>% filter(case_control == 0) %>% nrow()
  
  # effective n used by model, after removing missing values
  if(adjusted == "no"){
    n.cases.eff <- n.cases   
    n.controls.eff <- n.controls
    formula <- as.formula("case_control ~ rsv_mab")
  }
  if(adjusted == "yes"){
    n.cases.eff <-  df.ve[, c("case_control", confounders)] %>% 
      filter(across(everything(), ~ !is.na(.))) %>% filter(case_control == 1) %>% nrow()
    n.controls.eff <- df.ve[, c("case_control", confounders)] %>% 
      filter(across(everything(), ~ !is.na(.))) %>% filter(case_control == 0) %>% nrow()
    
    formula <- as.formula(paste(c("case_control ~ rsv_mab", confounders), collapse = " + ")) 
  }
  
  model <- glm(formula, data = df.ve, family = "binomial") 
  
  # get estimates
  ve.median <- 1 - exp(model$coefficients[2])
  ve.ub <- 1 - exp(confint(model)[2,][1])
  ve.lb <- 1 - exp(confint(model)[2,][2])
  
  # make a table
  ve.results <- data.frame(
    n_overall = n.cases + n.controls,
    n_overall_eff = n.cases.eff + n.controls.eff,
    n_cases = n.cases,
    n_cases_eff = n.cases.eff,
    n_controls = n.controls,
    n_controls_eff = n.controls.eff,
    ve_median = ve.median,
    ve_lb = ve.lb,
    ve_ub = ve.ub
  ) 
  
  row.names(ve.results) <- NULL
  
  return(ve.results)
}



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













