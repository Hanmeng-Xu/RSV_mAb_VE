
model_string_waning_1monthinterval <- "
  # classify time since vax to testing into three groups: 0-1,1-2,2-3,3-4,4+ months
  
model{
  
  # go through every record in the dataset
  # here N_records is the totol records in the dataset
  for(j in 1:N_records){
    
    # likelihood of each record
    case_status[j] ~ dbin(pi[j], 1)
    
    # adjusted logistic model at individual level
    logit(pi[j]) <- 
      int + 
      vax_time_group1[j] * beta1 +  # vax_time_group1[j] = 1 if this individual took mAb 0-1 months ago, otherwise vax_time_group1[j] = 0
      vax_time_group2[j] * beta2 +  # vax_time_group2[j] = 1 if this individual took mAb 1-2 months ago, otherwise vax_time_group2[j] = 0
      vax_time_group3[j] * beta3 +  # vax_time_group3[j] = 1 if this individual took mAb 2-3 months ago, otherwise vax_time_group3[j] = 0
      vax_time_group4[j] * beta4 +  # vax_time_group4[j] = 1 if this individual took mAb 3-4 months ago, otherwise vax_time_group4[j] = 0
      vax_time_group5[j] * beta5 +  # vax_time_group5[j] = 1 if this individual took mAb 4+ months ago, otherwise vax_time_group5[j] = 0
      cf_age_1[j] * beta_cf_age_1 + # dummy variables for categorical confounders
      cf_age_2[j] * beta_cf_age_2 +
      cf_age_3[j] * beta_cf_age_3 +
      cf_age_4[j] * beta_cf_age_4 +
      cf_age_5[j] * beta_cf_age_5 +
      cf_month_tested_1[j] * beta_cf_month_tested_1 +
      cf_month_tested_2[j] * beta_cf_month_tested_2 +
      cf_month_tested_3[j] * beta_cf_month_tested_3 +
      cf_month_tested_4[j] * beta_cf_month_tested_4 
      # +
      # cf_birth_weight[j] * beta_cf_birth_weight +
      # cf_gestage[j] * beta_cf_gestage
  
  }
  
  # auto regressive structure for VE coefficient
  # beta1 ~ dnorm(0, (1 - rho^2) * tau)
  # beta2 ~ dnorm(rho * beta1, tau)
  # beta3 ~ dnorm(rho * beta2, tau)
  # beta4 ~ dnorm(rho * beta3, tau)
  # beta5 ~ dnorm(rho * beta4, tau)
  # 
  # # increasing beta's
  # # Non-negative increments for ensuring order
  d[1] ~ dnorm(0, 0.0001)  # beta1 (uninformative)
  d[2] ~ dnorm(0, tau_d)T(0,)  # non-negative increases
  d[3] ~ dnorm(0, tau_d)T(0,)  # non-negative
  d[4] ~ dnorm(0, tau_d)T(0,)  # non-negative
  d[5] ~ dnorm(0, tau_d)T(0,)  # non-negative
  # 
  # Ensure monotonicity through cumulative sums
  beta1 <- d[1]
  beta2 <- beta1 + d[2]
  beta3 <- beta2 + d[3]
  beta4 <- beta3 + d[4]
  beta5 <- beta4 + d[5]
  
  
   tau_d ~ dgamma(0.01, 0.01) # uninformative prior for tau_d 
  # tau_d ~ dgamma(3,2) # informative prior for tau_d

  
  
  ## priors
  # for AR
  tau ~ dgamma(0.01, 0.01)
  rho ~ dunif(-1, 1)
  # how to structure rho to let VE can only decrease over time?
  
  # uninformative priors for coefficient of confounders 
  beta_cf_age_1 ~ dnorm(0, 1e-04)
  beta_cf_age_2 ~ dnorm(0, 1e-04)
  beta_cf_age_3 ~ dnorm(0, 1e-04)
  beta_cf_age_4 ~ dnorm(0, 1e-04)
  beta_cf_age_5 ~ dnorm(0, 1e-04)
  beta_cf_month_tested_1 ~ dnorm(0, 1e-04)
  beta_cf_month_tested_2 ~ dnorm(0, 1e-04)
  beta_cf_month_tested_3 ~ dnorm(0, 1e-04)
  beta_cf_month_tested_4 ~ dnorm(0, 1e-04)
  # beta_cf_birth_weight ~ dnorm(0, 1e-04)
  # beta_cf_gestage ~ dnorm(0, 1e-04)
  
  # intercept 
  int ~ dnorm(0, 1e-04)

} "


model_string_waning_biwkinterval <- "
  # classify time since vax to testing by biweek interval
  
model{
  
  # go through every record in the dataset
  # here N_records is the totol records in the dataset
  for(j in 1:N_records){
    
    # likelihood of each record
    case_status[j] ~ dbin(pi[j], 1)
    
    # adjusted logistic model at individual level
    logit(pi[j]) <- 
      int + 
      vax_time_group1[j] * beta1 +  
      vax_time_group2[j] * beta2 +  
      vax_time_group3[j] * beta3 + 
      vax_time_group4[j] * beta4 + 
      vax_time_group5[j] * beta5 +  
      vax_time_group6[j] * beta6 +  
      vax_time_group7[j] * beta7 +  
      vax_time_group8[j] * beta8 +  
      vax_time_group9[j] * beta9 +  
      cf_age_1[j] * beta_cf_age_1 + # dummy variables for categorical confounders
      cf_age_2[j] * beta_cf_age_2 +
      cf_age_3[j] * beta_cf_age_3 +
      cf_age_4[j] * beta_cf_age_4 +
      cf_age_5[j] * beta_cf_age_5 +
      cf_month_tested_1[j] * beta_cf_month_tested_1 +
      cf_month_tested_2[j] * beta_cf_month_tested_2 +
      cf_month_tested_3[j] * beta_cf_month_tested_3 +
      cf_month_tested_4[j] * beta_cf_month_tested_4 
      # +
      # cf_birth_weight[j] * beta_cf_birth_weight +
      # cf_gestage[j] * beta_cf_gestage
  
  }
  
  # auto regressive structure for VE coefficient
  # beta1 ~ dnorm(0, (1 - rho^2) * tau)
  # beta2 ~ dnorm(rho * beta1, tau)
  # beta3 ~ dnorm(rho * beta2, tau)
  # beta4 ~ dnorm(rho * beta3, tau)
  # beta5 ~ dnorm(rho * beta4, tau)
  # 
  # # increasing beta's
  # # Non-negative increments for ensuring order
  d[1] ~ dnorm(0, 0.0001)  # beta1 (uninformative)
  for(m in 2:9){
    d[m] ~ dnorm(0, tau_d)T(0,) # non-negative increases
  }
  
  # Ensure monotonicity through cumulative sums
  beta1 <- d[1]
  beta2 <- beta1 + d[2]
  beta3 <- beta2 + d[3]
  beta4 <- beta3 + d[4]
  beta5 <- beta4 + d[5]
  beta6 <- beta5 + d[6]
  beta7 <- beta6 + d[7]
  beta8 <- beta7 + d[8]
  beta9 <- beta8 + d[9]
  
  
  tau_d ~ dgamma(0.01, 0.01) # uninformative prior for tau_d 
  # tau_d ~ dgamma(3,2) # informative prior for tau_d

  
  
  ## priors
  # for AR
  tau ~ dgamma(0.01, 0.01)
  rho ~ dunif(-1, 1)
  # how to structure rho to let VE can only decrease over time?
  
  # uninformative priors for coefficient of confounders 
  beta_cf_age_1 ~ dnorm(0, 1e-04)
  beta_cf_age_2 ~ dnorm(0, 1e-04)
  beta_cf_age_3 ~ dnorm(0, 1e-04)
  beta_cf_age_4 ~ dnorm(0, 1e-04)
  beta_cf_age_5 ~ dnorm(0, 1e-04)
  beta_cf_month_tested_1 ~ dnorm(0, 1e-04)
  beta_cf_month_tested_2 ~ dnorm(0, 1e-04)
  beta_cf_month_tested_3 ~ dnorm(0, 1e-04)
  beta_cf_month_tested_4 ~ dnorm(0, 1e-04)
  # beta_cf_birth_weight ~ dnorm(0, 1e-04)
  # beta_cf_gestage ~ dnorm(0, 1e-04)
  
  # intercept 
  int ~ dnorm(0, 1e-04)

} "



################################################################################
# UNUSED / TESTED models
################################################################################
model_string_waning_continuous <- "
# in this model, we model time since vaccination as continuous variable, and the variable we use to represent time is the inverse of time since vaccination 
# and for unvaccinated individuals, we assume time since vaccination is infinite, thus inverse is 0.
model{
  
  for(j in 1: N_records){
    
    # likelihood of each record
    case_status[j] ~ dbin(p[j], 1)
    
    # # model for waning VE
    # logit(p[j]) <- int + beta * inverse_time_since_vax[j] 
    #   +
    #   cf_age[j] * beta_cf_age +
    #   cf_birth_weight[j] * beta_cf_birth_weight + # 20% missing
    #   #cf_month_tested_1[j] * beta_cf_month_tested_1 +
    #   cf_month_tested_2[j] * beta_cf_month_tested_2 +
    #   cf_month_tested_3[j] * beta_cf_month_tested_3 +
    #   cf_month_tested_4[j] * beta_cf_month_tested_4 +
    #   cf_month_tested_5[j] * beta_cf_month_tested_5 +
    #   cf_month_tested_6[j] * beta_cf_month_tested_6 +
    #   cf_month_tested_7[j] * beta_cf_month_tested_7 +
    #   cf_month_tested_8[j] * beta_cf_month_tested_8 +
    #   cf_gestage[j] * beta_cf_gestage
      
      
    # model for waning VE 
    # logit(p[j]) <- int + beta * inverse_time_since_vax[j] + 
    #   #cf_age_1[j] * beta_cf_age_1 + # dummy variables for categorical confounders
    #   cf_age_2[j] * beta_cf_age_2 +
    #   cf_age_3[j] * beta_cf_age_3 +
    #   cf_age_4[j] * beta_cf_age_4 +
    #   cf_age_5[j] * beta_cf_age_5 +
    #   #cf_month_tested_1[j] * beta_cf_month_tested_1 +
    #   cf_month_tested_2[j] * beta_cf_month_tested_2 +
    #   cf_month_tested_3[j] * beta_cf_month_tested_3 +
    #   cf_month_tested_4[j] * beta_cf_month_tested_4 
      
      
    logit(p[j]) <- int + beta * log_inverse_time_since_vax[j]
      # cf_age_2[j] * beta_cf_age_2 +
      # cf_age_3[j] * beta_cf_age_3 +
      # cf_age_4[j] * beta_cf_age_4 +
      # cf_age_5[j] * beta_cf_age_5 +
      # cf_month_tested_2[j] * beta_cf_month_tested_2 +
      # cf_month_tested_3[j] * beta_cf_month_tested_3 +
      # cf_month_tested_4[j] * beta_cf_month_tested_4 +
      # cf_birth_weight[j] * beta_cf_birth_weight + # 20% missing 
      # cf_gestage[j] * beta_cf_gestage
    
      
    
  }
  
  # priors 
  int ~ dnorm(0, 1e-04)
  # beta_cf_age ~ dnorm(0, 1e-04)
  # beta_cf_birth_weight ~ dnorm(0, 1e-04)
  # beta_cf_month_tested_1 ~ dnorm(0, 1e-04)
  # beta_cf_month_tested_2 ~ dnorm(0, 1e-04)
  # beta_cf_month_tested_3 ~ dnorm(0, 1e-04)
  # beta_cf_month_tested_4 ~ dnorm(0, 1e-04)
  # beta_cf_month_tested_5 ~ dnorm(0, 1e-04)
  # beta_cf_month_tested_6 ~ dnorm(0, 1e-04)
  # beta_cf_month_tested_7 ~ dnorm(0, 1e-04)
  # beta_cf_month_tested_8 ~ dnorm(0, 1e-04)
  

  
  beta_cf_age_1 ~ dnorm(0, 1e-04)
  beta_cf_age_2 ~ dnorm(0, 1e-04)
  beta_cf_age_3 ~ dnorm(0, 1e-04)
  beta_cf_age_4 ~ dnorm(0, 1e-04)
  beta_cf_age_5 ~ dnorm(0, 1e-04)
  beta_cf_month_tested_1 ~ dnorm(0, 1e-04)
  beta_cf_month_tested_2 ~ dnorm(0, 1e-04)
  beta_cf_month_tested_3 ~ dnorm(0, 1e-04)
  beta_cf_month_tested_4 ~ dnorm(0, 1e-04)
  beta_cf_birth_weight ~ dnorm(0, 1e-04)
  beta_cf_gestage ~ dnorm(0, 1e-04)
  
  
  beta ~ dnorm(0, 1e-04)
  beta_vax ~ dnorm(0, 1e-04)
  
}
"

  
  