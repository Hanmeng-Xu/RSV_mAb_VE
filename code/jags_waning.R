
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
      M_cf[j,] %*% beta_cf[] # confounder matrix
  
  }
  
  
  # increasing beta's
  # Non-negative increments for ensuring order
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

  
  ## priors
  
  # uninformative priors for coefficient of confounders 
  for(k in 1:N_cf){ # N_cf: number of confounder dummy variables included 
    beta_cf[k] ~ dnorm(0, 1e-04)
  }
  
  # intercept 
  int ~ dnorm(0, 1e-04)
  
  # for the increment 
  tau_d ~ dgamma(0.01, 0.01) # uninformative prior for tau_d 
  # tau_d ~ dgamma(3,2) # informative prior for tau_d


} "

