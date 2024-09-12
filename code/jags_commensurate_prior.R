# model from:
# https://pubmed.ncbi.nlm.nih.gov/37963353/
# https://github.com/RsvModeling/sequential_bayesian_trials

# adjusted model with uninformative priors (simplify confounders)
model_string_uninformative_priors <- "

model{

# loop for data from current case-control study with N_people total (cases + controls)
for(j in 1:N_people){
  # Likelihood for data case control
  case_status[j] ~ dbin(pi[j], 1)  
  
  # adjusted logistic regression
  logit(pi[j])  <- int  +  vax[j]*beta1  + M_cf[j,] %*% beta_cf[]
}
  
# uninformative priors for current case-control
  int ~ dnorm(0, 1e-04)
  beta1 ~ dnorm(0, 1e-04) 
  for(k in 1:N_cf){ # N_cf: number of confounder dummy variables included 
    beta_cf[k] ~ dnorm(0, 1e-04)
  }
  

}
"



# adjusted model using trial data as prior 
model_string_informative_priors <- "

model{

# data from previous trial and estimate of vaccine effect.  
# N_cases_orig is a vector of length 2 with N cases in placebo and vax group; 
# pop_orig has population size in placebo and vax group
for(i in 1:2){
  N_cases_orig[i] ~ dpois(mu[i]) # Likelihood for data from original trial
  
  log(mu[i]) <- int_orig + (i-1)*delta + log(pop_orig[i]/100000) # i=1: placebo, i=2:vax
}

# loop for data from current case-control study with N_people total (cases + controls)
for(j in 1:N_people){
  # Likelihood for data case control
  case_status[j] ~ dbin(pi[j], 1)  
  
  # adjusted logistic regression
  logit(pi[j])  <- int  +  vax[j]*beta1  + M_cf[j,] %*% beta_cf[]
}
  
 
  # Priors for original trial
  int_orig ~ dnorm(0, 1e-4) #uninformative prior on intercept from originl trial
  delta ~ dnorm(0, 1e-4)    #uninformative prior on VE for original trial

  # Priors for current case-control
  int ~ dnorm(0, 1e-04) # the intercept shouldn't rely on the old trial, as in the original trial, it was risk but in current case control it is odds
  beta1 ~ dnorm(delta, tau) #beta centered on delta with highly informative prior, which can become less informative if it does not match
  
  # Uninformative priors for confounders
  for(k in 1:N_cf){ # N_cf: number of confounder dummy variables included 
    beta_cf[k] ~ dnorm(0, 1e-04)
  }

  # tau ~ dgamma(set_tau_shp, set_tau_rate) # skeptical variance--prevents extremes--might bias towards prior value?
  # tau2 ~ dgamma(set_tau_shp, set_tau_rate) # gamma hyperprior from psborrow; uninformative
  tau ~ dgamma(0.01, 0.01)
  tau2 ~ dgamma(0.01, 0.01)
   
   #these don't do anything; they are just carried through so function works
   a1 <- prior_prec_log_irr
   a2 <- prior_mean_log_irr
   a3 <- sd.upper
   
   # quantified the amount of information-sharing between the previous trial and 
   # current study as a transformation of the variance parameter from the commensurate prior (exp(-σ1^2)).
   # If the variance was small, that indicated increased sharing, and the transformed value was close to 1. 
   # If the variance was large, this indicated decreased sharing, and the value approached 0.
   shared <- exp(-1/tau)
   
   
  # alpha=1
}
"




