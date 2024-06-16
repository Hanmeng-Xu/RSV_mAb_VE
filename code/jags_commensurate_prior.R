# model from:
# https://pubmed.ncbi.nlm.nih.gov/37963353/
# https://github.com/RsvModeling/sequential_bayesian_trials

# adjusted model with uninformative priors 
model_string_uninformative_priors <- "

model{

# loop for data from current case-control study with N_people total (cases + controls)
for(j in 1:N_people){
  # Likelihood for data case control
  case_status[j] ~ dbin(pi[j], 1)  # TBC (N=1 as this is only one individual patient/record), or maybe can write bernoulli distribution
  
  # adjusted logistic regression
  logit(pi[j])  <- int  +  vax[j]*beta1  + cf_age[j]*beta2 + cf_month_tested[j]*beta3
}
  
# uninformative priors for current case-control
  int ~ dnorm(0, 1e-04)
  beta1 ~ dnorm(0, 1e-04) 
  beta2 ~ dnorm(0, 1e-04) 
  beta3 ~ dnorm(0, 1e-04) 

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

#loop for data from current case-control study with N_people total (cases + controls)
for(j in 1:N_people){
  # Likelihood for data case control
  case_status[j] ~ dbin(pi[j], 1)  # TBC (N=1 as this is only one individual patient/record), or maybe can write bernoulli distribution
 
  # adjusted logistic regression
  logit(pi[j])  <- int  +  vax[j]*beta1 + cf_age[j]*beta2 + cf_month_tested[j]*beta3
}
  
 
  # Priors for original trial
  int_orig ~ dnorm(0, 1e-4) #uninformative prior on intercept from originl trial
  delta ~ dnorm(0, 1e-4)    #uninformative prior on VE for original trial

  # Priors for current case-control
  int ~ dnorm(int_orig, tau2)
  beta1 ~ dnorm(delta, tau) #beta centered on delta with highly informative prior, which can become less informative if it does not match
  
  # Uninformative priors for confounders
  beta2 ~ dnorm(0, 1e-4)
  beta3 ~ dnorm(0, 1e-4)

  tau ~ dgamma(set_tau_shp, set_tau_rate) # skeptical variance--prevents extremes--might bias towards prior value?
  tau2 ~ dgamma(set_tau_shp, set_tau_rate) # gamma hyperprior from psborrow; uninformative
   
   #these don't do anything; they are just carried through so function works
   a1 <- prior_prec_log_irr
   a2 <- prior_mean_log_irr
   a3 <- sd.upper

  # alpha=1
}
"