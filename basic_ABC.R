# load required libraries
library(individual); library(dplyr); library(EasyABC); library(tidyverse); library(parallel)

## 1) Remember when you first set this up and you were getting D_obs decreasing
## sometimes rather than always monotonically increasing. This was because 
## you'd end up scheduling folks to move ->I but before they did that they'd be moved E->D
## and then they'd move to D_obs, but then eventually the ->I transition would fire and take them
## back to I. Events focus on where the individual will transition TO. It doesn't care 
## where they are before that (i.e. if the individual is in the same state or not as when
## the event was first created)
## 2) get_scheduled() only checks whether someone has been scheduled for a particular event,
## not events in general.
## 3) note that need to think about how long incubation period is for monkey to monkey
## i.e. not implicitly considering the mosquito here - should we be? Think we need delay equations
## in the FOI experienced by S monkeys by I monkeys
## 4) whilst 3) has kind of been addresse with the delay, note that I'm currently multiply weighting
##    folks in I compartment, and need to find some of not doing this (i.e. properly replicate the renewal
##    equation)

## Sourcing functions
source("functions/model2.R")
source("functions/particle_filter.R")

## Generating synthetic data
R0 <- 3
N <- 86
gamma <- 0.25
beta_sim <- R0 * gamma / N
dt <- 0.2
past_length <- 15
past_weightings_vector <- rev(dgamma(1:past_length, shape = 5, rate = 0.5))
synthetic_data <- run_simulation2(seed = 100, steps = 400, dt = dt, N = N, 
                                  initial_infections = 1, death_obs_prop = 1, 
                                  beta = beta_sim, past_length = past_length, past_weightings_vector = past_weightings_vector,
                                  initial_run = TRUE, overall_run_length = 400)
synthetic_data$result$time <- floor(synthetic_data$result$timestep * dt)
observed_incidence <- synthetic_data$result %>%
  mutate(incidence = c(0, diff(Dobs_count))) %>%
  group_by(time) %>%
  summarise(daily_incidence = sum(incidence)) %>%
  mutate(cumulative_incidence = cumsum(daily_incidence))
observed_data <- observed_incidence$daily_incidence
plot(observed_data)
plot(observed_incidence$cumulative_incidence)

## Generate summary statistic from single run
model_simulate <- function(beta, seed) {
  synthetic_data <- run_simulation2(seed = seed, steps = 400, dt = 0.2, N = 86, 
                                    initial_infections = 1, death_obs_prop = 1, 
                                    beta = beta, past_length = 15, 
                                    past_weightings_vector = rev(dgamma(1:past_length, shape = 5, rate = 0.5)),
                                    initial_run = TRUE, overall_run_length = 400)
  synthetic_data$result$time <- floor(synthetic_data$result$timestep * 0.2)
  synthetic_output <- synthetic_data$result %>%
    mutate(incidence = c(0, diff(Dobs_count))) %>%
    group_by(time) %>%
    summarise(daily_incidence = sum(incidence)) %>%
    mutate(cumulative_incidence = cumsum(daily_incidence))
  return(synthetic_output$cumulative_incidence)
}

## Generating distance
rmse <- function(observed, simulated) {
  sqrt(mean((observed - simulated)^2))
}

# Prior sampler: assuming we know nothing about the mean, use a wide uniform distribution
prior_sampler <- function() {
  R0 <- runif(n = 1, min = 1, max = 6) 
  beta_sim <- R0 * gamma / N
  return(beta_sim)
}

# ABC function
abc_parallel_rmse <- function(observed_data, prior_sampler, model_simulate, rmse, tolerance, n) {
  
  # Function to perform one iteration of parameter trial
  # try_parameter <- function(.) {
  #   param <- prior_sampler()
  #   simulated_data <- simulate_data(param, rnbinom(n = 1, mu = 100000, size = 2))
  #   
  #   if(rmse(observed_data, simulated_data) < tolerance) {
  #     return(param)
  #   } else {
  #     return(NA)  # Return NA if the parameter is not accepted
  #   }
  # }
  try_parameter <- function(param) {
    simulated_data <- model_simulate(param, rnbinom(n = 1, mu = 100000, size = 2))
    
    if(rmse(observed_data, simulated_data) < tolerance) {
      return(param)
    } else {
      return(NA)  # Return NA if the parameter is not accepted
    }
  }
  
  # Detect the number of available cores
  no_cores <- detectCores() - 4
  cl <- makeCluster(no_cores)
  clusterExport(cl, c("prior_sampler", "rmse", "observed_data", "tolerance", "run_simulation2",
                      "past_length", "model_simulate"))
  clusterEvalQ(cl, library(individual))
  clusterEvalQ(cl, library(dplyr))
  
  # Store accepted parameters, loop until enough parameters are accepted
  accepted_parameters <- numeric(0)
  while(length(accepted_parameters) < n) {
    # Use mclapply for parallel processing, 'mc.cores' defines number of cores
    # trial_results <- mclapply(1:10, try_parameter, mc.cores = 1)
    params <- replicate(100, prior_sampler())
    trial_results <- parLapply(cl, as.list(params), try_parameter)
    
    # Filter out NAs and append to accepted parameters
    accepted_parameters <- c(accepted_parameters, unlist(Filter(Negate(is.na), trial_results)))
    
    # If more parameters are accepted than needed, truncate the list
    if(length(accepted_parameters) > n) {
      accepted_parameters <- accepted_parameters[1:n]
    }
    print(length(accepted_parameters))
  }
  stopCluster(cl)
  
  return(accepted_parameters)
}

# Run ABC with parallel processing and RMSE
set.seed(123)
observed_data <- observed_incidence$cumulative_incidence
prior_sampler <- prior_sampler
simulate_data <- model_simulate
tolerance <- 20
n <- 500
accepted_parameters <- abc_parallel_rmse(
  observed_data = observed_data,
  prior_sampler = prior_sampler,
  model_simulate = model_simulate,
  rmse = rmse,
  tolerance = tolerance,  # Set a tolerance level, adjusted for RMSE scale
  n = n  # Number of accepted samples we want
)
hist(unlist(accepted_parameters) * N / gamma)

mean(unlist(accepted_parameters) * N / gamma)
median(unlist(accepted_parameters) * N / gamma)
