# load required libraries
library(individual); library(dplyr); library(tidyverse); library(truncnorm); library(tmvtnorm)
library(foreach); library(doParallel); library(tictoc)

## Sourcing functions
source("functions/model2.R")
source("functions/particle_filter.R")

# Loading in fitted parameters
death_observation_fit <- readRDS("outputs/deathObservation_distExp_stanFit.rds")
death_observation_gamma_rate <- mean(rstan::extract(death_observation_fit, "a")[[1]]) # note exp used here and gamma below, but if shape set to 1, then is an exponential
latent_period_fit <- readRDS("outputs/exposure_infectiousDist_stanFit.rds")
latent_period_gamma_shape <- mean(rstan::extract(latent_period_fit, "a")[[1]]) # note exp used here and gamma below, but if shape set to 1, then is an exponential
latent_period_gamma_rate <- mean(rstan::extract(latent_period_fit, "b")[[1]]) # note exp used here and gamma below, but if shape set to 1, then is an exponential
infectious_period_fit <- readRDS("outputs/infectious_deathDist_stanFit.rds")
infectious_period_gamma_shape <- mean(rstan::extract(infectious_period_fit, "a")[[1]]) # note exp used here and gamma below, but if shape set to 1, then is an exponential
infectious_period_gamma_rate <- mean(rstan::extract(infectious_period_fit, "b")[[1]]) # note exp used here and gamma below, but if shape set to 1, then is an exponential
EIP_gamma_fit <- readRDS("outputs/EIP_adultMice_gammaParams.rds")
EIP_gamma_shape <- EIP_gamma_fit$gamma_a
EIP_gamma_rate <- EIP_gamma_fit$gamma_b

## Generating synthetic data
R0 <- 3
N <- 86
gamma <- 1 / (infectious_period_gamma_shape / infectious_period_gamma_rate)
beta_sim <- R0 * gamma / N
dt <- 0.2
past_length <- 15
initial_infections <- 5
empirical_seed <- 180
steps <- 325
synthetic_data <- run_simulation2(seed = empirical_seed, steps = steps, dt = dt, N = N, 
                                  initial_infections = initial_infections, death_obs_prop = 1, 
                                  beta = beta_sim, 
                                  initial_run = TRUE, overall_run_length = steps,
                                  latent_period_gamma_shape = latent_period_gamma_shape, 
                                  EIP_gamma_shape = EIP_gamma_shape,
                                  EIP_gamma_rate = EIP_gamma_rate, 
                                  latent_period_gamma_rate = latent_period_gamma_rate,
                                  infectious_period_gamma_shape = infectious_period_gamma_shape, 
                                  infectious_period_gamma_rate = infectious_period_gamma_rate,
                                  death_observation_gamma_shape = 1, 
                                  death_observation_gamma_rate = death_observation_gamma_rate,
                                  state = NULL)


synthetic_data$result$time <- ceiling(synthetic_data$result$timestep * dt)
observed_incidence <- synthetic_data$result %>%
  mutate(incidence = c(0, diff(Dobs_count))) %>%
  group_by(time) %>%
  summarise(daily_incidence = sum(incidence))
observed_data <- observed_incidence$daily_incidence
plot(observed_data)

## Defining stuff needed for MCMC
data <- observed_incidence
particles <- 1000
misc <- list(seed = rpois(particles, 100000), 
             steps = steps, 
             gamma = gamma,
             particles = particles,
             dt = dt, 
             N = N, 
             initial_infections = initial_infections, 
             death_obs_prop = 1, 
             beta = beta_sim, 
             initial_run = TRUE, 
             overall_run_length = steps,
             latent_period_gamma_shape = latent_period_gamma_shape, 
             EIP_gamma_shape = EIP_gamma_shape,
             EIP_gamma_rate = EIP_gamma_rate, 
             latent_period_gamma_rate = latent_period_gamma_rate,
             infectious_period_gamma_shape = infectious_period_gamma_shape, 
             infectious_period_gamma_rate = infectious_period_gamma_rate,
             death_observation_gamma_shape = 1, 
             death_observation_gamma_rate = death_observation_gamma_rate,
             fixed = FALSE,
             mean = 3,
             sd = 5,
             lower = 1,
             upper = 10)

## Testing each of the particle filter functions
r_logprior(params, misc)

params <- c("R0" = 3)
r_loglike(params, data, misc)

params <- c("R0" = 1.25)
r_loglike(params, data, misc)

params <- c("R0" = 1)
r_loglike(params, data, misc)

r_logposterior(params, data, misc)

## Running the particle MCMC
initial_values <- c("R0" = 3)
tic()
x <- run_MCMC(initial_values = initial_values,
              misc = misc,
              data = data, 
              iterations = 1000,
              burnin = 500,
              start_covariance_adaptation = 100,
              sd_proposals = c(0.1),
              scaling_factor = 1,
              target_acceptance = 0.2,
              refresh = 5,
              temp_save = FALSE,
              final_save = FALSE,
              save_directory = getwd(),
              filename = "",
              output = TRUE)
toc()


