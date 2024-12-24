# load required libraries
library(individual); library(dplyr); library(tidyverse); library(doParallel); library(tictoc); library(parallel); library(profvis);

## Sourcing functions
source("functions/IBM_model.R")
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
EIP_gamma_fit <- readRDS("outputs/EIP_adultMice_gammaParams_25degrees.rds")
EIP_gamma_shape <- EIP_gamma_fit$gamma_a
EIP_gamma_rate <- EIP_gamma_fit$gamma_b

## Model parameters
N <- 200
days <- 50
initial_infections <- 1
importations <- 7
importation_rate <- 7 / 32 # the actual time when all the susceptibles are depleted
dt <- 0.2
steps <- days / dt
iterations <- 100

x <- run_simulation2(seed = rnbinom(n = 1, mu = 10^5, size = 1), 
                     steps = steps,
                     dt = dt,
                     N = N, 
                     initial_infections = 0, 
                     death_obs_prop = 1, 
                     beta = 0.01, 
                     initial_run = TRUE, 
                     overall_run_length = steps, 
                     importation_rate = importation_rate,
                     EIP_gamma_shape = EIP_gamma_shape,
                     EIP_gamma_rate = EIP_gamma_rate,
                     latent_period_gamma_shape = latent_period_gamma_shape, 
                     latent_period_gamma_rate = latent_period_gamma_rate,
                     infectious_period_gamma_shape = infectious_period_gamma_shape, 
                     infectious_period_gamma_rate = infectious_period_gamma_rate,
                     death_observation_gamma_shape = 1, 
                     death_observation_gamma_rate = death_observation_gamma_rate,
                     state = NULL)

plot(x$result$num_to_import)
plot(x$result$Dobs_count)


possible_importation_count <- vector(mode = "numeric", length = iterations)
actual_importation_count <- vector(mode = "numeric", length = iterations)


for (i in 1:iterations) {
  x <- run_simulation2(seed = rnbinom(n = 1, mu = 10^5, size = 1), 
                       steps = steps,
                       dt = dt,
                       N = N, 
                       initial_infections = 0, 
                       death_obs_prop = 1, 
                       beta = 0.05, 
                       initial_run = TRUE, 
                       overall_run_length = steps, 
                       importation_rate = importation_rate,
                       EIP_gamma_shape = EIP_gamma_shape,
                       EIP_gamma_rate = EIP_gamma_rate,
                       latent_period_gamma_shape = latent_period_gamma_shape, 
                       latent_period_gamma_rate = latent_period_gamma_rate,
                       infectious_period_gamma_shape = infectious_period_gamma_shape, 
                       infectious_period_gamma_rate = infectious_period_gamma_rate,
                       death_observation_gamma_shape = 1, 
                       death_observation_gamma_rate = death_observation_gamma_rate,
                       state = NULL)
  possible_importation_count[i] <- sum(x$result$num_imports)
  actual_importation_count[i] <- sum(x$result$num_to_import, na.rm = TRUE)
  print(i)
}
hist(possible_importation_count, breaks = 20)
hist(actual_importation_count, breaks = 20)

plot(x$result$num_imports, pch = 20)
points(x$result$num_to_import, col = "red")

plot(x$result$S_count)

sum(x$result$num_imports)
sum(x$result$num_to_import, na.rm = TRUE)

plot(x$result$D_obs_new)

mean(possible_importation_count)
mean(actual_importation_count)


