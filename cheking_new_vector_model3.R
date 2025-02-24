# load required libraries
library(individual); library(dplyr); library(tidyverse); library(doParallel); library(tictoc); library(parallel); library(profvis);

## Sourcing functions
source("functions/IBM_model_explicitVector_check.R")

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

N <- 10000 # this seemed to be working okay when N was 100 but maybe red herring
v_population <- 100000
R0 <- 1.5
EIP_mean <- EIP_gamma_shape / EIP_gamma_rate
NHP_death_rate <- 1 / (infectious_period_gamma_shape / infectious_period_gamma_rate)
vector_mortality_rate <- 0.02
beta_HV <- 0.03
# beta <- (R0 * vector_mortality_rate * NHP_dur_inf_mean) / (exp(- vector_mortality_rate * EIP_mean) * beta_VH)

final_size <- c()
for (i in 1:25) {
  x <- run_simulation_vector(seed = rpois(1, 100000), 
                             steps = 8000, 
                             dt = 0.2, 
                             N = N, 
                             initial_infections = 5, 
                             death_obs_prop = 1,
                             R0 = R0,
                             EIP_mean = EIP_mean,
                             NHP_death_rate = NHP_death_rate,
                             beta_HV = beta_HV,
                             initial_run = TRUE, 
                             overall_run_length = NA,
                             importation_rate = 0,
                             EIP_gamma_shape = EIP_gamma_shape,
                             EIP_gamma_rate = EIP_gamma_rate,
                             latent_period_gamma_shape = latent_period_gamma_shape, 
                             latent_period_gamma_rate = latent_period_gamma_rate,
                             infectious_period_gamma_shape = infectious_period_gamma_shape, 
                             infectious_period_gamma_rate = infectious_period_gamma_rate,
                             death_observation_gamma_shape = 1, 
                             death_observation_gamma_rate = death_observation_gamma_rate,
                             v_population = v_population,
                             vector_mortality_rate = vector_mortality_rate,
                             state = NULL) 
  final_size <- c(final_size, max(x$result$Dobs_count + x$result$D_unobs_count) / N)
  print(max(x$result$Dobs_count + x$result$D_unobs_count) / N)
  if (i == 1) {
    plot(x$result$timestep, x$result$Dobs_count + x$result$D_unobs_count, type = "l", ylim = c(0, N))
  } else {
    par(new=TRUE)
    plot(x$result$timestep, x$result$Dobs_count + x$result$D_unobs_count, type = "l", ylim = c(0, N))
  }
}
hist(final_size, breaks = 30)
final_size
median(final_size)
mean(final_size)

epi <- x$result
plot(epi$timestep, epi$S_count, type = "l", ylim = c(0, N))
lines(epi$timestep, epi$E_count, col = "red")
lines(epi$timestep, epi$I_count, col = "green")
lines(epi$timestep, epi$Dobs_count + epi$D_unobs_count, col = "blue")

vector <- x$final_vector_state
plot(vector$timestep, v_population * vector$V_S_count / max(vector$V_S_count), type = "l", ylim = c(0, v_population), col = "purple")
lines(vector$timestep, v_population * vector$V_E_count / max(vector$V_S_count), type = "l", col = "orange")
lines(vector$timestep, v_population * vector$V_I_count / max(vector$V_S_count), type = "l", col = "grey")

plot(vector$timestep, vector$D_v_new, type = "l")
