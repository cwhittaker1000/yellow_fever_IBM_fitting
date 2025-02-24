# load required libraries
library(individual); library(dplyr); library(tidyverse); library(doParallel); library(tictoc); library(parallel); library(profvis);

## Sourcing functions
source("functions/IBM_model_explicitVector.R")
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

seed = 10 
steps = 250
dt = 0.2
N = 83
initial_infections = 5
death_obs_prop = 0.5
beta = 0.05
initial_run = TRUE
overall_run_length = NA
importation_rate = 0.25
EIP_gamma_shape = EIP_gamma_shape
EIP_gamma_rate = EIP_gamma_rate
latent_period_gamma_shape = latent_period_gamma_shape 
latent_period_gamma_rate = latent_period_gamma_rate
infectious_period_gamma_shape = infectious_period_gamma_shape
infectious_period_gamma_rate = infectious_period_gamma_rate
death_observation_gamma_shape = 1 
death_observation_gamma_rate = death_observation_gamma_rate
vector_mortality_rate = 0.02
state = NULL

EIP_mean <- EIP_gamma_shape / EIP_gamma_rate
NHP_dur_inf_mean <- infectious_period_gamma_shape / infectious_period_gamma_rate
R0 <- ((10000 / N) * beta * exp(-vector_mortality_rate * EIP_mean)) / (NHP_dur_inf_mean * vector_mortality_rate)
R0 <- 5
beta <- (R0 * (1 / NHP_dur_inf_mean) * vector_mortality_rate) / ((10000 / N) * exp(-vector_mortality_rate * EIP_mean))

R0_scan <- c(1, 2, 3, 4, 5)
iterations <- 100
final_size <- array(data = NA, dim = c(length(R0_scan), iterations))
for (i in 1:length(R0_scan)) {
  beta <- (R0_scan[i] * (1 / NHP_dur_inf_mean) * vector_mortality_rate) / ((10000 / N) * exp(-vector_mortality_rate * EIP_mean))
  for (j in 1:iterations) {
    x <- run_simulation_vector(seed = j, 
                               steps = 1000, 
                               dt = 0.2, 
                               N = 83, 
                               initial_infections = 5, 
                               death_obs_prop = 1, 
                               beta = beta,
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
                               vector_mortality_rate = vector_mortality_rate,
                               state = NULL) 
    final_size[i, j] <- max(x$result$Dobs_count) + max(x$result$D_unobs_count)
  }
  print(i)
}

hist(final_size, breaks = 20)
apply(final_size, 1, mean)

########
final_size <- c()
for (i in 1:25) {
  x <- run_simulation_vector(seed = rpois(1, 100000), 
                             steps = 2000, 
                             dt = 0.1, 
                             N = 83, 
                             initial_infections = 5, 
                             death_obs_prop = 1, 
                             beta_fixed = 0.5,
                             R0 = 1,
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
                             vector_mortality_rate = vector_mortality_rate,
                             state = NULL) 
  final_size <- c(final_size, max(x$result$Dobs_count + x$result$D_unobs_count) / N)
  print(max(x$result$Dobs_count + x$result$D_unobs_count) / N)
}
hist(final_size, breaks = 30)
final_size
median(final_size)
mean(final_size)

epi <- x$result
plot(epi$timestep, epi$S_count, type = "l", ylim = c(0, 83))
lines(epi$timestep, epi$E_count, col = "red")
lines(epi$timestep, epi$I_count, col = "green")
lines(epi$timestep, epi$Dobs_count + epi$D_unobs_count, col = "blue")

vector <- x$final_vector_state
lines(vector$timestep, 80 * vector$V_S_count / max(vector$V_S_count), type = "l", ylim = c(0, 10000), col = "purple")
lines(vector$timestep, 80 * vector$V_E_count / max(vector$V_S_count), type = "l", col = "orange")
lines(vector$timestep, 80 * vector$V_I_count / max(vector$V_S_count), type = "l", col = "grey")

########

# R0 <- 1
# beta_fixed <- 0.4
# beta <- (R0 * (1 / NHP_dur_inf_mean) * vector_mortality_rate) / ((10000 / N) * exp(-vector_mortality_rate * EIP_mean) * beta_fixed)
# x <- run_simulation_vector(seed = j, 
#                            steps = 2000, 
#                            dt = 0.1, 
#                            N = 83, 
#                            initial_infections = 5, 
#                            death_obs_prop = 1, 
#                            beta_fixed = beta_fixed,
#                            beta = beta,
#                            initial_run = TRUE, 
#                            overall_run_length = NA,
#                            importation_rate = 0,
#                            EIP_gamma_shape = EIP_gamma_shape,
#                            EIP_gamma_rate = EIP_gamma_rate,
#                            latent_period_gamma_shape = latent_period_gamma_shape, 
#                            latent_period_gamma_rate = latent_period_gamma_rate,
#                            infectious_period_gamma_shape = infectious_period_gamma_shape, 
#                            infectious_period_gamma_rate = infectious_period_gamma_rate,
#                            death_observation_gamma_shape = 1, 
#                            death_observation_gamma_rate = death_observation_gamma_rate,
#                            vector_mortality_rate = vector_mortality_rate,
#                            state = NULL) 
# 
# epi <- x$result
# plot(epi$timestep, epi$S_count, type = "l", ylim = c(0, 83))
# lines(epi$timestep, epi$E_count, col = "red")
# lines(epi$timestep, epi$I_count, col = "green")
# lines(epi$timestep, epi$Dobs_count + epi$D_unobs_count, col = "blue")
# 
# vector <- x$final_vector_state
# lines(vector$timestep, 80 * vector$V_S_count / max(vector$V_S_count), type = "l", ylim = c(0, 10000), col = "purple")
# lines(vector$timestep, 80 * vector$V_E_count / max(vector$V_S_count), type = "l", col = "orange")
# lines(vector$timestep, 80 * vector$V_I_count / max(vector$V_S_count), type = "l", col = "grey")
# 
# 
# 
# 
