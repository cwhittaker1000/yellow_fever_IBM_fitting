# load required libraries
library(individual); library(dplyr); library(EasyABC); library(tidyverse)

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

# Loading in and processing Horto/PEAL data for model fitting
horto_df <- readRDS("data/processed_HortoData.rds") %>%
  filter(!is.na(zone_peal)) %>%
  filter(final_yfv_result != "negative")
epi_curve <- incidence::incidence(horto_df$date_collection)
plot(epi_curve)

# Generating incidence data and cutting off first 4 infections
start_date <- as.Date("2017-12-01")
horto_df_fitting <- horto_df %>%
  filter(date_collection > start_date) %>%
  group_by(date_collection) %>%
  summarise(count = n()) %>%
  complete(date_collection = seq.Date(start_date, 
                                      as.Date("2018-01-08"), 
                                      by = "days"),
           fill = list(count = 0))
plot(horto_df_fitting$date_collection, horto_df_fitting$count)
horto_df_fitting$time <- 1:nrow(horto_df_fitting)

## Particle filtering

## Model parameters
N <- 86 - 3 # (3 negative monkeys - assume the rest not in database killed by yellow fever)
dt <- 0.2
steps <- 1 / dt
initial_infections <- 2
gamma <- 1 / (infectious_period_gamma_shape / infectious_period_gamma_rate)

## Parameters for particle filtering
# R0_scan <- c(4, 5, 6, 7, 8, 9, 10, 11, 12)
R0_scan <- c(4, 8, 12)
particles <- 200
seed <- rpois(particles, 1000000)

## Storage for particle filtering
loglikelihood_matrix <- array(data = NA, dim = c(length(R0_scan), particles, length(horto_df_fitting$time)))
particles_kept_matrix <- array(data = NA, dim = c(length(R0_scan), particles, length(horto_df_fitting$time)))
weights_matrix <- array(data = NA, dim = c(length(R0_scan), particles, length(horto_df_fitting$time)))
deaths_df <- array(data = NA, dim = c(length(R0_scan), particles, length(horto_df_fitting$time)))
deaths_df2 <- array(data = NA, dim = c(length(R0_scan), particles, length(horto_df_fitting$time)))
final_size_matrix <- array(data = NA, dim = c(length(R0_scan), particles, length(horto_df_fitting$time)))
output_matrix <- array(data = NA, dim = c(length(R0_scan), particles, length(horto_df_fitting$time)))

for (i in 1:length(R0_scan)) {
  storage_list <- vector(mode = "list", length = particles)
  beta_sim <- R0_scan[i] * gamma / N
  
  for (j in 1:length(horto_df_fitting$time)) {
    
    num_deaths_timestep_particle <- vector(mode = "double", length = particles)
    
    for (k in 1:particles) {
      
      if (j == 1) {
        temp <- run_simulation2(seed = seed[k], steps = 1 / dt, dt = dt, N = N, 
                                initial_infections = initial_infections, death_obs_prop = 1, 
                                beta = beta_sim,
                                initial_run = TRUE, overall_run_length = 505,
                                latent_period_gamma_shape = latent_period_gamma_shape, 
                                EIP_gamma_shape = EIP_gamma_shape,
                                EIP_gamma_rate = EIP_gamma_rate, 
                                latent_period_gamma_rate = latent_period_gamma_rate,
                                infectious_period_gamma_shape = infectious_period_gamma_shape, 
                                infectious_period_gamma_rate = infectious_period_gamma_rate,
                                death_observation_gamma_shape = 1, 
                                death_observation_gamma_rate = death_observation_gamma_rate,
                                state = NULL)
        storage_list[[k]]$output <- temp$result
        
      } else {
        temp <- run_simulation2(seed = seed[k], steps = (j / dt), dt = dt, N = N, 
                                initial_infections = initial_infections, death_obs_prop = 1, 
                                beta = beta_sim, 
                                initial_run = FALSE, overall_run_length = NA,
                                latent_period_gamma_shape = latent_period_gamma_shape, 
                                EIP_gamma_shape = EIP_gamma_shape,
                                EIP_gamma_rate = EIP_gamma_rate, 
                                latent_period_gamma_rate = latent_period_gamma_rate,
                                infectious_period_gamma_shape = infectious_period_gamma_shape, 
                                infectious_period_gamma_rate = infectious_period_gamma_rate,
                                death_observation_gamma_shape = 1, 
                                death_observation_gamma_rate = death_observation_gamma_rate,
                                state = storage_list[[k]]$state)
        storage_list[[k]]$output <- rbind(storage_list[[k]]$output, 
                                          temp$result[(1 + nrow(temp$result) - 1/dt):nrow(temp$result), ])
      }
      
      storage_list[[k]]$state <- temp$state
      
      curr_timestep_Dobs <- max(temp$result$Dobs_count[(1 + nrow(temp$result) - 1/dt):nrow(temp$result)])
      if (j == 1) {
        prev_timestep_Dobs <- 0
      } else {
        prev_timestep_Dobs <- storage_list[[k]]$output$Dobs_count[(nrow(temp$result) - 1/dt)] # get the last timepoint of the previous
      }
      temp_num_deaths_timestep <- curr_timestep_Dobs - prev_timestep_Dobs
      num_deaths_timestep_particle[k] <- temp_num_deaths_timestep
      deaths_df[i, k, j] <- temp_num_deaths_timestep
      
    }
    
    num_deaths_timestep_particle2 <- deaths_df[i, , j]
    num_deaths_timestep_particle2[num_deaths_timestep_particle2 == 0] <- 0.01 # see if this is actually needed - need to change this.
    eval_loglik <- weight_particles(num_deaths_timestep_particle2, horto_df_fitting$count[j])
    
    weights_matrix[i, , j] <- eval_loglik$raw_weights
    resampled_weights <- sample(1:particles, prob = eval_loglik$normalised_weights, replace = TRUE)
    loglikelihood_matrix[i, , j] <- eval_loglik$logliklihoods[resampled_weights]
    particles_kept_matrix[i, , j] <- resampled_weights
    storage_list <- storage_list[resampled_weights]
    deaths_df2[i, , j] <- num_deaths_timestep_particle[resampled_weights]
    
    print(c(j, k))
    
  }
  print(i)
}

colors37 <- c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b","#bd5975")
par(mfrow = c(2, 5), mar = c(2, 2, 2, 2))
for (k in 1:length(R0_scan)) {
  logliks <- apply(weights_matrix[k, , ], 2, function(x) {
    average_weight <- mean(x)
    log(average_weight)
  })
  loglik <- sum(logliks)
  
  for (i in 1:particles) {
    if (i == 1) {
      plot(deaths_df2[k, i, ], type = "l", col = adjustcolor(colors37[k], alpha.f = 0.1),
           ylim = c(0, max(c(horto_df_fitting$count, deaths_df2[k, , ]))),
           main = paste0("R0 = ", R0_scan[k], ", loglik = ", round(loglik)),
           ylab = "", xlab = "")
    } else {
      lines(deaths_df2[k, i, ], type = "l", col = adjustcolor(colors37[k], alpha.f = 0.1))
    }
  }
  points(horto_df_fitting$count, pch = 20, col = "black", cex = 1)
}
