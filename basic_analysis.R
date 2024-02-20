# load required libraries
library(individual); library(dplyr); library(EasyABC); library(tidyverse)

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

## Sourcing functions
source("functions/model2.R")
source("functions/particle_filter.R")

## Generating synthetic data
R0 <- 1.5
N <- 86
gamma <- 0.25
beta_sim <- R0 * gamma / N
dt <- 0.2
past_length <- 15
past_weightings_vector <- rev(dgamma(1:past_length, shape = 5, rate = 0.5))
synthetic_data <- run_simulation2(seed = 100, steps = 500, dt = dt, N = N, 
                                  initial_infections = 1, death_obs_prop = 1, 
                                  beta = beta_sim, past_length = past_length, past_weightings_vector = past_weightings_vector,
                                  initial_run = TRUE, overall_run_length = 500)
synthetic_data$result$time <- floor(synthetic_data$result$timestep * dt)
observed_incidence <- synthetic_data$result %>%
  mutate(incidence = c(0, diff(Dobs_count))) %>%
  group_by(time) %>%
  summarise(daily_incidence = sum(incidence))
observed_data <- observed_incidence$daily_incidence
plot(observed_data)

## Particle Filtering
storage_list <- list()
seed <- rpois(1, 1000000)
for (i in 1:length(observed_data)) {
  if (i == 1) {
    temp <- run_simulation2(seed = seed, steps = 1 / dt, dt = dt, N = N, 
                             initial_infections = 1, death_obs_prop = 1, 
                             beta = beta_sim, past_length = past_length, 
                             past_weightings_vector = past_weightings_vector,
                             initial_run = TRUE, overall_run_length = 505)
    storage_list$output <- temp$result
    storage_list$state <- temp$state
    storage_list$lagged_I <- temp$lagged_I
  } else {
    temp <- run_simulation2(seed = seed, steps = (i / dt), dt = dt, N = N, 
                            initial_infections = 1, death_obs_prop = 1, 
                            beta = 3 * beta_sim, past_length = past_length, 
                            past_weightings_vector = past_weightings_vector,
                            initial_run = FALSE, overall_run_length = NA,
                            lagged_I_input = storage_list$lagged_I,
                            state = storage_list$state)
    storage_list$output <- rbind(storage_list$output, 
                                 temp$result[(1 + nrow(temp$result) - 1/dt):nrow(temp$result), ])
    storage_list$state <- temp$state
    storage_list$lagged_I <- temp$lagged_I

  }
}
plot(diff(-storage_list$output$S_count))



## Simplified version
storage_list <- vector(mode = "list", length = 1)
loglikelihood_matrix <- matrix(nrow = 1, ncol = length(observed_data))
particles_kept_matrix <- matrix(nrow = 1, ncol = length(observed_data))
weights_matrix <- matrix(nrow = 1, ncol = length(observed_data))
deaths_df <- matrix(nrow = 1, ncol = length(observed_data))
deaths_df2 <- matrix(nrow = 1, ncol = length(observed_data))
for (i in 1:length(observed_data)) {
  if (i == 1) {
    temp_mod_output <- run_simulation2(seed = rpois(1, 10^8), steps = 1 / dt, dt = dt, N = N, 
                                       initial_infections = 1, death_obs_prop = 1, 
                                       beta = beta_sim, past_length = past_length, past_weightings_vector = past_weightings_vector)
    storage_list[[1]]$output <- temp_mod_output$result
    storage_list[[1]]$state <- temp_mod_output$state
    previous_timestep_final_Dobs <- 0
    current_timestep_final_Dobs <- max(storage_list[[1]]$output$Dobs_count[(1 + nrow(temp_mod_output$result) - 1/dt):nrow(temp_mod_output$result)])
    temp_num_deaths_timestep <- current_timestep_final_Dobs - previous_timestep_final_Dobs
    num_deaths_timestep_particle[j] <- temp_num_deaths_timestep
    deaths_df[1, i] <- num_deaths_timestep_particle[1]
  } else {
    temp_mod_output <- run_simulation2(seed = rpois(1, 10^8), steps = (i / dt), dt = dt, N = N, 
                                       initial_infections = 1, death_obs_prop = 1, 
                                       beta = beta_sim, past_length = past_length, past_weightings_vector = past_weightings_vector,
                                       state = storage_list[[1]]$state)
    storage_list[[1]]$output <- rbind(storage_list[[1]]$output, temp_mod_output$result[(1 + nrow(temp_mod_output$result) - 1/dt):nrow(temp_mod_output$result), ])
    storage_list[[1]]$state <- temp_mod_output$state
    
    ### this is wrong - need to remember to change this!
    previous_timestep_final_Dobs <- max(storage_list[[1]]$output$Dobs_count[(1 + nrow(temp_mod_output$result) - 2/dt):(nrow(temp_mod_output$result) - 1/dt)])
    current_timestep_final_Dobs <- max(storage_list[[1]]$output$Dobs_count[(1 + nrow(temp_mod_output$result) - 1/dt):nrow(temp_mod_output$result)])
    temp_num_deaths_timestep <- current_timestep_final_Dobs - previous_timestep_final_Dobs
    
    
    num_deaths_timestep_particle <- temp_num_deaths_timestep
    deaths_df[1, i] <- num_deaths_timestep_particle
  }
}

plot(deaths_df[1, ])
sum(deaths_df[1, ])

### Setup
particles <- 20
steps <- 1 / dt
storage_list <- vector(mode = "list", length = particles)
loglikelihood_matrix <- matrix(nrow = particles, ncol = length(observed_data))
particles_kept_matrix <- matrix(nrow = particles, ncol = length(observed_data))
weights_matrix <- matrix(nrow = particles, ncol = length(observed_data))
deaths_df <- matrix(nrow = particles, ncol = length(observed_data))
deaths_df2 <- matrix(nrow = particles, ncol = length(observed_data))

test_beta <- beta_sim
for (i in 1:length(observed_data)) {
  
  for (j in 1:particles) {
    
    num_deaths_timestep_particle <- vector(mode = "double", length = particles)
    
    if (i == 1) {
      temp_mod_output <- run_simulation2(seed = rpois(1, 10^8), steps = 1 / dt, dt = dt, N = N, 
                                         initial_infections = 1, death_obs_prop = 1, 
                                         beta = beta_sim, past_length = 1, past_weightings_vector = 1)
      storage_list[[j]]$output <- temp_mod_output$result
      
    } else {
      temp_mod_output <- run_simulation2(seed = rpois(1, 10^8), steps = (i / dt), dt = dt, N = N, 
                                         initial_infections = 1, death_obs_prop = 1, 
                                         beta = beta_sim, past_length = 1, past_weightings_vector = 1,
                                         state = storage_list[[j]]$state)
      storage_list[[j]]$output <- rbind(storage_list[[j]]$output, temp_mod_output$result[(1 + nrow(temp_mod_output$result) - 1/dt):nrow(temp_mod_output$result), ])
      
    }
    storage_list[[j]]$state <- temp_mod_output$state
    timestep_Dobs <- temp_mod_output$result$Dobs_count[(1 + nrow(temp_mod_output$result) - 1/dt):nrow(temp_mod_output$result)]
    temp_num_deaths_timestep <- max(timestep_Dobs) - min(timestep_Dobs)
    num_deaths_timestep_particle[j] <- temp_num_deaths_timestep
    deaths_df[j, i] <- num_deaths_timestep_particle[j]
  }
  
  num_deaths_timestep_particle2 <- deaths_df[, i]
  num_deaths_timestep_particle2[num_deaths_timestep_particle2 == 0] <- 0.01
  eval_loglik <- weight_particles(num_deaths_timestep_particle2, observed_data[i])
  
  ## should the weights be the resampled or the raw weights???
  weights_matrix[, i] <- eval_loglik$raw_weights
  resampled_weights <- sample(1:particles, prob = eval_loglik$normalised_weights, replace = TRUE)
  loglikelihood_matrix[, i] <- eval_loglik$logliklihoods[resampled_weights]
  particles_kept_matrix[, i] <- resampled_weights
  storage_list <- storage_list[resampled_weights]
  deaths_df2[, i] <- num_deaths_timestep_particle[resampled_weights]
  
  if (length(unique(resampled_weights)) == 1) {
    stop("particle collapse")
  }
  
  print(c(i, j))
  
}

apply(particles_kept_matrix, 2, function(x) length(unique(x)))
for (i in 1:particles) {
  if (i == 1) {
    plot(deaths_df2[i, ], type = "l", col = adjustcolor("black", alpha.f = 0.2),
         ylim = c(0, max(c(observed_data, deaths_df2))))
  } else {
    lines(deaths_df2[i, ], type = "l", col = adjustcolor("black", alpha.f = 0.2))
  }
}
points(observed_data, pch = 20, col = "black", cex = 2)

logliks <- apply(weights_matrix, 2, function(x) {
  average_weight <- mean(x)
  log(average_weight)
})
sum(logliks)





## R0 scan to evaluate whether likelihood seems right for different R0 values
R0_vector <- c(0.5, 0.9, 1, 1.1, 1.5, 2, 3, 5)
iterations <- 100
final_size_matrix <- matrix(NA, nrow = length(R0_vector), ncol = iterations)
loglikelihood_matrix <- matrix(NA, nrow = length(R0_vector), ncol = iterations)
output_array <- array(NA, dim = c(length(R0_vector), iterations, dt * 500 + 1))
for (i in 1:length(R0_vector)) {
  test_beta <- R0_vector[i] * gamma / N
  for (j in 1:iterations) {
    temp <- evaluate_likelihood(observed_data = observed_data, seed = j * R0_vector[i], steps = 500, dt = dt, N = N,
                                initial_infections = 1, death_obs_prop = 1, 
                                beta = test_beta, past_length = past_length, past_weightings_vector = past_weightings_vector)
    final_size_matrix[i, j] <- sum(temp$model_output$daily_incidence)
    loglikelihood_matrix[i, j] <- temp$loglikelihood
    output_array[i, j, ] <- temp$model_output$daily_incidence
  }
  print(i)
}

plot(R0_vector, apply(final_size_matrix, 1, function(x) sum(x > 25))/iterations)
plot(R0_vector, apply(loglikelihood_matrix, 1, function(x) median(x)))

loglikelihood_matrix[R0_vector == 2, 86]
final_size_matrix[R0_vector == 2, 86]

plot(output_array[R0_vector == 2, 86, ], type = "l", ylim = c(0, max(observed_data)))
points(observed_data, pch = 20, col = "black")

sum(dpois(x = observed_data, lambda = output_array[R0_vector == 2, 86, ], log = TRUE))
sum(dpois(x = observed_data, lambda = output_array[R0_vector == 2, 86, ], log = TRUE))

x <- data.frame(R0 = R0_vector, final_size_matrix) %>%
  pivot_longer(cols = -R0) %>%
  rename(final_size = value)
y <- data.frame(R0 = R0_vector, loglikelihood_matrix) %>%
  pivot_longer(cols = -R0) %>%
  rename(loglik = value)
z <- x %>%
  left_join(y, by = c("R0", "name"))
z %>%
  filter(final_size > 10) %>%
  group_by(R0) %>% 
  summarise(num_sims = n(), 
            avg_loglik = mean(loglik))



