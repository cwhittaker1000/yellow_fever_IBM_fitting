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
R0 <- 3
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

## Checking the broken up simulations working okay
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
                            beta = beta_sim, past_length = past_length, 
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
output_incidence <- data.frame(time = storage_list$output$timestep * dt,
                               incidence = c(0, diff(storage_list$output$Dobs_count))) %>% 
  mutate(timestep = floor(time)) %>%
  group_by(timestep) %>% 
  summarise(incidence = sum(incidence))
plot(output_incidence$timestep, output_incidence$incidence, type = "l")
points(observed_data, pch = 20)

## R0 scan
R0_scan <- c(0.75, 1, 1.25, 1.5, 2, 3, 4, 5)
iterations <- 50
seed <- rpois(iterations, 1000000)
steps <- 1 / dt
final_size_matrix <- matrix(NA, nrow = length(R0_scan), ncol = iterations)
output_matrix <- array(data = NA, dim = c(length(R0_scan), iterations, length(output_incidence$incidence)))

storage_list <- list()
for (k in 1:length(R0_scan)) {
  beta_sim <- R0_scan[k] * gamma / N
  for (j in 1:iterations) {
    for (i in 1:length(observed_data)) {
      if (i == 1) {
        temp <- run_simulation2(seed = seed[j], steps = 1 / dt, dt = dt, N = N, 
                                initial_infections = 1, death_obs_prop = 1, 
                                beta = beta_sim, past_length = past_length, 
                                past_weightings_vector = past_weightings_vector,
                                initial_run = TRUE, overall_run_length = 505)
        storage_list$output <- temp$result
        storage_list$state <- temp$state
        storage_list$lagged_I <- temp$lagged_I
      } else {
        temp <- run_simulation2(seed = seed[j], steps = (i / dt), dt = dt, N = N, 
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
    output_incidence <- data.frame(time = storage_list$output$timestep * dt,
                                   incidence = c(0, diff(storage_list$output$Dobs_count))) %>% 
      mutate(timestep = floor(time)) %>%
      group_by(timestep) %>% 
      summarise(incidence = sum(incidence))
    final_size_matrix[k, j] <- sum(output_incidence$incidence)
    output_matrix[k, j, ] <- output_incidence$incidence
  }
  print(k)
}

plot(R0_scan, apply(final_size_matrix, 1, mean), type = "l", ylim = c(0, 86))

## Particle filtering
R0_scan <- c(0.75, 1, 1.25, 1.5, 2, 3, 4, 5)
particles <- 50
seed <- rpois(particles, 1000000)
steps <- 1 / dt
loglikelihood_matrix <- array(data = NA, dim = c(length(R0_scan), particles, length(observed_data)))
particles_kept_matrix <- array(data = NA, dim = c(length(R0_scan), particles, length(observed_data)))
weights_matrix <- array(data = NA, dim = c(length(R0_scan), particles, length(observed_data)))
deaths_df <- array(data = NA, dim = c(length(R0_scan), particles, length(observed_data)))
deaths_df2 <- array(data = NA, dim = c(length(R0_scan), particles, length(observed_data)))
final_size_matrix <- array(data = NA, dim = c(length(R0_scan), particles, length(observed_data)))
output_matrix <- array(data = NA, dim = c(length(R0_scan), particles, length(observed_data)))

for (i in 1:length(R0_scan)) {
  storage_list <- vector(mode = "list", length = particles)
  beta_sim <- R0_scan[i] * gamma / N
  
  for (j in 1:length(observed_data)) {
    
    num_deaths_timestep_particle <- vector(mode = "double", length = particles)
    
    for (k in 1:particles) {
      
      if (j == 1) {
        temp <- run_simulation2(seed = seed[k], steps = 1 / dt, dt = dt, N = N, 
                                initial_infections = 1, death_obs_prop = 1, 
                                beta = beta_sim, past_length = past_length, 
                                past_weightings_vector = past_weightings_vector,
                                initial_run = TRUE, overall_run_length = 505)
        storage_list[[k]]$output <- temp$result
        
      } else {
        temp <- run_simulation2(seed = seed[k], steps = (j / dt), dt = dt, N = N, 
                                initial_infections = 1, death_obs_prop = 1, 
                                beta = beta_sim, past_length = past_length, 
                                past_weightings_vector = past_weightings_vector,
                                initial_run = FALSE, overall_run_length = NA,
                                lagged_I_input = storage_list[[k]]$lagged_I,
                                state = storage_list[[k]]$state)
        storage_list[[k]]$output <- rbind(storage_list[[k]]$output, 
                                          temp$result[(1 + nrow(temp$result) - 1/dt):nrow(temp$result), ])
      }
      
      storage_list[[k]]$lagged_I <- temp$lagged_I
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
    num_deaths_timestep_particle2[num_deaths_timestep_particle2 == 0] <- 0.01
    eval_loglik <- weight_particles(num_deaths_timestep_particle2, observed_data[j])
    
    weights_matrix[i, , j] <- eval_loglik$raw_weights
    resampled_weights <- sample(1:particles, prob = eval_loglik$normalised_weights, replace = TRUE)
    loglikelihood_matrix[i, , j] <- eval_loglik$logliklihoods[resampled_weights]
    particles_kept_matrix[i, , j] <- resampled_weights
    storage_list <- storage_list[resampled_weights]
    deaths_df2[i, , j] <- num_deaths_timestep_particle[resampled_weights]
    
    # if (length(unique(resampled_weights)) == 1) {
    #   stop("particle collapse")
    # }
    
    print(c(j, k))
    
  }
  print(i)
}

colors37 <- c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b","#bd5975")
par(mfrow = c(2, 4), mar = c(2, 2, 2, 2))
for (k in 1:length(R0_scan)) {
  logliks <- apply(weights_matrix[k, , ], 2, function(x) {
    average_weight <- mean(x)
    log(average_weight)
  })
  loglik <- sum(logliks)
  
  for (i in 1:particles) {
    if (i == 1) {
      plot(deaths_df2[k, i, ], type = "l", col = adjustcolor(colors37[k], alpha.f = 0.1),
           ylim = c(0, max(c(observed_data, deaths_df2[k, , ]))),
           main = paste0("R0 = ", R0_scan[k], ", loglik = ", round(loglik)),
           ylab = "", xlab = "")
    } else {
      lines(deaths_df2[k, i, ], type = "l", col = adjustcolor(colors37[k], alpha.f = 0.1))
    }
  }
  points(observed_data, pch = 20, col = "black", cex = 1)
}





# ## Particle filtering
# R0_scan <- c(2, 3) # c(0.75, 1, 1.25, 1.5, 2, 3, 5)
# particles <- 50
# seed <- rpois(particles, 1000000)
# steps <- 1 / dt
# storage_list <- vector(mode = "list", length = particles)
# loglikelihood_matrix <- matrix(nrow = particles, ncol = length(observed_data))
# particles_kept_matrix <- matrix(nrow = particles, ncol = length(observed_data))
# weights_matrix <- matrix(nrow = particles, ncol = length(observed_data))
# deaths_df <- matrix(nrow = particles, ncol = length(observed_data))
# deaths_df2 <- matrix(nrow = particles, ncol = length(observed_data))
# final_size_matrix <- matrix(NA, nrow = length(R0_scan), ncol = particles)
# output_matrix <- array(data = NA, dim = c(length(R0_scan), particles, length(output_incidence$incidence)))
# 
# for (i in 1:length(R0_scan)) {
#   
#   beta_sim <- R0_scan[i] * gamma / N
#   
#   for (j in 1:length(observed_data)) {
#     
#     num_deaths_timestep_particle <- vector(mode = "double", length = particles)
#     
#     for (k in 1:particles) {
#       
#       if (j == 1) {
#         temp <- run_simulation2(seed = seed[k], steps = 1 / dt, dt = dt, N = N, 
#                                 initial_infections = 1, death_obs_prop = 1, 
#                                 beta = beta_sim, past_length = past_length, 
#                                 past_weightings_vector = past_weightings_vector,
#                                 initial_run = TRUE, overall_run_length = 505)
#         storage_list[[k]]$output <- temp$result
#         
#       } else {
#         temp <- run_simulation2(seed = seed[k], steps = (j / dt), dt = dt, N = N, 
#                                 initial_infections = 1, death_obs_prop = 1, 
#                                 beta = beta_sim, past_length = past_length, 
#                                 past_weightings_vector = past_weightings_vector,
#                                 initial_run = FALSE, overall_run_length = NA,
#                                 lagged_I_input = storage_list[[k]]$lagged_I,
#                                 state = storage_list[[k]]$state)
#         storage_list[[k]]$output <- rbind(storage_list[[k]]$output, 
#                                           temp$result[(1 + nrow(temp$result) - 1/dt):nrow(temp$result), ])
#       }
#       
#       storage_list[[k]]$lagged_I <- temp$lagged_I
#       storage_list[[k]]$state <- temp$state
#       
#       curr_timestep_Dobs <- max(temp$result$Dobs_count[(1 + nrow(temp$result) - 1/dt):nrow(temp$result)])
#       if (j == 1) {
#         prev_timestep_Dobs <- 0
#       } else {
#         prev_timestep_Dobs <- storage_list[[k]]$output$Dobs_count[(nrow(temp$result) - 1/dt)] # get the last timepoint of the previous
#       }
#       temp_num_deaths_timestep <- curr_timestep_Dobs - prev_timestep_Dobs
#       num_deaths_timestep_particle[k] <- temp_num_deaths_timestep
#       deaths_df[k, j] <- temp_num_deaths_timestep
#       
#     }
#     
#     num_deaths_timestep_particle2 <- deaths_df[, j]
#     num_deaths_timestep_particle2[num_deaths_timestep_particle2 == 0] <- 0.01
#     eval_loglik <- weight_particles(num_deaths_timestep_particle2, observed_data[j])
#     
#     weights_matrix[, j] <- eval_loglik$raw_weights
#     resampled_weights <- sample(1:particles, prob = eval_loglik$normalised_weights, replace = TRUE)
#     loglikelihood_matrix[, j] <- eval_loglik$logliklihoods[resampled_weights]
#     particles_kept_matrix[, j] <- resampled_weights
#     storage_list <- storage_list[resampled_weights]
#     deaths_df2[, j] <- num_deaths_timestep_particle[resampled_weights]
#     
#     if (length(unique(resampled_weights)) == 1) {
#       stop("particle collapse")
#     }
#     
#     print(c(j, k))
# 
#   }
#   print(i)
# }

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



