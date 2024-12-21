# load required libraries
library(individual); library(dplyr); library(tidyverse); library(truncnorm); library(tmvtnorm)
library(foreach); library(doParallel); library(tictoc); library(parallel)

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

## Parameter scan for diff R0 values
iterations <- 10
cores <- 10
R0_scan <- c(1.5, 2, 3, 4, 5, 6)
start_date_scan <- c(1, 3, 5, 7, 9)
particles <- 250

loglikelihood_matrix <- array(data = NA, dim = c(iterations, length(R0_scan), length(start_date_scan)))
final_size_matrix <- array(data = NA, dim = c(iterations, length(R0_scan), length(start_date_scan)))
output_matrix <- array(data = NA, dim = c(iterations, length(R0_scan), length(start_date_scan), length(observed_data)))

for (i in 1:length(R0_scan)) {
  
  for (j in 1:length(start_date_scan)) {
    
    start_date <- start_date_scan[j]
    data <- observed_incidence[start_date:nrow(observed_incidence), ]
    steps <- nrow(data) / dt
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
    
    cl <- makeCluster(cores)
    clusterExport(cl, varlist = c("r_loglike", "weight_particles", "data", "misc", "run_simulation2"))
    clusterEvalQ(cl, {
      library(individual)
    })
    
    R0_temp <- c("R0" = R0_scan[i])
    clusterExport(cl, varlist = c("R0_temp"))
    result_parallel <- parLapply(cl, 1:iterations, function(i) {
      temp <- r_loglike(R0_temp, data, misc)
      return(temp)
    })
    
    parallel::stopCluster(cl)
    
    for (k in 1:iterations) {
      output_matrix[k, i, j, ] <- c(rep(0, start_date - 1), result_parallel[[k]]$deaths_trajectory)
      final_size_matrix[k, i, j] <- sum(result_parallel[[k]]$deaths_trajectory)
      loglikelihood_matrix[k, i, j] <- result_parallel[[k]]$loglikelihood
    }
    
    print(c("j = ", j))
    
  }

  print(i)

}



colors37 <- c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b","#bd5975")
par(mfrow = c(3, 4), mar = c(2, 2, 2, 2))
for (i in 1:length(R0_scan)) {
  for (j in 1:iterations) {
    if (j == 1) {
      plot(output_matrix[j, i, ], type = "l", col = adjustcolor(colors37[i], alpha.f = 0.2),
           ylim = c(0, max(c(observed_data, output_matrix[, i, ]))),
           main = paste0("R0 = ", R0_scan[i], ", loglik = ", 
                         round(apply(loglikelihood_matrix, 2, mean)[i], 2)),
           ylab = "", xlab = "")
    } else {
      lines(output_matrix[j, i, ], type = "l", col = adjustcolor(colors37[i], alpha.f = 0.2))
    }
  }
  points(observed_data, pch = 20, col = "black", cex = 1)
}

