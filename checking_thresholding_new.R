# load required libraries
library(individual); library(dplyr); library(tidyverse); library(doParallel); library(tictoc); library(parallel); library(profvis);

## Sourcing functions
source("functions/IBM_model.R")

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

NHP_death_rate <- 1 / (infectious_period_gamma_shape / infectious_period_gamma_rate)
N_scan <- c(50, 500, 5000)
R0_scan <- c(seq(0.6, 2, 0.2), seq(3, 6, 1))
iterations <- 25

final_size_matrix <- array(data = NA, 
                           dim = c(length(R0_scan), 
                                   length(N_scan),
                                   iterations))
for (i in 1:length(R0_scan)) {
  for (j in 1:length(N_scan)) {
    
    beta <- R0_scan * NHP_death_rate / N_scan[j]
    
    for (k in 1:iterations) {
      
      x <- run_simulation2(seed = rnbinom(n = 1, size = 3, mu = 1e6), 
                           steps = 8000, 
                           dt = 0.1, 
                           N = N_scan[j], 
                           initial_infections = 1, 
                           death_obs_prop = 1, 
                           beta = beta[i], 
                           initial_run = FALSE, 
                           overall_run_length = FALSE, 
                           transmission_type = "density_dependent",
                           importation_rate = 0,
                           EIP_gamma_shape = EIP_gamma_shape,
                           EIP_gamma_rate = EIP_gamma_rate,
                           latent_period_gamma_shape = latent_period_gamma_shape, 
                           latent_period_gamma_rate = latent_period_gamma_rate,
                           infectious_period_gamma_shape = infectious_period_gamma_shape, 
                           infectious_period_gamma_rate = infectious_period_gamma_rate,
                           death_observation_gamma_shape = 1, 
                           death_observation_gamma_rate = death_observation_gamma_rate,
                           state = NULL) 
      # final_size <- c(final_size, max(x$result$Dobs_count + x$result$D_unobs_count) / N) ## not currently outputting D_unobs but get round this by setting prob_observe to 1
      # print(max(x$result$Dobs_count + x$result$D_unobs_count) / N) ## not currently outputting D_unobs but get round this by setting prob_observe to 1
      final_size_matrix[i, j, k] <- max(x$result$Dobs_count) / N_scan[j]
      # print(max(x$result$Dobs_count) / N)
      # if (i == 1) {
      #   # plot(x$result$timestep, x$result$Dobs_count + x$result$D_unobs_count, type = "l", ylim = c(0, N)) ## not currently outputting D_unobs but get round this by setting prob_observe to 1
      #   plot(x$result$timestep, x$result$Dobs_count, type = "l", ylim = c(0, N))
      # } else {
      #   par(new=TRUE)
      #   # plot(x$result$timestep, x$result$Dobs_count + x$result$D_unobs_count, type = "l", ylim = c(0, N)) ## not currently outputting D_unobs but get round this by setting prob_observe to 1
      #   plot(x$result$timestep, x$result$Dobs_count, type = "l", ylim = c(0, N))
      # }
      # print(paste0("k = ", k))
    }
  }
}

## Overall loglikelihood
final_size <- apply(final_size_matrix, c(1, 2), min)
dimnames(final_size) <- list(R0 = R0_scan, N = N_scan)
df_long <- as.data.frame.table(final_size, responseName = "finalsize")
head(df_long)
ggplot(df_long, aes(x = R0, y = finalsize, col = N)) +
  geom_point() +
  theme_bw()
