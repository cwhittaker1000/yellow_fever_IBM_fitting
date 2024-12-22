## Particle weighting scheme
weight_particles <- function(particle_values, observed) {
  likelihoods <- dpois(x = observed, lambda = particle_values)
  normalised_weights <- likelihoods / sum(likelihoods)
  return(list(normalised_weights = normalised_weights,
              raw_weights = likelihoods))
}

## Running the particle filter
r_loglike <- function(params, data, misc) {
  
  # Set.seed
  misc$seed <- rpois(misc$particles, 1000000)
  
  ## Converting R0 to model input beta
  beta_sim <- params["R0"] * misc$gamma / misc$N
  
  ## Creating storage for model outputs
  deaths_df <- array(data = NA, dim = c(misc$particles, length(data$time)))
  deaths_df2 <- array(data = NA, dim = c(misc$particles, length(data$time)))
  loglikelihood <- vector(mode = "numeric", length = length(data$time))
  
  storage_list <- vector(mode = "list", length = misc$particles)
  num_rows_output <- length(data$time) * (1 / misc$dt)
  num_cols_output <- 3
  for (j in seq_len(misc$particles)) {
    storage_list[[j]] <- list(output = matrix(NA_real_, nrow = num_rows_output, ncol = num_cols_output), 
                              state = NULL)
  }
  
  ## Looping over timepoints
  for (i in 1:length(data$time)) {
    
    num_deaths_timestep_particle <- vector(mode = "double", length = misc$particles)
    
    ## Looping over particles
    for (j in 1:misc$particles) {
      
      ## If it's the first timestep, don't reload the state
      if (i == 1) {
        temp <- run_simulation2(seed = misc$seed[j], steps = 1 / misc$dt, dt = misc$dt, N = misc$N, 
                                initial_infections = misc$initial_infections, death_obs_prop = misc$death_obs_prop,
                                beta = beta_sim,
                                initial_run = TRUE, overall_run_length = misc$overall_run_length,
                                latent_period_gamma_shape = misc$latent_period_gamma_shape, 
                                EIP_gamma_shape = misc$EIP_gamma_shape,
                                EIP_gamma_rate = misc$EIP_gamma_rate, 
                                latent_period_gamma_rate = misc$latent_period_gamma_rate,
                                infectious_period_gamma_shape = misc$infectious_period_gamma_shape, 
                                infectious_period_gamma_rate = misc$infectious_period_gamma_rate,
                                death_observation_gamma_shape = 1, 
                                death_observation_gamma_rate = misc$death_observation_gamma_rate,
                                state = NULL)
        storage_list[[j]]$output <- temp$result
        
      ## If we've already done any simulating, reload the appropriate state
      } else {
        temp <- run_simulation2(seed = misc$seed[j], steps = (i / misc$dt), dt = misc$dt, N = misc$N, 
                                initial_infections = misc$initial_infections, death_obs_prop = misc$death_obs_prop, 
                                beta = beta_sim, 
                                initial_run = FALSE, overall_run_length = NA,
                                latent_period_gamma_shape = misc$latent_period_gamma_shape, 
                                EIP_gamma_shape = misc$EIP_gamma_shape,
                                EIP_gamma_rate = misc$EIP_gamma_rate, 
                                latent_period_gamma_rate = misc$latent_period_gamma_rate,
                                infectious_period_gamma_shape = misc$infectious_period_gamma_shape, 
                                infectious_period_gamma_rate = misc$infectious_period_gamma_rate,
                                death_observation_gamma_shape = 1, 
                                death_observation_gamma_rate = misc$death_observation_gamma_rate,
                                state = storage_list[[j]]$state)
        
        start_row <- (i - 1) * (1 / misc$dt) + 1
        end_row   <- i * (1 / misc$dt)
        storage_list[[j]]$output[start_row:end_row, ] <- temp$result[(1 + nrow(temp$result) - (1 / misc$dt)):nrow(temp$result), ]
        
      }
      storage_list[[j]]$state <- temp$state
      
      # Calculating the number of deaths that occur in that timestep
      curr_timestep_Dobs <- max(temp$result$Dobs_count[(1 + nrow(temp$result) - 1/misc$dt):nrow(temp$result)])
      if (i == 1) {
        prev_timestep_Dobs <- 0
      } else {
        prev_timestep_Dobs <- storage_list[[j]]$output$Dobs_count[(nrow(temp$result) - 1/misc$dt)] # get the last timepoint of the previous
      }
      temp_num_deaths_timestep <- curr_timestep_Dobs - prev_timestep_Dobs
      num_deaths_timestep_particle[j] <- temp_num_deaths_timestep
      deaths_df[j, i] <- temp_num_deaths_timestep
      
    }
    
    ## Generating weights for each of the particles
    num_deaths_timestep_particle2 <- deaths_df[, i]
    to_replace <- sum(num_deaths_timestep_particle2 == 0)
    num_deaths_timestep_particle2[num_deaths_timestep_particle2 == 0] <- rexp(to_replace, rate = 1/1e-5) 
    eval_loglik <- weight_particles(num_deaths_timestep_particle2, data$daily_incidence[i])
    
    ## Resampling particles using the weights
    resampled_indices <- sample(1:misc$particles, prob = eval_loglik$normalised_weights, replace = TRUE)
    storage_list <- storage_list[resampled_indices]
    deaths_df2[, i] <- num_deaths_timestep_particle[resampled_indices]
    loglikelihood[i] <- log(mean(eval_loglik$raw_weights))
    
  }
  
  ## One last round of resampling
  final_sampling_index <- sample(x = 1:misc$particles, size = 1, prob = eval_loglik$normalised_weights)
  deaths_trajectory <- deaths_df2[final_sampling_index, ]

  ## Returning output
  return(list(deaths_trajectory = deaths_trajectory, loglikelihood = sum(loglikelihood)))
  
}

# logprior function
r_logprior <- function(params, misc) {
  
  # extract parameter values
  param_values <- as.numeric(params)
  param_values <- param_values[!misc$fixed]
  
  # extract mean, sd, lower, and upper values for each parameter
  param_means <- misc$mean[!misc$fixed]
  param_sds <- misc$sd[!misc$fixed]
  param_lowers <- misc$lower[!misc$fixed]
  param_uppers <- misc$upper[!misc$fixed]
  
  # check if length of parameters matches
  if (length(param_values) != length(param_means) || length(param_values) != length(param_sds) ||
      length(param_values) != length(param_lowers) || length(param_values) != length(param_uppers)) {
    stop("Length of parameter values and values in misc list must match.")
  }
  if (sum(!misc$fixed) != length(param_means)) {
    stop("Length of inferred parameters does not equal number of prior mean distribution specified")
  }
  
  # calculate log-prior for each parameter
  log_priors <- sapply(seq_along(param_values), function(i) {
    log(dtruncnorm(x = param_values[i], mean = param_means[i], 
                   sd = param_sds[i], a = param_lowers[i], b = param_uppers[i]))
  })
  
  # sum of log-priors
  total_log_prior <- sum(log_priors)
  
  return(total_log_prior)
}

# Log-posterior function
r_logposterior <- function(params, data, misc) {
  logprior <- r_logprior(params, misc)
  loglikelihood <- r_loglike(params, data, misc)
  return(list(posterior = unname(logprior + loglikelihood$loglikelihood),
              trajectory = loglikelihood$deaths_trajectory))
}

# Function up Updating Proposals Based on Johnstone-Chang Algorithm updating the scaling factor
jc_prop_update <- function(accepted, i, current_sf, previous_mu, current_parameters,
                           current_covariance_matrix, required_acceptance_ratio) {
  
  cooldown <- (i + 1)^-0.6
  new_covariance_matrix <- ((1 - cooldown) * current_covariance_matrix) +
    (cooldown * (t(current_parameters - previous_mu) %*% (current_parameters - previous_mu)))
  new_mu <- ((1 - cooldown) * previous_mu) + (cooldown * current_parameters)
  log_new_scaling_factor <- log(current_sf) + cooldown * (accepted - required_acceptance_ratio)
  new_scaling_factor = exp(log_new_scaling_factor);
  
  return(list("covariance_matrix" = new_covariance_matrix,
              "mu" = new_mu,
              "scaling_factor" = new_scaling_factor))
}

## Proposal function (assuming parameters on normal scale)
proposal_function <- function(current_parameters, lower, upper, covariance_matrix, scaling_factor) {
  number_parameters <- length(current_parameters)
  names_parameters <- names(current_parameters)
  if (all(eigen(scaling_factor * covariance_matrix)$values > 0)) {
    proposed_parameter_values <- rtmvnorm(1,
                                          mean = current_parameters,
                                          sigma = scaling_factor * covariance_matrix,
                                          lower = lower,
                                          upper = upper,
                                          algorithm = "rejection")  
  } else {
    print(current_parameters)
    print(scaling_factor)
    print(covariance_matrix)
    return("sigma is not positive definite")
  }
  proposed_parameter_values <- as.vector(proposed_parameter_values)
  names(proposed_parameter_values) <- names_parameters
  return(proposed_parameter_values)
}  

### Run MCMC
run_MCMC <- function(initial_values,
                     misc,
                     data, 
                     iterations,
                     burnin,
                     start_covariance_adaptation,
                     sd_proposals,
                     scaling_factor = 1,
                     target_acceptance = 0.2,
                     refresh = 5,
                     temp_save = FALSE,
                     final_save = FALSE,
                     save_directory = getwd(),
                     filename = "",
                     output = TRUE) {
  
  ## Generating Initial Diagonal Covariance Matrix
  num_params <- sum(!misc$fixed)
  sigma <- matrix(0, nrow = num_params, ncol = num_params)
  diag(sigma) <- sd_proposals
  MCMC_output <- matrix(nrow = iterations + 1, ncol = length(misc$fixed)) 
  
  ## Initialising Storage for Outputs
  colnames(MCMC_output) <- names(initial_values)
  MCMC_output[1, ] <- unlist(initial_values)
  acceptance_tracker <- rep(NA, iterations + 1)
  acceptance_ratio_tracker <- rep(NA, iterations + 1)
  posterior <- rep(NA, iterations + 1)
  trajectory_storage <- matrix(nrow = iterations, ncol = length(data$daily_incidence)) 
  
  # Calculating the Posterior Probability for the Initial Values
  current_posterior_output <- r_logposterior(params = MCMC_output[1, ], data = data, misc = misc)
  current_posterior <- current_posterior_output$posterior
    
  # Running the Actual MCMC
  for (i in 1:iterations){
    
    ## Proposing new parameters and accept/reject based on relative posterior density
    proposed_params <- MCMC_output[i, ]
    param_draws <-  proposal_function(MCMC_output[i, !misc$fixed], 
                                      misc$lower[!misc$fixed], 
                                      misc$upper[!misc$fixed], sigma, scaling_factor)
    proposed_params[!misc$fixed] <- param_draws
    proposed_posterior_output <- r_logposterior(params = proposed_params, data = data, misc = misc) 
    proposed_posterior <- proposed_posterior_output$posterior
    likelihood_ratio <- exp(proposed_posterior - current_posterior)
    
    if (runif(1) < likelihood_ratio) {
      MCMC_output[i + 1, ] <- proposed_params
      acceptance_tracker[i] <- 1
      posterior[i] <- proposed_posterior
      current_posterior <- proposed_posterior
      current_posterior_output <- proposed_posterior_output
    } else {
      MCMC_output[i + 1, ] <- MCMC_output[i, ]
      acceptance_tracker[i] <- 0
      posterior[i] <- current_posterior_output$posterior
    }
    trajectory_storage[i, ] <- current_posterior_output$trajectory
    
    ## Covariance adaptation using the Johnstone Chang Algorithm
    if (i >= start_covariance_adaptation) {
      timing_cov <- i - start_covariance_adaptation + 1 # iteration relative to when covariance adaptation started
      if (i == start_covariance_adaptation) {
        if(dim(MCMC_output)[2] == 1) {
          previous_mu <- matrix(mean(MCMC_output[1:(start_covariance_adaptation + 1), !misc$fixed]), nrow = 1) 
        } else {
          previous_mu <- matrix(colMeans(MCMC_output[1:(start_covariance_adaptation + 1), !misc$fixed]), nrow = 1) 
        }
        current_parameters <- matrix(MCMC_output[start_covariance_adaptation + 1, !misc$fixed], nrow = 1) 
        temp <- jc_prop_update(accepted = acceptance_tracker[i], 
                               i = timing_cov, 
                               current_sf = scaling_factor, 
                               previous_mu = previous_mu,
                               current_parameters = current_parameters, 
                               current_covariance_matrix = sigma, 
                               required_acceptance_ratio = target_acceptance)
        scaling_factor <- temp$scaling_factor
        sigma <- temp$covariance_matrix
        previous_mu <- temp$mu
      } else {
        current_parameters <- matrix(MCMC_output[i + 1, !misc$fixed], nrow = 1)
        temp <- jc_prop_update(accepted = acceptance_tracker[i], 
                               i = timing_cov, 
                               current_sf = scaling_factor, 
                               previous_mu = previous_mu,
                               current_parameters = current_parameters, 
                               current_covariance_matrix = sigma, 
                               required_acceptance_ratio = target_acceptance)
        scaling_factor <- temp$scaling_factor
        sigma <- temp$covariance_matrix
        previous_mu <- temp$mu
      }
    }
    
    ## Tracking MCMC-related diagnostics in real-time
    acceptance_ratio_tracker[i] <- sum(acceptance_tracker, na.rm = TRUE)/i
    if(i %% refresh == 0 & output == TRUE) {
      print(c("The iteration number is", i))
      print(c("The acceptance ratio is", acceptance_ratio_tracker[i]))
      print(c("Total acceptances is", sum(acceptance_tracker, na.rm = TRUE)))
      print(c("Scaling factor is ", scaling_factor))
      if (temp_save) {
        saveRDS(object = list(MCMC_output = MCMC_output,trajectory_storage = trajectory_storage,
                              posterior = posterior,
                              acceptance_tracker = acceptance_tracker), file = paste0(save_directory, "/temp_MCMC_output.rds"))
      }
    }
  }
  
  ## Summarising and returning outputs
  # param_ess <- apply(MCMC_output[burnin:iterations, ], 2, coda::effectiveSize)
  # param_mean <- apply(MCMC_output[burnin:iterations, ], 2, mean)
  # param_median <- apply(MCMC_output[burnin:iterations, ], 2, median)
  # param_lower <- apply(MCMC_output[burnin:iterations, ], 2, quantile, 0.025)
  # param_upper <- apply(MCMC_output[burnin:iterations, ], 2, quantile, 0.975)
  # summary_df <- data.frame(parameter = names(initial_values), mean = param_mean, median = param_median, 
  #                          lower = param_lower, upper = param_upper, ess = param_ess)
  
  list <- list()
  # list[["MCMC_summary"]] <- summary_df
  list[["MCMC_output"]] <- MCMC_output
  list[["acceptance_tracker"]] <- acceptance_tracker
  list[["posterior"]] <- posterior
  list[["final_covariance_matrix"]] <- sigma * scaling_factor
  if (final_save) {
    saveRDS(object = list(MCMC_output = MCMC_output, trajectory_storage = trajectory_storage,
                          posterior = posterior, acceptance_tracker = acceptance_tracker), file = paste0(save_directory, "/", filename, ".rds"))
  }
  return(list)
}

## still need to work on this
# run_MCMC_multichain_wrapper <- function(n_chains, 
#                                         initial_values,
#                                         misc,
#                                         data, 
#                                         iterations,
#                                         burnin,
#                                         start_covariance_adaptation,
#                                         sd_proposals,
#                                         scaling_factor = 1,
#                                         target_acceptance = 0.2,
#                                         refresh = 5,
#                                         temp_save = FALSE,
#                                         final_save = FALSE,
#                                         save_directory = getwd(),
#                                         filename = "",
#                                         output = TRUE) {
#   
#   results <- parallel::parLapply(NULL, 1:n_chains, function(chain_id) {
#     set.seed(chain_id)
#     run_MCMC(initial_values = initial_values,
#              misc = misc,
#              data = data, 
#              filename = filename,
#              iterations = iterations, burnin = burnin, start_covariance_adaptation = start_covariance_adaptation,
#              sd_proposals = sd_proposals_initial, scaling_factor = scaling_factor, 
#              target_acceptance = target_acceptance, refresh = refresh, 
#              temp_save = temp_save, final_save = final_save,
#              save_directory = save_directory, output = output)
#   })
#   output <- list(results = results, misc = misc)
#   return(output)
# }

# r_loglike_parallel <- function(params, data, misc, cores) {
#   
#   source("functions/model2.R")
#   
#   # Set up parallel backend
#   misc$seed <- rpois(misc$particles, 1000000)
#   num_cores <- cores
#   cl <- makeCluster(num_cores)
#   registerDoParallel(cl)
#   
#   # Number of particles
#   particles <- misc$particles
#   
#   # Initialize storage
#   storage_list <- vector(mode = "list", length = particles)
#   deaths_df <- array(data = NA, dim = c(particles, length(data$time)))
#   deaths_df2 <- array(data = NA, dim = c(particles, length(data$time)))
#   loglikelihood <- vector(mode = "numeric", length = length(data$time))
#   
#   # Convert R0 to beta
#   beta_sim <- params["R0"] * misc$gamma / misc$N
#   
#   # Time loop
#   for (i in 1:length(data$time)) {
#     
#     num_deaths_timestep_particle <- vector(mode = "double", length = misc$particles)
#     
#     # Parallel particle loop
#     results <- foreach(j = 1:particles, .export = "run_simulation2", .packages = c("individual")) %dopar% {
#       
#       # Initialize an empty list to store results for particle j
#       particle_result <- list()
#       
#       # If it's the first timestep, don't reload the state
#       if (i == 1) {
#         temp <- run_simulation2(
#           seed = misc$seed[j], 
#           steps = 1 / misc$dt, 
#           dt = misc$dt, 
#           N = misc$N, 
#           initial_infections = misc$initial_infections, 
#           death_obs_prop = misc$death_obs_prop,
#           beta = beta_sim,
#           initial_run = TRUE, 
#           overall_run_length = misc$overall_run_length,
#           latent_period_gamma_shape = misc$latent_period_gamma_shape, 
#           EIP_gamma_shape = misc$EIP_gamma_shape,
#           EIP_gamma_rate = misc$EIP_gamma_rate, 
#           latent_period_gamma_rate = misc$latent_period_gamma_rate,
#           infectious_period_gamma_shape = misc$infectious_period_gamma_shape, 
#           infectious_period_gamma_rate = misc$infectious_period_gamma_rate,
#           death_observation_gamma_shape = 1, 
#           death_observation_gamma_rate = misc$death_observation_gamma_rate,
#           state = NULL
#         )
#         output <- temp$result
#       } else {
#         temp <- run_simulation2(
#           seed = misc$seed[j], 
#           steps = i / misc$dt, 
#           dt = misc$dt, 
#           N = misc$N, 
#           initial_infections = misc$initial_infections, 
#           death_obs_prop = misc$death_obs_prop, 
#           beta = beta_sim, 
#           initial_run = FALSE, 
#           overall_run_length = NA,
#           latent_period_gamma_shape = misc$latent_period_gamma_shape, 
#           EIP_gamma_shape = misc$EIP_gamma_shape,
#           EIP_gamma_rate = misc$EIP_gamma_rate, 
#           latent_period_gamma_rate = misc$latent_period_gamma_rate,
#           infectious_period_gamma_shape = misc$infectious_period_gamma_shape, 
#           infectious_period_gamma_rate = misc$infectious_period_gamma_rate,
#           death_observation_gamma_shape = 1, 
#           death_observation_gamma_rate = misc$death_observation_gamma_rate,
#           state = storage_list[[j]]$state
#         )
#         start_index <- as.integer(1 + nrow(temp$result) - (1 / misc$dt))
#         output <- rbind(
#           storage_list[[j]]$output, 
#           temp$result[start_index:nrow(temp$result), ]
#         )
#       }
#       state <- temp$state
#       
#       # Calculating the number of deaths that occur in that timestep
#       curr_index <- as.integer(1 + nrow(temp$result) - (1 / misc$dt))
#       curr_timestep_Dobs <- max(temp$result$Dobs_count[curr_index:nrow(temp$result)])
#       if (i == 1) {
#         prev_timestep_Dobs <- 0
#       } else {
#         prev_index <- as.integer(nrow(temp$result) - (1 / misc$dt))
#         prev_timestep_Dobs <- storage_list[[j]]$output$Dobs_count[prev_index]
#       }
#       temp_num_deaths_timestep <- curr_timestep_Dobs - prev_timestep_Dobs
#       
#       # Store the results in the list
#       particle_result$output <- output
#       particle_result$state <- state
#       particle_result$temp_num_deaths_timestep <- temp_num_deaths_timestep
#       
#       # Return the particle's result
#       return(particle_result)
#     }
#     
#     # Collect the results from the parallel execution
#     for (j in 1:particles) {
#       storage_list[[j]]$output <- results[[j]]$output
#       storage_list[[j]]$state <- results[[j]]$state
#       num_deaths_timestep_particle[j] <- results[[j]]$temp_num_deaths_timestep
#       deaths_df[j, i] <- num_deaths_timestep_particle[j]
#     }
#     
#     # Generating weights for each of the particles
#     num_deaths_timestep_particle2 <- deaths_df[, i]
#     num_deaths_timestep_particle2[num_deaths_timestep_particle2 == 0] <- 0.01  # Optional adjustment
#     eval_loglik <- weight_particles(num_deaths_timestep_particle2, data$daily_incidence[i])
#     
#     # Resampling particles using the weights
#     resampled_indices <- sample(1:particles, prob = eval_loglik$normalised_weights, replace = TRUE)
#     storage_list <- storage_list[resampled_indices]
#     deaths_df2[, i] <- num_deaths_timestep_particle[resampled_indices]
#     loglikelihood[i] <- log(mean(eval_loglik$raw_weights))
#   }
#   
#   # One last round of resampling
#   final_sampling_index <- sample(x = 1:particles, size = 1, prob = eval_loglik$normalised_weights)
#   deaths_trajectory <- deaths_df2[final_sampling_index, ]
#   
#   # Stop the cluster after computation
#   stopCluster(cl)
#   
#   # Returning output
#   return(list(deaths_trajectory = deaths_trajectory, loglikelihood = sum(loglikelihood)))
# }
