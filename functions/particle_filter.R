weight_particles <- function(particle_values, observed) {
  likelihoods <- dpois(x = observed, lambda = particle_values)
  loglikelihoods <- dpois(x = observed, lambda = particle_values, log = TRUE)
  normalised_weights <- likelihoods / sum(likelihoods)
  return(list(normalised_weights = normalised_weights,
              raw_weights = likelihoods,
              logliklihoods = loglikelihoods))
}

evaluate_likelihood <- function(observed_data, seed, steps, dt, N, initial_infections, death_obs_prop, 
                                beta, past_length, past_weightings_vector, ...) {
  
  ## Running the model
  temp <- run_simulation2(seed = seed, steps = steps, dt = dt, N = N, 
                          initial_infections = initial_infections, death_obs_prop = death_obs_prop, 
                          beta = beta, past_length = past_length, past_weightings_vector = past_weightings_vector)
  
  ## Extracting daily incidence
  temp$result$time <- floor(temp$result$timestep * dt)
  temp_incidence <- temp$result %>%
    mutate(incidence = c(0, diff(Dobs_count))) %>%
    group_by(time) %>%
    summarise(daily_incidence = sum(incidence))
  
  ## Comparing 
  temp_incidence$daily_incidence[temp_incidence$daily_incidence == 0] <- 0.001
  loglikelihood <- sum(dpois(x = observed_data, lambda = temp_incidence$daily_incidence, log = TRUE))
  
  ## Returning output
  return(list(observed_data = observed_data,
              model_output = temp_incidence,
              loglikelihood = loglikelihood))
  
}
