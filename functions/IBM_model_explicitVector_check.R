run_simulation_vector <- function(seed, steps, dt, N, initial_infections, death_obs_prop, 
                                  beta_HV,
                                  R0 = R0,
                                  EIP_mean = EIP_mean,
                                  NHP_death_rate = NHP_death_rate,
                                  initial_run, overall_run_length,
                                  importation_rate,
                                  EIP_gamma_shape,  # Gamma shape for extrinsic incubation period (EIP)
                                  EIP_gamma_rate,   # Gamma rate for extrinsic incubation period (EIP)
                                  latent_period_gamma_shape, 
                                  latent_period_gamma_rate,
                                  infectious_period_gamma_shape, 
                                  infectious_period_gamma_rate,
                                  death_observation_gamma_shape, 
                                  death_observation_gamma_rate,
                                  v_population,
                                  vector_mortality_rate,  # per-timestep mortality rate for mosquitoes
                                  state) {
  
  ## Setting the seed
  set.seed(seed)
  
  #############################
  ## HOST (MONKEY) VARIABLES & PROCESSES
  #############################
  
  ## Define host disease states: "S", "E", "I", "D", "D_unobs", "Dobs"
  health <- CategoricalVariable$new(
    categories = c("S", "E", "I", "D", "D_unobs", "Dobs"),
    initial_values = c(rep("S", N - initial_infections), rep("E", initial_infections))
  )

  #############################
  ## VECTOR (MOSQUITO) VARIABLES & PROCESSES
  #############################
  
  # Create an individual-based variable for mosquitoes.
  # Total population is fixed at 10,000.
  v_health <- CategoricalVariable$new(
    categories = c("V_S", "V_E", "V_I"),
    initial_values = rep("V_S", v_population)
  )
  
  #############################
  ## VECTOR PROCESSES
  #############################
  
  ## Vector Exposure Process (Host -> Vector)
  # Susceptible mosquitoes (V_S) become exposed (V_E) based on the force of infection from infectious monkeys.
  v_exposure_process <- function(t) {
    V_S_index <- v_health$get_index_of("V_S")
    # Calculate force on mosquitoes from infectious monkeys:
    # beta <- (R0 * (1 / NHP_dur_inf_mean) * vector_mortality_rate) / (beta_fixed * (v_population / N_hosts) * exp(-vector_mortality_rate * EIP_mean))
    N_hosts <- N # health$get_size_of(c("S", "E", "I")) # 
    I_monkeys <- health$get_index_of("I")
    I_count <- I_monkeys$size()
    lambda_V <- ifelse(N_hosts == 0, 0, beta_HV * (I_count / N_hosts))
    p_inf_vector <- 1 - exp(-lambda_V * dt)
    
    # Vectorized sampling: select a set of susceptible mosquitoes to become exposed.
    new_exposed <- V_S_index$sample(rate = p_inf_vector)
    new_exposed_size <- new_exposed$size()
    if (new_exposed_size > 0) {
      v_health$queue_update(value = "V_E", index = new_exposed)
    }
  }
  
  ## Vector Progression Process: E -> I
  # Create a progression event for mosquitoes transitioning from V_E to V_I.
  v_progression_event <- TargetedEvent$new(population_size = v_population)
  v_progression_event$add_listener(function(t, target) {
    v_health$queue_update("V_I", target)
  })
  v_exposed_progression_process <- function(t) {
    V_E <- v_health$get_index_of("V_E")
    v_exposed_infectious_already_scheduled <- v_progression_event$get_scheduled()
    V_E$and(v_exposed_infectious_already_scheduled$not(inplace = TRUE))
    delay_steps <- round(rgamma(n = V_E$size(), 
                                shape = EIP_gamma_shape, 
                                rate = EIP_gamma_rate) / dt)
    v_progression_event$schedule(target = V_E, delay = round(delay_steps))
  }
  
  ## Vector Mortality Process
  # At each timestep, each mosquito has a probability (vector_mortality_rate * dt) of dying. When a mosquito dies, it is reset to susceptible state ("V_S").
  v_mortality_process <- function(t) {
    all_v <- v_health$get_index_of(c("V_S", "V_E", "V_I"))  # All mosquitoes
    p_death_vector <- 1 - exp(-vector_mortality_rate * dt)
    new_deaths <- all_v$sample(rate = p_death_vector)
    new_deaths_size <- new_deaths$size()
    if (new_deaths_size > 0) {
      v_health$queue_update(value = "V_S", index = new_deaths)
    }
    vector_render$render('D_v_new', new_deaths_size, t)
  }
  
  #############################
  ## HOST EXPOSURE PROCESS (Vector -> Host)
  #############################
  
  # Susceptible monkeys become exposed by bites from infectious mosquitoes.
  host_exposure_process <- function(t) {
    V_I_count <- v_health$get_index_of("V_I")$size()
    N_hosts <- health$get_size_of(c("S", "E", "I")) # N 
    beta_VH <- (R0 * vector_mortality_rate * NHP_death_rate) / ((v_population / N) * exp(- vector_mortality_rate * EIP_mean) * beta_HV)
    lambda_M <- ifelse(N_hosts == 0, 0, (v_population / N) * beta_VH * (V_I_count / v_population))
    p_inf_host <- 1 - exp(-lambda_M * dt)
    S_all <- health$get_index_of("S")
    local_infections <- S_all$copy()
    local_infections$sample(rate = p_inf_host)
    
    ## Handle importations as before:
    num_imports <- rpois(1, importation_rate * dt)
    if (num_imports > 0) {
      import_infections <- S_all$copy()
      import_infections$set_difference(local_infections)
      if (import_infections$size() > 0) {
        num_to_import <- min(num_imports, import_infections$size())
        if (is.na(num_to_import)) { num_to_import <- 0 }
        health_render$render('num_to_import', num_to_import, t)
        import_infections$choose(num_to_import)
      } else {
        health_render$render('num_to_import', 0, t) ## check this
      }
      newly_infected <- local_infections
      newly_infected$or(import_infections)
      health$queue_update(value = "E", index = newly_infected)
    } else {
      num_imports <- 0
      health_render$render('num_to_import', num_imports, t)
      health$queue_update(value = "E", index = local_infections)
    }
  }
  
  #############################
  ## HOST PROCESSES (Progression, Death, Observation)
  #############################
  
  ## Host Progression Process: E -> I
  exposed_infectious_event <- TargetedEvent$new(population_size = N)
  exposed_infectious_event$add_listener(function(t, target) {
    health$queue_update("I", target)
  })
  exposed_progression_process <- function(t) {
    E <- health$get_index_of("E")
    exposed_infectious_already_scheduled <- exposed_infectious_event$get_scheduled()
    E$and(exposed_infectious_already_scheduled$not(inplace = TRUE))
    infection_times <- rgamma(n = E$size(), 
                              shape = latent_period_gamma_shape, 
                              rate = latent_period_gamma_rate) / dt
    exposed_infection_progression_times <- infection_times
    exposed_infectious_event$schedule(target = E, delay = round(exposed_infection_progression_times))
  }
  
  ## Host Death Process: I -> D
  infectious_death_event <- TargetedEvent$new(population_size = N)
  infectious_death_event$add_listener(function(t, target) {
    health$queue_update("D", target)
  })
  infectious_death_process <- function(t) {
    I <- health$get_index_of("I")
    already_scheduled <- infectious_death_event$get_scheduled()
    I$and(already_scheduled$not(inplace = TRUE))
    death_times <- round((rgamma(n = I$size(), 
                                 shape = infectious_period_gamma_shape, 
                                 rate = infectious_period_gamma_rate)) / dt)
    infectious_death_event$schedule(target = I, delay = death_times)
  }
  
  ## Host Observation Process: D -> (Dobs, D_unobs)
  observation_event <- TargetedEvent$new(population_size = N)
  observation_event$add_listener(function(t, target) {
    health$queue_update("Dobs", target)
  })
  unobserved_event <- TargetedEvent$new(population_size = N)
  unobserved_event$add_listener(function(t, target) {
    health$queue_update("D_unobs", target)
  })
  observation_process <- function(t) {
    D <- health$get_index_of("D")
    Dobs_already_scheduled <- observation_event$get_scheduled()
    Dunobs_already_scheduled <- unobserved_event$get_scheduled()
    D$and(Dobs_already_scheduled$not(inplace = TRUE))
    D$and(Dunobs_already_scheduled$not(inplace = TRUE))
    unscheduled_to_Dobs <- D$copy()
    unscheduled_to_Dobs <- unscheduled_to_Dobs$sample(death_obs_prop)
    unscheduled_to_D_unobs <- D$and(unscheduled_to_Dobs$not(inplace = FALSE))
    observation_times <- round((rgamma(n = unscheduled_to_Dobs$size(), 
                                       shape = death_observation_gamma_shape, 
                                       rate = death_observation_gamma_rate)) / dt)
    observation_event$schedule(target = unscheduled_to_Dobs, delay = observation_times)
    unobserved_event$schedule(target = unscheduled_to_D_unobs, delay = 1)
    health_render$render('D_obs_new', unscheduled_to_Dobs$size(), t) ## this is rendering all the individuals who were SCHEDULED to go
                                                                     ## to D_obs in that timestep, not all those who actually DID
  }
  
  #############################
  ## Rendering
  #############################
  health_render <- Render$new(timesteps = steps)
  health_render_process <- categorical_count_renderer_process(
    renderer = health_render,
    variable = health,
    categories = c("S", "E", "I", "D_unobs", "Dobs")
  )
  vector_render <- Render$new(timesteps = steps)
  vector_render_process <- categorical_count_renderer_process(
    renderer = vector_render,
    variable = v_health,
    categories = c("V_S", "V_E", "V_I")
  )
  
  
  #############################
  ## Run the simulation loop
  #############################
  final_state <- simulation_loop(
    variables = list(health, v_health),
    events = list(exposed_infectious_event, observation_event, unobserved_event, infectious_death_event, v_progression_event),
    processes = list(v_exposure_process, v_exposed_progression_process, v_mortality_process, 
                     host_exposure_process, exposed_progression_process, infectious_death_process, observation_process, 
                     health_render_process, vector_render_process),
    timesteps = steps,
    state = state
  )
  
  return(list(result = health_render$to_dataframe(), final_vector_state = vector_render$to_dataframe(), state = final_state))
}
