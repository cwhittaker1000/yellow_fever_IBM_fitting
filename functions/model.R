### Individual model
run_simulation <- function(seed, steps, dt, N, initial_infections, death_obs_prop, 
                           beta, ...) {
  
  ## Setting the seed
  set.seed(seed)
  
  ## Define disease states
  health <- CategoricalVariable$new(categories = c("S", "E", "I", "D", "D_unobs", "Dobs"),
                                    initial_values = c(rep("S", N - initial_infections), rep("E", initial_infections)))
  
  ## Define exposure process
  exposure_process <- function(t){
    I <- health$get_size_of("I")
    foi <- beta * I
    S <- health$get_index_of("S")
    S$sample(rate = pexp(q = foi * dt))
    health$queue_update(value = "E",index = S)
  }
  
  ## Define infection event and process to schedule moves from E->I
  exposed_infectious_event <- TargetedEvent$new(population_size = N)
  exposed_infectious_event$add_listener(function(t, target) {
    health$queue_update("I", target)
  })
  exposed_death_event <- TargetedEvent$new(population_size = N)
  exposed_death_event$add_listener(function(t, target) {
    health$queue_update("D", target)
  })
  exposed_progression_process <- function(t){
    
    ## Getting index of individuals in D state
    E <- health$get_index_of("E")
    
    ## Getting individuals already scheduled
    exposed_infectious_already_scheduled <- exposed_infectious_event$get_scheduled()
    exposed_death_already_scheduled <- exposed_death_event$get_scheduled()
    
    ## Subsetting D inplace by those who aren't already scheduled
    E$and(exposed_infectious_already_scheduled$not(inplace = TRUE))
    E$and(exposed_death_already_scheduled$not(inplace = TRUE))
    
    unscheduled_to_I <- E$copy()
    unscheduled_to_I <- unscheduled_to_I$sample(0.5) ## arbitrarily saying 50% go on to die?? That doesn't seem right.
    unscheduled_to_D <- E$and(unscheduled_to_I$not(inplace = FALSE))
    
    infection_times <- round((rgamma(unscheduled_to_I$size(), 4, 0.33) + 1) / dt)
    exposed_infectious_event$schedule(target = unscheduled_to_I, delay = infection_times) 
    death_times <- round((rgamma(unscheduled_to_D$size(), 4, 1) + 1) / dt)
    exposed_death_event$schedule(target = unscheduled_to_D, delay = death_times)
    
  }
  
  ## Define death of I event and process to schedule moves from I->D
  infectious_death_event <- TargetedEvent$new(population_size = N)
  infectious_death_event$add_listener(function(t, target) {
    health$queue_update("D", target)
  })
  infectious_death_process <- function(t){
    I <- health$get_index_of("I")
    already_scheduled <- infectious_death_event$get_scheduled()
    I$and(already_scheduled$not(inplace = TRUE))
    death_times <- round((rgamma(I$size(), 4, 1) + 1) / dt)
    infectious_death_event$schedule(target = I, delay = death_times)
  }
  
  ## Define event for individuals in D to go to either Dobs or D_unobs
  observation_event <- TargetedEvent$new(population_size = N)
  observation_event$add_listener(function(t, target) {
    health$queue_update("Dobs", target)
  })
  unobserved_event <- TargetedEvent$new(population_size = N)
  unobserved_event$add_listener(function(t, target) {
    health$queue_update("D_unobs", target)
  })
  observation_process <- function(t){
    
    ## Getting index of individuals in D state
    D <- health$get_index_of("D")
    
    ## Getting individuals already scheduled
    Dobs_already_scheduled <- observation_event$get_scheduled()
    Dunobs_already_scheduled <- unobserved_event$get_scheduled()
    
    ## Subsetting D inplace by those who aren't already scheduled
    D$and(Dobs_already_scheduled$not(inplace = TRUE))
    D$and(Dunobs_already_scheduled$not(inplace = TRUE))
    
    unscheduled_to_Dobs <- D$copy()
    unscheduled_to_Dobs <- unscheduled_to_Dobs$sample(death_obs_prop)
    unscheduled_to_D_unobs <- D$and(unscheduled_to_Dobs$not(inplace = FALSE))
    
    observation_times <- round((rgamma(unscheduled_to_Dobs$size(), 1.5, 0.75) + 1) / dt)
    observation_event$schedule(target = unscheduled_to_Dobs, delay = observation_times)
    unobserved_event$schedule(target = unscheduled_to_D_unobs, delay = 1)
    
  }
  
  ## Define render
  health_render <- Render$new(timesteps = steps)
  health_render_process <- categorical_count_renderer_process(
    renderer = health_render,
    variable = health,
    categories =  c("S", "E", "I", "D", "D_unobs", "Dobs")
  )
  
  ## Run simulation loop
  final_state <- simulation_loop(
    variables = list(health),
    events = list(exposed_death_event, exposed_infectious_event, 
                  observation_event, unobserved_event, infectious_death_event),
    processes = list(exposure_process, exposed_progression_process, 
                     infectious_death_process, observation_process, 
                     health_render_process),
    timesteps = steps,
    restore_random_state = FALSE,
    ...)
  
  return(list(result = health_render$to_dataframe(), state = final_state))
}

