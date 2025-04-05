### Individual model (this time with a fixed version of the renewal-equation-style EIP delay)
run_simulation2 <- function(parameters_list, ...) {
  
  ## Setting the seed
  set.seed(parameters_list$seed)
  
  ## Define disease states
  health <- CategoricalVariable$new(categories = c("S", "E", "I", "D", "D_unobs", "Dobs"),
                                    initial_values = c(rep("S", parameters_list$N - parameters_list$initial_infections), 
                                                       rep("E", parameters_list$initial_infections)))
  
  ## Defining lagged variable to accommodate implicit EIP in vector population (not explicitly modelled)
  LaggedValue <- R6::R6Class(
    'LaggedValue',
    private = list(
      history = NULL
    ),
    public = list(
      initialize = function(default) {
        private$history <- rep(0, default)
        names(private$history) <- seq(0:(default - 1))
      },
      
      save = function(value, timesteps) {
        private$history[timesteps] <- value
      },
      
      get = function(timesteps) {
        timesteps[timesteps < 1] <- 1
        private$history[timesteps]
      },
      
      get_state = function() {
        private$history
      },
      
      restore_state = function(history) {
        private$history <- history
      }
    )
  )
  lagged_inc <- LaggedValue$new(parameters_list$overall_run_length)
  
  ## Exposure process moving individuals S->E
  ### Note: In previous versions of the model, this was applied to "I" which isn't quite right
  ###       as monkeys spend more than 1 timestep in I. Really what I think you want to do is 
  ###       apply it to incidence (not number of Infectious individuals).
  ###       Need to check that this plays nicely with the timestep though.
  # exposure_process <- function(t){
  #   lagged_I$save(health$get_size_of("I"), t)
  #   I <- lagged_I$get(seq(t - parameters_list$past_length, t - 1))
  #   foi <- parameters_list$beta * sum((parameters_list$past_weightings_vector * I))
  #   S <- health$get_index_of("S")
  #   S$sample(rate = pexp(q = foi * parameters_list$dt))
  #   health$queue_update(value = "E",index = S)
  # }
  exposure_process <- function(t){
    inc <- lagged_inc$get(seq(t - parameters_list$past_length, t - 1))                # get all incidence (up to past length in the past) from timestep before onwards
    foi <- parameters_list$beta * sum((parameters_list$past_weightings_vector * inc)) # calculate FOI
    S <- health$get_index_of("S")                                                     # get index of all monkeys still in susceptible state
    S$sample(rate = pexp(q = foi * parameters_list$dt))                               # sampling those who are infected (move to E) in this timestep
    health$queue_update(value = "E",index = S)                                        # updating the health categorical variable
  }
  
  ## Define infection event and process to schedule moves from E->I
  exposed_infectious_event <- TargetedEvent$new(population_size = parameters_list$N)
  exposed_infectious_event$add_listener(function(t, target) {
    health$queue_update("I", target)
  })
  exposed_progression_process <- function(t){
    E <- health$get_index_of("E")
    exposed_infectious_already_scheduled <- exposed_infectious_event$get_scheduled()
    E$and(exposed_infectious_already_scheduled$not(inplace = TRUE))
    infection_times <- round((rgamma(E$size(), parameters_list$latent_period_gamma_shape, parameters_list$latent_period_gamma_rate) + 1) / parameters_list$dt)
    exposed_infectious_event$schedule(target = E, delay = infection_times)
    for (i in seq_along(infection_times)) {
      infection_time_temp <- infection_times[i]
      print(infection_time_temp)
      previous_incidence <- lagged_inc$get(round(t + infection_time_temp))
      lagged_inc$save(previous_incidence + 1, t + infection_time_temp)                                                      # saving the total number of individuals infected in this timestep
    }
  }
  
  ## Define death of I event and process to schedule moves from I->D
  infectious_death_event <- TargetedEvent$new(population_size = parameters_list$N)
  infectious_death_event$add_listener(function(t, target) {
    health$queue_update("D", target)
  })
  infectious_death_process <- function(t){
    I <- health$get_index_of("I")
    already_scheduled <- infectious_death_event$get_scheduled()
    I$and(already_scheduled$not(inplace = TRUE))
    death_times <- round((rgamma(I$size(), parameters_list$infectious_period_gamma_shape, parameters_list$infectious_period_gamma_rate) + 1) / parameters_list$dt)
    infectious_death_event$schedule(target = I, delay = death_times)
  }
  
  ## Define event for individuals in D to go to either Dobs or D_unobs
  observation_event <- TargetedEvent$new(population_size = parameters_list$N)
  observation_event$add_listener(function(t, target) {
    health$queue_update("Dobs", target)
  })
  unobserved_event <- TargetedEvent$new(population_size = parameters_list$N)
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
    unscheduled_to_Dobs <- unscheduled_to_Dobs$sample(parameters_list$death_obs_prop)
    unscheduled_to_D_unobs <- D$and(unscheduled_to_Dobs$not(inplace = FALSE))
    
    observation_times <- round((rgamma(unscheduled_to_Dobs$size(), parameters_list$death_observation_gamma_shape, parameters_list$death_observation_gamma_rate) + 1) / parameters_list$dt)
    observation_event$schedule(target = unscheduled_to_Dobs, delay = observation_times)
    unobserved_event$schedule(target = unscheduled_to_D_unobs, delay = 1)
    
  }

  ## Define render
  health_render <- Render$new(timesteps = parameters_list$steps)
  health_render_process <- categorical_count_renderer_process(
    renderer = health_render,
    variable = health,
    categories =  c("S", "E", "I", "D", "D_unobs", "Dobs")
  )
  
  ## Run simulation loop
  final_state <- simulation_loop(
    variables = list(health),
    events = list(exposed_infectious_event, observation_event, unobserved_event, infectious_death_event),
    processes = list(exposure_process, exposed_progression_process, 
                     infectious_death_process, observation_process, health_render_process),
    timesteps = parameters_list$steps,
    restore_random_state = FALSE,
    ...)
  
  return(list(result = health_render$to_dataframe(), state = final_state, lagged_inc = lagged_inc))
}
