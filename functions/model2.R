### Individual model

## this isn't quite right because the I compartment at different timepoints
## will contain the same people (i.e. renewal equation style weighting should)
## be applied to incidence of infection not to the prevalence, which is what
## the I state represents

run_simulation2 <- function(seed, steps, dt, N, initial_infections, death_obs_prop, 
                            beta, past_length, past_weightings_vector, initial_run, overall_run_length, 
                            lagged_I_input = NULL,
                            ...) {
  
  ## Setting the seed
  set.seed(seed)
  
  ## Define disease states
  health <- CategoricalVariable$new(categories = c("S", "E", "I", "D", "D_unobs", "Dobs"),
                                    initial_values = c(rep("S", N - initial_infections), rep("E", initial_infections)))
  
  ## Defining bits and bobs required for exposure process
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
  if (initial_run) {
    lagged_I = LaggedValue$new(overall_run_length)
  } else {
    lagged_I = LaggedValue$new(length(lagged_I_input$get_state()))
    if (!is.null(lagged_I_input)) {
      lagged_I$restore_state(lagged_I_input$get_state())
    }
  }
  
  exposure_process <- function(t){
    lagged_I$save(health$get_size_of("I"), t)
    I <- lagged_I$get(seq(t - past_length, t - 1))
    foi <- beta * sum((past_weightings_vector * I))
    S <- health$get_index_of("S")
    S$sample(rate = pexp(q = foi * dt))
    health$queue_update(value = "E",index = S)
  }
  
  ## Define infection event and process to schedule moves from E->I
  exposed_infectious_event <- TargetedEvent$new(population_size = N)
  exposed_infectious_event$add_listener(function(t, target) {
    health$queue_update("I", target)
  })
  exposed_progression_process <- function(t){
    E <- health$get_index_of("E")
    exposed_infectious_already_scheduled <- exposed_infectious_event$get_scheduled()
    E$and(exposed_infectious_already_scheduled$not(inplace = TRUE))
    infection_times <- round((rgamma(E$size(), 4, 2) + 1) / dt)
    exposed_infectious_event$schedule(target = E, delay = infection_times) 
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
    events = list(exposed_infectious_event, observation_event, unobserved_event, infectious_death_event),
    processes = list(exposure_process, exposed_progression_process, 
                     infectious_death_process, observation_process, health_render_process),
    timesteps = steps,
    restore_random_state = FALSE,
    ...)

  return(list(result = health_render$to_dataframe(), state = final_state, lagged_I = lagged_I))
}

## Checking out the simulation
# R0 <- 3
# N <- 86
# gamma <- 0.25
# beta <- R0 * gamma / N
# dt <- 0.2
# past_length <- 15
# past_weightings_vector <- rev(dgamma(1:past_length, shape = 5, rate = 0.5)) ### do I need to reverse it? I think yes
# x <- run_simulation2(seed = rpois(1, 10^7), steps = 300, dt = dt, N = 86, 
#                      initial_infections = 1, death_obs_prop = 1, 
#                      beta = beta, past_length = past_length, past_weightings_vector = past_weightings_vector)
# plot(x$result$timestep * dt, x$result$Dobs_count)
