# load required libraries
library(individual); library(dplyr); library(tidyverse); library(doParallel); library(tictoc); library(parallel); library(profvis);

## Sourcing functions
source("functions/IBM_model.R")
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

# Loading in and processing Horto/PEAL data for model fitting
horto_df <- readRDS("data/processed_HortoData.rds") %>%
  filter(!is.na(zone_peal)) %>%
  filter(final_yfv_result != "negative")
epi_curve <- incidence::incidence(horto_df$date_collection)
plot(epi_curve)

# Generating incidence data and cutting off first 4 infections
start_date <- as.Date("2017-11-12")
horto_df_fitting <- horto_df %>%
  filter(date_collection > start_date) %>%
  group_by(date_collection) %>%
  summarise(count = n()) %>%
  complete(date_collection = seq.Date(start_date, 
                                      as.Date("2018-01-08"), 
                                      by = "days"),
           fill = list(count = 0))
plot(horto_df_fitting$date_collection, horto_df_fitting$count)
horto_df_fitting$time <- 1:nrow(horto_df_fitting)

## Particle filtering

## Model parameters
N <- 86 - 3 - 3 # (3 negative monkeys - assume the rest not in database killed by yellow fever) and other 3 are predeceased from where we're starting
N_obs <- sum(horto_df_fitting$count)
death_obs_prop <- N_obs / N
dt <- 0.2
initial_infections <- 1
gamma <- 1 / (infectious_period_gamma_shape / infectious_period_gamma_rate)

## Parameters for initial particle filtering to identify parameter regime of highest likelihood
R0_scan <- c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
start_date_scan <- as.Date(c("2017-11-12", "2017-11-14", "2017-11-16", "2017-11-18", "2017-11-20",
                             "2017-11-22", "2017-11-24", "2017-11-26", "2017-11-28", "2017-11-30"))
iterations <- 10
particles <- 500
cores <- 10

loglikelihood_matrix <- array(data = NA, dim = c(iterations, length(R0_scan), length(start_date_scan)))
final_size_matrix <- array(data = NA, dim = c(iterations, length(R0_scan), length(start_date_scan)))
output_matrix <- array(data = NA, dim = c(iterations, length(R0_scan), length(start_date_scan), length(horto_df_fitting$count)))

overall_seed <- 10
set.seed(overall_seed)
simulation_seeds <- array(data = rnbinom(n = iterations * length(R0_scan) * length(start_date_scan), mu = 10^6, size = 1), 
                          dim = c(length(R0_scan), length(start_date_scan), iterations))

## Looping through R0
for (i in 1:length(R0_scan)) {
  
  ## Looping through the start dates
  for (j in 1:length(start_date_scan)) {
    
    # Selecting the start date and filtering the Horto data to start then
    start_date <- start_date_scan[j]
    data <- horto_df_fitting %>%
      filter(date_collection >= start_date) %>%
      rename(daily_incidence = count)
    steps <- nrow(data) / dt
    
    # Defining the misc list that supports running the particle filter
    misc <- list(seed = simulation_seeds[i, j,  ], 
                 steps = steps, 
                 gamma = gamma,
                 particles = particles,
                 dt = dt, 
                 N = N, 
                 initial_infections = initial_infections, 
                 death_obs_prop = death_obs_prop, 
                 initial_run = TRUE, 
                 overall_run_length = steps,
                 latent_period_gamma_shape = latent_period_gamma_shape, 
                 EIP_gamma_shape = EIP_gamma_shape,
                 EIP_gamma_rate = EIP_gamma_rate, 
                 latent_period_gamma_rate = latent_period_gamma_rate,
                 infectious_period_gamma_shape = infectious_period_gamma_shape, 
                 infectious_period_gamma_rate = infectious_period_gamma_rate,
                 death_observation_gamma_shape = 1, 
                 death_observation_gamma_rate = death_observation_gamma_rate)
    
    # Setting up the cluster to run everything in parallel
    cl <- makeCluster(cores)
    clusterExport(cl, varlist = c("r_loglike", "weight_particles", "data", "misc", "run_simulation2"))
    clusterEvalQ(cl, {
      library(individual)
    })
    
    # Running the loglikelihood function in parallel
    R0_temp <- c("R0" = R0_scan[i])
    clusterExport(cl, varlist = c("R0_temp"))
    result_parallel <- parLapply(cl, 1:iterations, function(i) {
      misc_new <- misc
      misc_new$seed <- misc$seed[i]
      temp <- r_loglike(R0_temp, data, misc_new)
      return(temp)
    })
    parallel::stopCluster(cl)
    
    # Storing the output
    padding_zeroes <- rep(0, as.numeric(start_date_scan[j] - start_date_scan[1]))
    for (k in 1:iterations) {
      output_matrix[k, i, j, ] <- c(padding_zeroes, result_parallel[[k]]$deaths_trajectory)
      final_size_matrix[k, i, j] <- sum(result_parallel[[k]]$deaths_trajectory)
      loglikelihood_matrix[k, i, j] <- result_parallel[[k]]$loglikelihood
    }
    
    print(c("j = ", j))
    
  }
  
  print(i)
  
}

saveRDS(list(output = output_matrix, final_size = final_size_matrix, loglike = loglikelihood_matrix),
        "3_horto_YFV_R0_estimation/initial_parameterScan_hortoEstimation.rds")

loglik_avg <- apply(loglikelihood_matrix, c(2, 3), mean)
colnames(loglik_avg) <- paste0("start=", start_date_scan)
rownames(loglik_avg) <- paste0("R0=", R0_scan)

df_long <- data.frame(R0 = R0_scan, loglik_avg) %>%
  pivot_longer(cols = -R0, 
               names_to = "StartDate",
               values_to = "Value") %>% 
  mutate(StartDate = sub("^start\\.", "", StartDate), 
         StartDate = gsub("\\.", "-", StartDate), 
         StartDate = as.Date(StartDate, format = "%Y-%m-%d")) %>%
  mutate(LogLikelihood_adj = Value - max(Value),
         Likelihood = exp(LogLikelihood_adj),
         Probability = Likelihood / sum(Likelihood))

ggplot(df_long, aes(x = StartDate, y = factor(R0), fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "RdBu", limits = c(-60, -50), oob = scales::squish) + 
  labs(
    title = "Heatmap of Values by R₀ and Start Date",
    x = "Start Date",
    y = expression(R[0]),
    fill = "Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

colors37 <- c("#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc","#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a","#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979","#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672","#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7","#895c8b","#bd5975")
par(mfrow = c(length(R0_scan), length(start_date_scan)), mar = c(2, 2, 2, 2))
for (i in 1:10) {
  for (j in 1:10) {
    padding_zeroes <- rep(0, as.numeric(start_date_scan[j] - start_date_scan[1]))
    for (k in 1:iterations) {
      if (k == 1) {
        plot(output_matrix[k, i, j, ], type = "l", col = adjustcolor(colors37[i], alpha.f = 0.2),
             ylim = c(0, max(c(horto_df_fitting$count, output_matrix[, i, , ]))),
             main = paste0("R0 = ", R0_scan[i], ", loglik = ", 
                           round(apply(loglikelihood_matrix, c(2, 3), mean)[i, j], 2)),
             ylab = "", xlab = "")
      } else {
        lines(output_matrix[k, i, j, ], type = "l", col = adjustcolor(colors37[i], alpha.f = 0.2))
      }
    }
    points(horto_df_fitting$count, pch = 20, col = "black", cex = 1)
  }
}

set.seed(123)  # For reproducibility
samples <- 10000
sampled_indices <- sample(
  1:nrow(df_long),
  size = 10000,
  replace = TRUE,
  prob = df_long$Probability
)
sampled_data <- df_long[sampled_indices, c("R0", "StartDate")]
for (i in 1:nrow(sampled_data)) {
  R0 <- unlist(sampled_data[i, "R0"])
  R0_index <- which(rownames(loglik_avg) == paste0("R0=", R0))
  start_date <- sampled_data[i, "StartDate"]
  start_date <- start_date$StartDate
  start_date_index <- which(colnames(loglik_avg) == paste0("start=", start_date))
  k <- sample(1:iterations, 1)
  if (i == 1) {
    plot(output_matrix[k, R0_index, start_date_index, ], type = "l", col = adjustcolor("red", alpha.f = 0.2),
         ylim = c(0, max(c(horto_df_fitting$count, output_matrix[, i, , ]))), ylab = "", xlab = "")
  } else {
    lines(output_matrix[k, R0_index, start_date_index, ], type = "l", col = adjustcolor("red", alpha.f = 0.2))
  }
}
points(horto_df_fitting$count, pch = 20, col = "black", cex = 1)

sampled_output_matrix <- matrix(nrow = samples, ncol = length(horto_df_fitting$count))
for (i in 1:samples) {
  R0 <- unlist(sampled_data[i, "R0"])
  R0_index <- which(rownames(loglik_avg) == paste0("R0=", R0))
  start_date <- sampled_data[i, "StartDate"]
  start_date <- start_date$StartDate
  start_date_index <- which(colnames(loglik_avg) == paste0("start=", start_date))
  k <- sample(1:iterations, 1)
  sampled_output_matrix[i, ] <- output_matrix[k, R0_index, start_date_index, ]
}

lower <- apply(sampled_output_matrix, 2, min)
upper <- apply(sampled_output_matrix, 2, max)
lower <- apply(sampled_output_matrix, 2, quantile, 0.025)
upper <- apply(sampled_output_matrix, 2, quantile, 0.975)
mean <- apply(sampled_output_matrix, 2, mean)

plot(mean, type = "l", col = adjustcolor("red", alpha.f = 1),
     ylim = c(0, max(c(horto_df_fitting$count, output_matrix))), ylab = "", xlab = "")
lines(lower)
lines(upper)
points(horto_df_fitting$count, pch = 20, col = "black", cex = 1)
