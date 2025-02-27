# load required libraries
library(individual); library(dplyr); library(tidyverse); library(doParallel); library(tictoc); 
library(parallel); library(profvis); library(truncnorm)

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
EIP_gamma_fit <- readRDS("outputs/EIP_adultMice_gammaParams_25degrees.rds")
EIP_gamma_shape <- EIP_gamma_fit$gamma_a
EIP_gamma_rate <- EIP_gamma_fit$gamma_b

exposure_death_delay <- round((latent_period_gamma_shape / latent_period_gamma_rate) + 
  (infectious_period_gamma_shape / infectious_period_gamma_rate) +
  (EIP_gamma_shape / EIP_gamma_rate) +
  (1 / death_observation_gamma_rate))

# Loading in and processing Horto/PEAL data for model fitting
horto_df <- readRDS("data/processed_HortoData.rds") %>%
  filter(!is.na(zone_peal)) %>%
  filter(final_yfv_result != "negative")
epi_curve <- incidence::incidence(horto_df$date_collection)
plot(epi_curve)

# Generating incidence data and cutting off first 4 infections
start_date <- as.Date("2017-10-23")
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
importations <- 18 # from the genomic data
importation_last_date <- max(horto_df_fitting$date_collection) - exposure_death_delay # upper bound assumed to be 1 generation time before the final monkey death

## Parameters for initial particle filtering to identify parameter regime of highest likelihood
R0_scan <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
start_date_scan <- start_date + seq(0, 36, 3)
transmission_type_scan <- c("density_dependent")
exponential_noise_scan <- 1/1e-1
R0_prior_function <- function(R0_value) { return(log(dtruncnorm(R0_value, a = 1, b = 18, mean = 3.55946, sd = 3.310322))) }

iterations <- 10
particles <- 500
cores <- 10

loglikelihood_matrix <- array(data = NA, dim = c(iterations, length(R0_scan), length(start_date_scan), length(transmission_type_scan), length(exponential_noise_scan)))
epilikelihood_matrix <- array(data = NA, dim = c(iterations, length(R0_scan), length(start_date_scan), length(transmission_type_scan), length(exponential_noise_scan)))
importlikelihood_matrix <- array(data = NA, dim = c(iterations, length(R0_scan), length(start_date_scan), length(transmission_type_scan), length(exponential_noise_scan)))
startdatelikelihood_matrix <- array(data = NA, dim = c(iterations, length(R0_scan), length(start_date_scan), length(transmission_type_scan), length(exponential_noise_scan)))
importations_matrix <- array(data = NA, dim = c(iterations, length(R0_scan), length(start_date_scan), length(transmission_type_scan), length(exponential_noise_scan)))
final_size_matrix <- array(data = NA, dim = c(iterations, length(R0_scan), length(start_date_scan), length(transmission_type_scan), length(exponential_noise_scan)))
output_matrix <- array(data = NA, dim = c(iterations, length(R0_scan), length(start_date_scan), length(transmission_type_scan), length(exponential_noise_scan), length(horto_df_fitting$count)))

overall_seed <- 10
set.seed(overall_seed)
simulation_seeds <- array(data = rnbinom(n = iterations * length(R0_scan) * length(start_date_scan) * length(transmission_type_scan) * length(exponential_noise_scan), 
                                         mu = 10^6, size = 1), 
                          dim = c(length(R0_scan), length(start_date_scan), length(transmission_type_scan), length(exponential_noise_scan), iterations))
fresh_run <- TRUE
if (fresh_run) {
  
  ## Looping through R0
  for (i in 1:length(R0_scan)) {
    
    ## Looping through the start dates
    for (j in 1:length(start_date_scan)) {
      
      for (k in 1:length(transmission_type_scan)) {
        
        for (l in 1:length(exponential_noise_scan)) {
          
          # Selecting the start date and filtering the Horto data to start then
          start_date <- start_date_scan[j]
          data <- horto_df_fitting %>%
            filter(date_collection >= start_date) %>%
            rename(daily_incidence = count)
          steps <- nrow(data) / dt
          days <- nrow(data)
          
          # Calculating the importation rate - note that given the epidemic goes to extinction, we really
          # need to calculate it up to 1 full generation time before the last death (i.e. the time when that last monkey was infected 
          # in our model) - final date in importation_date_range is the actual time when all the susceptibles are depleted
          importation_date_range <- data %>%
            filter(date_collection <= importation_last_date)
          importation_rate <- importations / nrow(importation_date_range) 
          
          # Defining the misc list that supports running the particle filter
          misc <- list(seed = simulation_seeds[i, j, k, l, ], 
                       steps = steps, 
                       gamma = gamma,
                       particles = particles,
                       dt = dt, 
                       N = N, 
                       start_date = start_date,
                       importation_rate = importation_rate,
                       empirical_importations = importations,
                       transmission_type = transmission_type_scan[k], 
                       exponential_noise_rate = exponential_noise_scan[l],
                       likelihood = c("epidemiological", "importations", "start_date"),
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
                       death_observation_gamma_rate = death_observation_gamma_rate,
                       prior_function = R0_prior_function)
          
          # Setting up the cluster to run everything in parallel
          cl <- makeCluster(cores)
          clusterExport(cl, varlist = c("r_loglike", "weight_particles", "data", "misc", "run_simulation2"))
          clusterEvalQ(cl, {
            library(individual)
            library(truncnorm)
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
          for (m in 1:iterations) {
            output_matrix[m, i, j, k, l, ] <- c(padding_zeroes, result_parallel[[m]]$deaths_trajectory)
            final_size_matrix[m, i, j, k, l] <- sum(result_parallel[[m]]$deaths_trajectory)
            loglikelihood_matrix[m, i, j, k, l] <- result_parallel[[m]]$loglikelihood
            epilikelihood_matrix[m, i, j, k, l] <- result_parallel[[m]]$likelihood_components$epi
            importlikelihood_matrix[m, i, j, k, l] <- result_parallel[[m]]$likelihood_components$importations
            startdatelikelihood_matrix[m, i, j, k, l] <- result_parallel[[m]]$likelihood_components$start
            importations_matrix[m, i, j, k, l] <- result_parallel[[m]]$importations
          }
          
          print(c("l = ", l))
          
        }
        
        print(c("k = ", k))
        
      }
      
      print(c("j = ", j))
      
    }
    
    print(i)
    
  }
  
  saveRDS(list(output = output_matrix, final_size = final_size_matrix, 
               loglike = loglikelihood_matrix, epilikelihood_matrix = epilikelihood_matrix,
               importlikelihood_matrix = importlikelihood_matrix, startdatelikelihood_matrix = startdatelikelihood_matrix, 
               importations = importations_matrix),
          "3_horto_YFV_R0_estimation/NewTester_parameterScan_hortoEstimation_YesImportations.rds")
  
} else {
  
  temp <- readRDS("3_horto_YFV_R0_estimation/parameterScan_hortoEstimation_YesImportations.rds")
  loglikelihood_matrix <- temp$loglike
  output_matrix <- temp$output
}

## Overall loglikelihood
loglik_avg <- apply(loglikelihood_matrix, c(2, 3, 4 ,5), mean)
dimnames(loglik_avg) <- list(
  R0 = R0_scan,                   # use your R0_scan vector here
  start_date = paste0("s", as.Date(start_date_scan)),
  transmission_type = transmission_type_scan,
  exponential_noise = exponential_noise_scan
)
df_long <- as.data.frame.table(loglik_avg, responseName = "loglikelihood") %>%
  mutate(start_date = as.Date(gsub("s", "", start_date))) %>%
  filter(exponential_noise == "10")
head(df_long)
scales <- c(-100, -60)
ggplot(df_long, aes(x = start_date, y = factor(R0), fill = loglikelihood)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "RdBu") + # , oob = scales::squish, limits = scales) + 
  labs(x = "Start Date",
       y = expression(R[0]),
       fill = "Avg.\nLoglike") +
  scale_x_date(expand = c(0, 0)) +  # Remove whitespace on the x-axis
  scale_y_discrete(expand = c(0, 0)) +  # Remove whitespace on the y-axis
  theme_bw() +
  facet_grid(transmission_type ~ exponential_noise) +
  theme(# axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right")

epi_loglik_avg <- apply(epilikelihood_matrix, c(2, 3, 4 ,5), mean)
dimnames(epi_loglik_avg) <- list(
  R0 = R0_scan,                   # use your R0_scan vector here
  start_date = paste0("s", as.Date(start_date_scan)),
  transmission_type = transmission_type_scan,
  exponential_noise = exponential_noise_scan
)
df_long <- as.data.frame.table(epi_loglik_avg, responseName = "loglikelihood") %>%
  mutate(start_date = as.Date(gsub("s", "", start_date))) %>%
  filter(exponential_noise == "10")
head(df_long)
scales <- c(-100, -60)
ggplot(df_long, aes(x = start_date, y = factor(R0), fill = loglikelihood)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "RdBu") + # , oob = scales::squish, limits = scales) + 
  labs(x = "Start Date",
       y = expression(R[0]),
       fill = "Avg.\nLoglike") +
  scale_x_date(expand = c(0, 0)) +  # Remove whitespace on the x-axis
  scale_y_discrete(expand = c(0, 0)) +  # Remove whitespace on the y-axis
  theme_bw() +
  facet_grid(transmission_type ~ exponential_noise) +
  theme(# axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right")

## Epidemiological likelihood

## Importations likelihood
importation_lik_avg <- apply(importlikelihood_matrix, c(2, 3, 4 ,5), mean)
dimnames(importation_lik_avg) <- list(
  R0 = R0_scan,                   # use your R0_scan vector here
  start_date = paste0("s", as.Date(start_date_scan)),
  transmission_type = transmission_type_scan,
  exponential_noise = exponential_noise_scan
)
df_long_import <- as.data.frame.table(importation_lik_avg, responseName = "loglikelihood") %>%
  mutate(start_date = as.Date(gsub("s", "", start_date))) %>%
  filter(exponential_noise == "10")
head(df_long_import)
scales <- c(-100, -50)
ggplot(df_long_import, aes(x = start_date, y = factor(R0), fill = loglikelihood)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "RdBu", oob = scales::squish) + #  limits = scales) + 
  labs(x = "Start Date",
       y = expression(R[0]),
       fill = "Avg.\nLoglike") +
  scale_x_date(expand = c(0, 0)) +  # Remove whitespace on the x-axis
  scale_y_discrete(expand = c(0, 0)) +  # Remove whitespace on the y-axis
  theme_bw() +
  facet_grid(transmission_type ~ exponential_noise) +
  theme(# axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right")

## startdatelikelihood_matrix likelihood
startdate_lik_avg <- apply(startdatelikelihood_matrix, c(2, 3, 4 ,5), mean)
dimnames(startdate_lik_avg) <- list(
  R0 = R0_scan,                   # use your R0_scan vector here
  start_date = paste0("s", as.Date(start_date_scan)),
  transmission_type = transmission_type_scan,
  exponential_noise = exponential_noise_scan
)
df_long_start <- as.data.frame.table(startdate_lik_avg, responseName = "loglikelihood") %>%
  mutate(start_date = as.Date(gsub("s", "", start_date)))
head(df_long)
scales <- c(-100, -60)
ggplot(df_long_start, aes(x = start_date, y = factor(R0), fill = loglikelihood)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "RdBu", oob = scales::squish) + #  limits = scales) + 
  labs(x = "Start Date",
       y = expression(R[0]),
       fill = "Avg.\nLoglike") +
  scale_x_date(expand = c(0, 0)) +  # Remove whitespace on the x-axis
  scale_y_discrete(expand = c(0, 0)) +  # Remove whitespace on the y-axis
  theme_bw() +
  facet_grid(transmission_type ~ exponential_noise) +
  theme(# axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right")


colnames(loglik_avg) <- paste0("start=", start_date_scan)
rownames(loglik_avg) <- paste0("R0=", R0_scan)

importations_avg <- apply(importations_matrix, c(2, 3), mean)
colnames(importations_avg) <- paste0("start=", start_date_scan)
rownames(importations_avg) <- paste0("R0=", R0_scan)

apply(importations_avg, 2, mean)

imports <- apply(importations_matrix, c(2, 3, 4, 5), mean)
dimnames(imports) <- list(
  R0 = R0_scan,                   # use your R0_scan vector here
  start_date = paste0("s", as.Date(start_date_scan)),
  transmission_type = transmission_type_scan,
  exponential_noise = exponential_noise_scan
)
df_long_imports <- as.data.frame.table(imports, responseName = "imports") %>%
  mutate(start_date = as.Date(gsub("s", "", start_date))) %>%
  filter(exponential_noise == "10")
ggplot(df_long_imports, aes(x = start_date, y = factor(R0), fill = imports)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "RdBu", oob = scales::squish) + #  limits = scales) + 
  labs(x = "Start Date",
       y = expression(R[0]),
       fill = "Avg.\nLoglike") +
  scale_x_date(expand = c(0, 0)) +  # Remove whitespace on the x-axis
  scale_y_discrete(expand = c(0, 0)) +  # Remove whitespace on the y-axis
  theme_bw() +
  facet_grid(transmission_type ~ exponential_noise) +
  theme(# axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right")
## lower R0 and later start date needs more imports

scales <- c(-65, -50)

## add in likelihood term for importations

## Plotting heatmap of inferred R0 and start-date combinations
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
inferred_parameters_plot <- ggplot(df_long, aes(x = StartDate, y = factor(R0), fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "RdBu", limits = scales, oob = scales::squish) + 
  labs(x = "Start Date",
       y = expression(R[0]),
       fill = "Avg.\nLoglike") +
  scale_x_date(expand = c(0, 0)) +  # Remove whitespace on the x-axis
  scale_y_discrete(expand = c(0, 0)) +  # Remove whitespace on the y-axis
  theme_bw() +
  theme(# axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right")

## Sampling parameter combinations
set.seed(123)
samples <- 10000
sampled_indices <- sample(1:nrow(df_long), 
                          size = 10000,
                          replace = TRUE,
                          prob = df_long$Probability)
sampled_data <- df_long[sampled_indices, c("R0", "StartDate", "Value")] 

## Marginal for R0
avg_R0_values <- sampled_data %>%
  group_by(R0) %>%
  summarise(AvgValue = mean(Value))
sampled_data_R0 <- sampled_data %>%
  left_join(avg_R0_values, by = "R0")
sampled_data_R0$R0 <- as.factor(sampled_data_R0$R0)
R0_marginal_plot <- ggplot(sampled_data_R0, aes(x = R0, fill = AvgValue)) +
  geom_histogram(color = "black", stat = "count") +
  scale_fill_distiller(palette = "RdBu", limits = scales, oob = scales::squish) + 
  labs(x = "Inferred Marginal R0 Distribution",
       y = "Frequency",
       fill = "Avg.\nLoglike") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  annotate("text", x = 0.75, y = 2000,
           label = "R0: Basic Reproduction Number", hjust = 0, fontface = "bold", size = 5)

cowplot::plot_grid(R0_marginal_plot, )

## Marginal for start date
avg_start_date_values <- sampled_data %>%
  group_by(StartDate) %>%
  summarise(AvgValue = mean(Value))
sampled_data_StartDate <- sampled_data %>%
  left_join(avg_start_date_values, by = "StartDate")
sampled_data_StartDate$StartDate <- as.factor(sampled_data_StartDate$StartDate)
start_date_marginal_plot <- ggplot(sampled_data_StartDate, aes(x = StartDate, fill = AvgValue)) +
  geom_histogram(color = "black", stat = "count") +
  scale_fill_distiller(palette = "RdBu", limits = scales, oob = scales::squish) + 
  labs(y = "Frequency",
       fill = "Avg.\nLoglike") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  annotate("text", x = 0.75, y = 3550,
           label = "Primary Epidemic Start Date", hjust = 0, fontface = "bold", size = 5)

marginals_legend <- cowplot::get_legend(start_date_marginal_plot)

marginals_plot <- cowplot::plot_grid(R0_marginal_plot + theme(legend.position = "none"), 
                                     start_date_marginal_plot + theme(legend.position = "none"), 
                                     nrow = 2, labels = c("C", "D"))

marginals_plot_with_legend <- cowplot::plot_grid(marginals_plot, marginals_legend, nrow = 1, rel_widths = c(4, 1))

inference_overall <- cowplot::plot_grid(inferred_parameters_plot, 
                                        marginals_plot + theme(legend.position = "none"))

## Plotting the inferred deaths trajectories
samples <- 10000
sampled_output_matrix <- matrix(nrow = samples, ncol = length(horto_df_fitting$count))
for (i in 1:samples) {
  R0 <- unlist(sampled_data[i, "R0"])
  R0_index <- which(rownames(loglik_avg) == paste0("R0=", R0))
  start_date <- sampled_data[i, "StartDate"]
  start_date <- start_date$StartDate
  start_date_index <- which(colnames(loglik_avg) == paste0("start=", start_date))
  k <- sample(1:iterations, 1)
  sampled_output_matrix[i, ] <- output_matrix[k, R0_index, start_date_index, 1, 1, ]
}

lower <- apply(sampled_output_matrix, 2, quantile, 0.025)
upper <- apply(sampled_output_matrix, 2, quantile, 0.975)
min <- apply(sampled_output_matrix, 2, min)
max <- apply(sampled_output_matrix, 2, max)
mean <- apply(sampled_output_matrix, 2, mean)
output_df <- data.frame(time = horto_df_fitting$date_collection, 
                        observed = horto_df_fitting$count, 
                        mean = mean, 
                        lower = lower, 
                        upper = upper,
                        min = min,
                        max = max)
"#948D9B"
"#63A375"
"#AFD5AA"
"#729B79"
"#DAFF7D"
"#419D78"
outbreak_inference_plot <- ggplot(output_df, aes(x = time)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#AFD5AA", alpha = 0.2) +
  geom_line(aes(y = mean), color = "#AFD5AA", size = 0.75) +
  geom_point(aes(y = observed), color = "black", size = 2) +
  labs(x = "", y = "Daily Reported\nNHP Deaths") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_x_date(date_breaks = "1 week",
               limits = c(as.Date("2017-11-15"), NA)) +
  theme_bw() +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

cowplot::plot_grid(outbreak_inference_plot, inference_overall, nrow = 2, rel_heights = c(1, 1.45), 
                   labels = c("A", "B"), align = "v", axis = "r")

##########################################################
# lower <- apply(sampled_output_matrix, 2, min)
# upper <- apply(sampled_output_matrix, 2, max)
# plot(mean, type = "l", col = adjustcolor("red", alpha.f = 1),
#      ylim = c(0, max(c(horto_df_fitting$count, output_matrix))), ylab = "", xlab = "")
# lines(lower)
# lines(upper)
# points(horto_df_fitting$count, pch = 20, col = "black", cex = 1)
# for (i in 1:nrow(sampled_data)) {
#   R0 <- unlist(sampled_data[i, "R0"])
#   R0_index <- which(rownames(loglik_avg) == paste0("R0=", R0))
#   start_date <- sampled_data[i, "StartDate"]
#   start_date <- start_date$StartDate
#   start_date_index <- which(colnames(loglik_avg) == paste0("start=", start_date))
#   k <- sample(1:iterations, 1)
#   if (i == 1) {
#     plot(output_matrix[k, R0_index, start_date_index, ], type = "l", col = adjustcolor("red", alpha.f = 0.2),
#          ylim = c(0, max(c(horto_df_fitting$count, output_matrix[, i, , ]))), ylab = "", xlab = "")
#   } else {
#     lines(output_matrix[k, R0_index, start_date_index, ], type = "l", col = adjustcolor("red", alpha.f = 0.2))
#   }
# }
# points(horto_df_fitting$count, pch = 20, col = "black", cex = 1)
