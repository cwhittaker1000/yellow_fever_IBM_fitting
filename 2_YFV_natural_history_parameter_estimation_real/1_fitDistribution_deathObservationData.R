## Loading required libraries
library(tidyverse); library(rstan)

# Loading in cleaned data
df <- readRDS("data/processed_HortoData.rds") %>%
  filter(!is.na(zone_peal)) %>%
  filter(final_yfv_result != "negative")
df$estimated_date_death[df$id == 39] <- "22/12/2017" ## date_notification umabiguously given as 27/12/2017 so assume the original "22/10/2017" is a typo
df$estimated_date_death[df$id == 52] <- "29/12/2017" ## date_notification umabiguously given as 29/12/2017 so assume the original "29/10/2017" is a typo

# Processing to get time delays between relevant events we need (i.e. time between death and collection)
df2 <- df %>%
  rename(date_collection_cleaned = date_collection) %>%
  mutate(date_notification_cleaned = as.Date(substr(date_notification, start = 1, stop = 10), format = "%d/%m/%Y")) %>%
  mutate(date_death_cleaned = gsub("[ \t\\?]", "", estimated_date_death)) %>%
  mutate(date_death_cleaned = as.Date(substr(date_death_cleaned, start = 1, stop = 10), format = "%d/%m/%Y")) %>%
  mutate(death_notification_delay = date_notification_cleaned - date_death_cleaned,
         death_collection_delay = date_collection_cleaned - date_death_cleaned,
         death_collection_delay2 = ifelse(death_collection_delay < 0.5, 0.5, death_collection_delay), 
         notification_collection_delay = date_collection_cleaned - date_notification_cleaned) %>%
  filter(death_collection_delay >= 0)
hist(as.numeric(df2$death_collection_delay), breaks = 15)
hist(as.numeric(df2$death_notification_delay), breaks = 15)
hist(as.numeric(df2$notification_collection_delay), breaks = 15)
mean(as.numeric(df2$death_collection_delay))
mean(as.numeric(df2$death_notification_delay), na.rm = TRUE)
median(as.numeric(df2$death_collection_delay))
median(as.numeric(df2$death_notification_delay), na.rm = TRUE)

x <- as.numeric(df2$death_collection_delay)
mean(x)
y <- x
y[y < 0.5] <- 0.5
mean(y)

## Fitting the time between infection and death - Gamma distribution
model <- stan_model("2_YFV_natural_history_parameter_estimation_real/models/observation_time_gamma.stan")
data_stan <- list(N = nrow(df) - 1,
                  days = as.numeric(df2$death_collection_delay2) + 0.01,
                  a_1 = 1,
                  a_2 = 1,
                  b_1 = 2,
                  b_2 = 2)
fit <- sampling(model, data=data_stan, iter=2000, chains=4)
hist(rstan::extract(fit, "days_simulated")[[1]], breaks = 50)
summary(fit)
saveRDS(fit, "outputs/deathObservation_distGamma_stanFit.rds")
hist(rgamma(10000, rstan::extract(fit, "a")[[1]], rstan::extract(fit, "b")[[1]]), breaks = 50)

df_gamma <- lapply(seq_along(1:1000), function(i) {
  x_vals <- seq(0, 15, by = 0.5)
  data.frame(i = i, x = x_vals,
             pdfval = dgamma(x_vals, shape = rstan::extract(fit, "a")[[1]][i], 
                             rate = rstan::extract(fit, "b")[[1]][i]))}) %>%
  bind_rows()


## Fitting the time between infection and death - Exponential distribution
model2 <- stan_model("2_YFV_natural_history_parameter_estimation_real/models/observation_time_exp.stan")
data_stan2 <- list(N = nrow(df) - 1,
                   days = as.numeric(df2$death_collection_delay2),
                   a_1 = 1)
fit2 <- sampling(model2, data = data_stan2, iter = 2000, chains = 4)
hist(rstan::extract(fit2, "days_simulated")[[1]], breaks = 50)
summary(fit2)
saveRDS(fit2, "outputs/deathObservation_distExp_stanFit.rds")
hist(rexp(10000, rstan::extract(fit2, "a")[[1]]), breaks = 50)

df_exp <- lapply(seq_along(1:1000), function(i) {
  x_vals <- seq(0, 15, by = 0.5)
  data.frame(i = i, x = x_vals,
             pdfval = dexp(x_vals, rate = rstan::extract(fit2, "a")[[1]][i]))}) %>%
  bind_rows()

## Comparing them to empirical distribution and calculating which fits better
df_sim_gamma <- tibble(obs_delay = rstan::extract(fit, "days_simulated")[[1]], iteration = seq_along(fit), type = "simulatedGamma")
df_sim_exp <- tibble(obs_delay = rstan::extract(fit2, "days_simulated")[[1]], iteration = seq_along(fit2), type = "simulatedExp")
df_empirical <- tibble(obs_delay = as.numeric(df2$death_collection_delay), iteration = seq_along(as.numeric(df2$death_collection_delay)), type = "empirical")

df_overall <- rbind(df_sim_gamma, df_sim_exp, df_empirical)
ggplot(df_overall) +
  geom_density(aes(x = obs_delay, colour=type)) +
  scale_x_continuous(limits = c(0, 15)) +
  scale_color_brewer("Data", palette = "Dark2") +
  xlab("Days from death to observation") +
  ylab("Density")

# Normalize counts for empirical data
df_empirical_counts <- df_overall %>%
  filter(type == "empirical") %>%
  count(obs_delay) %>% # Count occurrences of each day
  mutate(normalized_count = n / sum(n)) # Normalize by total counts

# Update the empirical part of df_overall with normalized values
df_empirical_normalized <- df_empirical_counts %>%
  rename(obs_delay = obs_delay) %>%
  mutate(type = "empirical")

# Add normalized empirical data back to df_overall
df_overall <- df_overall %>%
  filter(type != "empirical") %>%
  bind_rows(df_empirical_normalized)

# Create the plot
death_obs_delay_plot <- ggplot() +
  # Normalized bar plot for empirical data
  geom_bar(data = df_empirical_counts,
           aes(x = obs_delay, y = normalized_count, fill = "empirical"), 
           stat = "identity", alpha = 1, width = 0.7) +
  # Density for simulated gamma and exponential
  geom_density(data = df_overall %>% filter(type %in% c("simulatedExp")),
               aes(x = obs_delay), 
               size = 1, adjust = 1) +
  # Aesthetic adjustments
  scale_x_continuous(limits = c(-1, 15), breaks = seq(0, 15, 1)) +
  scale_y_continuous(name = "Density / Normalized Count") +
  scale_color_brewer("Density Data", palette = "Dark2") +
  scale_fill_manual("Data", values = c("empirical" = "#948D9B")) +
  xlab("Days from death to observation") +
  ylab("Density (Simulated) or Normalized Count (Empirical)") +
  theme(legend.position = "right") +
  theme_bw(base_family = "sans") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        # legend.position = c(0.98, 0.98),  
        # legend.justification = c(1, 1),
        legend.position = "none")

deaths_obs_delay_plot2 <- ggplot() +
  # Normalized bar plot for empirical data
  geom_bar(data = df_empirical_counts,
           aes(x = obs_delay, y = normalized_count, fill = "empirical"), 
           stat = "identity", alpha = 1, width = 0.7) +
  # Density for simulated gamma and exponential
  geom_line(data = df_exp,
               aes(x = x, y = pdfval, group = i), alpha = 0.025, 
               size = 1, adjust = 1, col = "#80C498") +
  # Aesthetic adjustments
  scale_x_continuous(limits = c(-1, 15), breaks = seq(0, 15, 1)) +
  scale_y_continuous(name = "Density / Normalized Count") +
  scale_color_brewer("Density Data", palette = "Dark2") +
  scale_fill_manual("Data", values = c("empirical" = "#948D9B")) +
  xlab("Days from death to observation") +
  ylab("Density (Simulated) or Normalized Count (Empirical)") +
  theme(legend.position = "right") +
  theme_bw(base_family = "sans") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        # legend.position = c(0.98, 0.98),  
        # legend.justification = c(1, 1),
        legend.position = "none")

ggsave(filename = "2_YFV_natural_history_parameter_estimation_real/figures/SI_NHP_DeathObs.pdf",
       plot = deaths_obs_delay_plot2,
       width = 4, height = 4)

