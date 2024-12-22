## Loading required libraries
library(tidyverse); library(rstan)

# Loading in cleaned data
df <- readRDS("analyses/1_IBM_estimationR0/data/processed_HortoData.rds") %>%
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
         notification_collection_delay = date_collection_cleaned - date_notification_cleaned) %>%
  filter(death_collection_delay >= 0)
hist(as.numeric(df2$death_collection_delay), breaks = 15)
hist(as.numeric(df2$death_notification_delay), breaks = 15)
hist(as.numeric(df2$notification_collection_delay), breaks = 15)
mean(as.numeric(df2$death_collection_delay))
mean(as.numeric(df2$death_notification_delay), na.rm = TRUE)
median(as.numeric(df2$death_collection_delay))
median(as.numeric(df2$death_notification_delay), na.rm = TRUE)

## Fitting the time between infection and death - Gamma distribution
model <- stan_model("analyses/0_IBM_inputEstimation/models/observation_time_gamma.stan")
data_stan <- list(N = nrow(df) - 1,
                  days = as.numeric(df2$death_collection_delay) + 0.01,
                  a_1 = 1,
                  a_2 = 1,
                  b_1 = 2,
                  b_2 = 2)
fit <- sampling(model, data=data_stan, iter=2000, chains=4)
hist(rstan::extract(fit, "days_simulated")[[1]], breaks = 50)
summary(fit)
saveRDS(fit, "analyses/0_IBM_inputEstimation/outputs/deathObservation_distGamma_stanFit.rds")
hist(rgamma(10000, rstan::extract(fit, "a")[[1]], rstan::extract(fit, "b")[[1]]), breaks = 50)

## Fitting the time between infection and death - Exponential distribution
model2 <- stan_model("analyses/0_IBM_inputEstimation/models/observation_time_exp.stan")
data_stan2 <- list(N = nrow(df) - 1,
                   days = as.numeric(df2$death_collection_delay),
                   a_1 = 1)
fit2 <- sampling(model2, data = data_stan2, iter = 2000, chains = 4)
hist(rstan::extract(fit2, "days_simulated")[[1]], breaks = 50)
summary(fit2)
saveRDS(fit2, "analyses/0_IBM_inputEstimation/outputs/deathObservation_distExp_stanFit.rds")
hist(rexp(10000, rstan::extract(fit2, "a")[[1]]), breaks = 50)

## Comparing them to empirical distribution and calculating which fits better
df_sim_gamma <- tibble(obs_delay = rstan::extract(fit, "days_simulated")[[1]], iteration = seq_along(fit), type = "simulatedGamma")
df_sim_exp <- tibble(obs_delay = rstan::extract(fit2, "days_simulated")[[1]], iteration = seq_along(fit2), type = "simulatedExp")
df_empirical <- tibble(obs_delay = as.numeric(df2$death_collection_delay), iteration = seq_along(as.numeric(df2$death_collection_delay)), type = "empirical")

df_overall <- rbind(df_sim_gamma, df_sim_exp, df_empirical)
ggplot(df_overall, aes(x=obs_delay, colour=type)) +
  geom_density() +
  geom_vline(xintercept = 10, linetype=2) +
  scale_x_continuous(limits = c(0, 15)) +
  scale_color_brewer("Data", palette = "Dark2") +
  xlab("Days from death to observation") +
  ylab("Density")
