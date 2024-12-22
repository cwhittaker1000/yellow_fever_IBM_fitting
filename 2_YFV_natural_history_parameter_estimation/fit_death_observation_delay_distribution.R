# fits time from death to corpse observed

library(tidyverse)
library(rstan)
options(mc.cores=4)
rstan_options(auto_write=TRUE)

df <- readxl::read_xlsx("data/raw/EPIZOOTIAS_HORTO _SP_2017.xlsx") %>% 
  mutate(date_corpse_reported=as.Date(Death_repoorting)) %>% 
  select(-Death_repoorting)

# remove alive monkey
df <- df %>% 
  filter(decomposition != "Alive")

# convert unknown days_since_death to medians
df <- df %>% 
  mutate(days_since_death=as.numeric(days_since_death))
median_days <- median(df$days_since_death, na.rm = T)
df <- df %>% 
  mutate(days_since_death_interpolated=if_else(is.na(days_since_death), 1, 0)) %>% 
  mutate(days_since_death=if_else(is.na(days_since_death), median_days, days_since_death)) %>% 
  mutate(date_death_estimated=date_corpse_reported-days_since_death)

saveRDS(df, "data/processed/cleaned_data.rds")


df <- readRDS("data/processed/cleaned_data.rds")

# fit only uninterpolated data
df <- df %>% 
  filter(days_since_death_interpolated != 1)

model <- stan_model("src/stan/observation_time.stan")

data_stan <- list(N=nrow(df),
                  days=df$days_since_death,
                  a_1=2, a_2=1,
                  b_1=3, b_2=3)

fit <- sampling(model, data=data_stan, iter=2000, chains=4)

saveRDS(fit, "data/processed/observation_time_fit.rds")

diags <- check_diagnostics("data/processed/observation_time_fit.rds")
saveRDS(diags, "data/processed/observation_time_fit_diagnostics.rds")

nreplicates <- 10000
a <- rgamma(nreplicates, 2, 1)
b <- rgamma(nreplicates, 3, 3)
days <- map2_dbl(a, b, ~rgamma(1, .x, .y))
g <- qplot(days) +
  scale_x_log10(limits=c(0.1, NA)) +
  xlab("Days from death to corpse observation") +
  ylab("Count")
ggsave("outputs/observation_time_prior_predictive.pdf", g,
       width = 10, height = 6)

diags <- readRDS("data/processed/observation_time_fit_diagnostics.rds")
if(Reduce(`+`, diags) != 0)
  stop("Observation time Stan model not converged.")

fit <- readRDS("data/processed/observation_time_fit.rds")

# check p values -- not sure how useful these are: i) 0.5 days is
# probably the least time an observer would say and ii) lots of
# corpses weren't observed so the max in the data is likely a massive
# understatement. This is probably why the p values are near 1
p_min <- mean(rstan::extract(fit, "is_min")[[1]])
p_max <- mean(rstan::extract(fit, "is_max")[[1]])
saveRDS(list(min=p_min, max=p_max),
        "data/processed/observation_time_posterior_predictive_p.rds")

# plot actual and simulated data
df <- readRDS("data/processed/cleaned_data.rds") %>% 
  filter(days_since_death_interpolated != 1) %>% 
  mutate(type="actual")

days_sim <- rstan::extract(fit, "days_simulated")[[1]]
df_sim <- tibble(days_since_death=days_sim, iteration=seq_along(days_sim)) %>% 
  mutate(type="simulated")

df_both <- df %>% 
  bind_rows(df_sim)

g <- ggplot(df_both %>% filter(type=="simulated"),
            aes(x=days_since_death)) +
  geom_histogram(data=df_both %>% filter(type=="actual"),
                 aes(y = ..density..), fill="blue") +
  geom_density() +
  scale_x_continuous(limits=c(0, 15)) +
  scale_color_brewer("Data", palette="Dark2") +
  xlab("Days from death to corpse observation") +
  ylab("Density")

ggsave("outputs/observation_time_posterior_predictive.pdf", g,
       width=10, height = 6)