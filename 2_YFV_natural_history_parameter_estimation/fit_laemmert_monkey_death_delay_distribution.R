library(tidyverse)
library(rstan)
options(mc.cores=4)
rstan_options(auto_write=TRUE)

# interpret (e.g.) 6th day after infection as being 5 days post-infection
days_post_infection <- c(5, 8, 2, NA, 6, 4, 3, 4, 3, 9)

df <- tibble(id=seq_along(days_post_infection),
             days_post_infection=days_post_infection) %>% 
  mutate(truncated=if_else(is.na(days_post_infection), 1, 0)) %>% 
  mutate(days_post_infection=if_else(is.na(days_post_infection), -99, days_post_infection))

model <- stan_model("src/stan/monkey_death.stan")

nreplicates <- 10000
a <- rgamma(nreplicates, 1, 0.5)
b <- rgamma(nreplicates, 1, 2)
days <- map2_dbl(a, b, ~rgamma(1, .x, .y))
g <- qplot(days) +
  scale_x_log10(limits=c(0.1, NA)) +
  xlab("Days from infection to death") +
  ylab("Count")
ggsave("outputs/monkey_death_prior_predictive.pdf")

data_stan <- list(N=nrow(df),
                  days=df$days_post_infection,
                  truncated=df$truncated,
                  days_truncated=10,
                  a_1=1,
                  a_2=0.5,
                  b_1=1,
                  b_2=2)

fit <- sampling(model, data=data_stan, iter=2000, chains=4)
saveRDS(fit, "data/processed/monkey_death_fit.rds")

fit <- readRDS("data/processed/monkey_death_fit.rds")

days <- rstan::extract(fit, "days_simulated")[[1]]
saveRDS(days, "data/processed/monkey_death_days.rds")

diags <- check_diagnostics("data/processed/monkey_death_fit.rds")
saveRDS(diags, "data/processed/monkey_death_fit_diagnostics.rds")

diags <- readRDS("data/processed/monkey_death_fit_diagnostics.rds")
if(Reduce(`+`, diags) != 0)
  stop("Monkey death Stan model not converged.")

fit <- readRDS("data/processed/monkey_death_fit.rds")

# check p values
p_min <- mean(rstan::extract(fit, "is_min")[[1]])
p_truncated <- mean(rstan::extract(fit, "is_truncated")[[1]])
saveRDS(list(min=p_min, truncated=p_truncated),
        "data/processed/monkey_death_posterior_predictive_p.rds")

# plot actual and simulated data
df <- readRDS("data/processed/laemmert.rds") %>% 
  mutate(type="actual") %>% 
  filter(!is.na(days_post_infection))

days_sim <- rstan::extract(fit, "days_simulated")[[1]]
df_sim <- tibble(days_post_infection=days_sim, iteration=seq_along(days_sim)) %>% 
  mutate(type="simulated")

df_both <- df %>% 
  bind_rows(df_sim)

g <- ggplot(df_both, aes(x=days_post_infection, colour=type)) +
  geom_density() +
  geom_vline(xintercept = 10, linetype=2) +
  scale_x_continuous(limits=c(0, 15)) +
  scale_color_brewer("Data", palette="Dark2") +
  xlab("Days from infection to death") +
  ylab("Density")

ggsave("outputs/monkey_death_posterior_predictive.pdf", g,
       width=10, height = 6)
