library(rstan); library(dplyr)

# read your CSV
df <- read.csv("yellow_fever_IBM_fitting/tester_ct.csv") %>%
  mutate(date = as.Date(notification_date, format = "%d/%m/%Y"))

# create 'epidemic' binary
df$epidemic <- ifelse(df$date < as.Date("2017-12-25"), 1, 0)

# create 'stage' as a numeric factor
df$stage_numeric <- as.integer(factor(df$decomposition.stage))

stan_mod <- rstan::stan_model(model_code = {
  
  "data {
    int<lower=1> N;             // Number of observations
    int<lower=1> K;             // Number of decomposition-stage categories
    int<lower=1, upper=K> stage[N];     // Stage of decomposition, as an integer 1..K
    int<lower=0, upper=1> epidemic[N];  // 1 if date < 20-Dec-2017, 0 otherwise
    real Ct[N];                  // Outcome: Ct values
  }
  
  parameters {
    real alpha;                  // Intercept
    vector[K - 1] beta_stage;    // Stage effects (with one stage as baseline)
    real beta_epidemic;          // Effect of growth vs. decline (epidemic indicator)
    real<lower=0> sigma;         // Noise scale
  }
  
  transformed parameters {
    // We will construct the linear predictor mu[i] for each observation i
    real mu[N];
    for (i in 1:N) {
      // By convention, stage=1 is the reference category -> add 0
      // If stage=i > 1, then add beta_stage[ stage[i] - 1 ]
      real stage_effect = (stage[i] == 1) ? 0 : beta_stage[ stage[i] - 1 ];
      mu[i] = alpha
      + stage_effect
      + beta_epidemic * epidemic[i];
    }
  }
  
  model {
    // -- Priors (you can tune/expand these as you like) --
    alpha ~ normal(0, 10);
    beta_stage ~ normal(0, 5);
    beta_epidemic ~ normal(0, 5);
    sigma ~ exponential(1);
    
    // -- Likelihood --
      for (i in 1:N) {
        Ct[i] ~ normal(mu[i], sigma);
      }
  }"
  
})


# then pass these into Stan:
data_list <- list(
  N = nrow(df),
  K = length(unique(df$stage_numeric)),
  stage = df$stage_numeric,
  epidemic = df$epidemic,
  Ct = df$Ct
)

fit <- sampling(
  object = stan_mod,
  data = data_list,
  chains = 1,
  iter = 500,
  warmup = 250,
  seed = 150
)
