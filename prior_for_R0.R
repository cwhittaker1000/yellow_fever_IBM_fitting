# Method 1: Using basic R functions
fit_lognormal <- function(data) {
  # Ensure all values are positive
  if (any(data <= 0)) {
    stop("All values must be positive for a lognormal distribution.")
  }
  
  # Take the logarithm of the data
  log_data <- log(data)
  
  # The MLE for mu and sigma are the mean and standard deviation of the log-transformed data
  mu_hat <- mean(log_data)
  sigma_hat <- sd(log_data)
  
  return(list(mu = mu_hat, sigma = sigma_hat))
}

# Example usage:
data_vector <- c(6, 4.8, 5.2, 7.1, 11, 4.1, 2.38, 3.59, 3.23, 4.21, 1.35)
fit1 <- fit_lognormal(data_vector)
print(fit1)
hist(rlnorm(n = 10000, meanlog = fit1$mu, sdlog = fit1$sigma), breaks = 40)

dlnorm(10, meanlog = fit1$mu, sdlog = fit1$sigma, log = TRUE)
dlnorm(18, meanlog = fit1$mu, sdlog = fit1$sigma, log = TRUE)

# ---------------------------------------------------

# Method 2: Using the MASS package's fitdistr function
# Uncomment the following lines if you want to use this method.
# install.packages("MASS")  # Uncomment if MASS is not already installed
library(MASS)

# MASS's fitdistr returns estimates for "meanlog" and "sdlog"
fit2 <- fitdistr(data_vector, densfun = "lognormal")
print(fit2)
hist(rlnorm(n = 10000, meanlog = fit2$estimate[1], sdlog = fit2$estimate[2]), breaks = 40)
dlnorm(10, meanlog = fit2$estimate[1], sdlog = fit2$estimate[2], log = TRUE)
dlnorm(18, meanlog = fit2$estimate[1], sdlog = fit2$estimate[2], log = TRUE)

# MASS's fitdistr returns estimates for "meanlog" and "sdlog"
fit3 <- fitdistr(data_vector, densfun = "gamma")
print(fit3)
hist(rgamma(n = 10000, shape = fit3$estimate[1], rate = fit3$estimate[2]), breaks = 40)
dgamma(10, shape = fit3$estimate[1], rate = fit3$estimate[2], log = TRUE)
dgamma(18, shape = fit3$estimate[1], rate = fit3$estimate[2], log = TRUE)

# Install and load the required package
# install.packages("truncnorm")  # Uncomment if not already installed
library(truncnorm)

# Negative log-likelihood function for the truncated normal distribution
neg_log_lik <- function(params, data, a, b) {
  mu <- params[1]
  sigma <- params[2]
  # Ensure sigma is positive
  if (sigma <= 0) return(Inf)
  
  # dtruncnorm gives the density of the truncated normal distribution
  # We sum the negative log densities for all observations
  -sum(log(dtruncnorm(data, a = a, b = b, mean = mu, sd = sigma)))
}

# Example data: replace this vector with your actual data
data_vector <- c(6, 4.8, 5.2, 7.1, 11, 4.1, 2.38, 3.59, 3.23, 4.21, 1.35)

# Specify truncation bounds (for example, only values between 0 and 5)
a <- 1
b <- 18

# Provide initial estimates for mu and sigma based on the data
init_mu <- mean(data_vector)
init_sigma <- sd(data_vector)
init_params <- c(init_mu, init_sigma)

# Use optim to minimize the negative log-likelihood
fit <- optim(
  par = init_params,
  fn = neg_log_lik,
  data = data_vector,
  a = a,
  b = b,
  method = "L-BFGS-B",
  lower = c(-Inf, 1e-6)  # sigma must be positive
)

# Extract the fitted parameters
fitted_mu <- fit$par[1]
fitted_sigma <- fit$par[2]

cat("Fitted truncated normal parameters:\n")
cat("mu =", fitted_mu, "\n")
cat("sigma =", fitted_sigma, "\n")

hist(rtruncnorm(10000, a = a, b = b, mean = fitted_mu, sd = fitted_sigma), breaks = 40)
log(dtruncnorm(10, a = a, b = b, mean = fitted_mu, sd = fitted_sigma))
log(dtruncnorm(16, a = a, b = b, mean = fitted_mu, sd = fitted_sigma))
