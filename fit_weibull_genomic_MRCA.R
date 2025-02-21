fit_weibull_3quantiles <- function(q025_target = 14, 
                                   q50_target = 29, 
                                   q975_target = 36) {
  
  # Objective: sum of squared differences from target quantiles
  obj_fun <- function(log_par) {
    # Exponentiate to ensure positivity
    shape <- exp(log_par[1])
    scale <- exp(log_par[2])
    
    # Calculate quantiles
    q025  <- qweibull(0.025, shape = shape, scale = scale)
    q50   <- qweibull(0.50,  shape = shape, scale = scale)
    q975  <- qweibull(0.975, shape = shape, scale = scale)
    
    # Sum of squared errors
    sse <- (q025 - q025_target)^2 + 
      (q50  - q50_target)^2  + 
      (q975 - q975_target)^2
    
    return(sse)
  }
  
  init_guess <- c(log(1.5), log(20))  # a rough guess
  fit <- optim(par = init_guess, fn = obj_fun, method = "BFGS")
  
  shape_hat <- exp(fit$par[1])
  scale_hat <- exp(fit$par[2])
  
  # Inspect final quantiles
  q025_hat  <- qweibull(0.025, shape = shape_hat, scale = scale_hat)
  q50_hat   <- qweibull(0.50,  shape = shape_hat, scale = scale_hat)
  q975_hat  <- qweibull(0.975, shape = shape_hat, scale = scale_hat)
  
  return(list(
    shape    = shape_hat,
    scale    = scale_hat,
    sse      = fit$value,
    q025_fit = q025_hat,
    q50_fit  = q50_hat,
    q975_fit = q975_hat,
    converged = fit$convergence == 0
  ))
}

## Example usage
weibull_fit <- fit_weibull_3quantiles()
weibull_fit

x <- rweibull(10000, shape = weibull_fit$shape, scale = weibull_fit$scale)
hist(x, breaks = 20)
sum(x < 14)
sum(x < 29)
sum(x < 36)
