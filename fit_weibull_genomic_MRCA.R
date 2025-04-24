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
start_date_weibull_fitting <- as.Date("2017-10-09")
lower <- as.Date("2017-10-30") - 5
median <- as.Date("2017-11-19") - 5
upper <-  as.Date("2017-11-25") - 5

weibull_fit <- fit_weibull_3quantiles(q025_target = as.numeric(lower - start_date_weibull_fitting),
                                      q50_target = as.numeric(median - start_date_weibull_fitting),  
                                      q975_target = as.numeric(upper - start_date_weibull_fitting))
weibull_shape <- weibull_fit$shape
weibull_scale <- weibull_fit$scale

# x <- rweibull(10000, shape = weibull_shape, scale = weibull_scale)
# hist(x - 5, breaks = 20)
# sum(x < 16)
# sum(x < 36)
# sum(x < 42)


# ### Gamma
# fit_gamma_3quantiles <- function(q025_target = 16,
#                                  q50_target  = 36,
#                                  q975_target = 42) {
#   
#   obj_fun <- function(log_par) {
#     shape <- exp(log_par[1])
#     scale <- exp(log_par[2])
#     
#     # quick guard against extreme values
#     if (shape <= 0 || scale <= 0) return(1e12)
#     
#     # compute all three quantiles at once, suppressing warnings
#     qs <- suppressWarnings(
#       qgamma(c(0.025, 0.50, 0.975),
#              shape = shape,
#              scale = scale)
#     )
#     
#     # if any NaN slipped through, give a big penalty
#     if (any(is.nan(qs))) return(1e12)
#     
#     # sum of squared errors
#     targets <- c(q025_target, q50_target, q975_target)
#     sum((qs - targets)^2)
#   }
#   
#   # reasonable starting point on the log scale
#   init_guess <- c(log(40), log(2))
#   
#   fit <- optim(
#     par    = init_guess,
#     fn     = obj_fun,
#     method = "L-BFGS-B",
#     lower  = c(log(1e-8), log(1e-8)),   # shape,scale ≥ 1e-8
#     upper  = c(log(1e8),  log(1e8))     # shape,scale ≤ 1e8
#   )
#   
#   shape_hat <- exp(fit$par[1])
#   scale_hat <- exp(fit$par[2])
#   
#   # recompute fitted quantiles
#   q_fit <- qgamma(c(0.025, 0.50, 0.975),
#                   shape = shape_hat,
#                   scale = scale_hat)
#   
#   list(
#     shape     = shape_hat,
#     scale     = scale_hat,
#     sse       = fit$value,
#     q025_fit  = q_fit[1],
#     q50_fit   = q_fit[2],
#     q975_fit  = q_fit[3],
#     converged = (fit$convergence == 0)
#   )
# }
# 
# 
# gamma_fit <- fit_gamma_3quantiles(q025_target = as.numeric(lower - start_date),
#                                   q50_target = as.numeric(median - start_date),  
#                                   q975_target = as.numeric(upper - start_date))
# gamma_fit
# 
# 
# ####
# fit_gamma_3quantiles <- function(q025, q50, q975) {
#   # target ratio of upper to lower quantile
#   target_ratio <- q975 / q025
#   
#   # function of shape (alpha) only: want
#   #   qgamma(0.975,shape,1) / qgamma(0.025,shape,1) == target_ratio
#   ratio_fn <- function(shape) {
#     q975_std <- qgamma(0.975, shape=shape, scale=1)
#     q025_std <- qgamma(0.025, shape=shape, scale=1)
#     q975_std / q025_std - target_ratio
#   }
#   
#   # find shape in a reasonable bracket
#   # (tweak lower/upper if your data are extreme)
#   root <- uniroot(ratio_fn, lower=0.1, upper=200)
#   shape_hat <- root$root
#   
#   # now pick scale to hit the median exactly
#   med_std <- qgamma(0.5, shape=shape_hat, scale=1)
#   scale_hat <- q50 / med_std
#   
#   # compute fitted quantiles
#   q025_fit <- qgamma(0.025, shape=shape_hat, scale=scale_hat)
#   q50_fit  <- qgamma(0.50,  shape=shape_hat, scale=scale_hat)
#   q975_fit <- qgamma(0.975, shape=shape_hat, scale=scale_hat)
#   
#   list(
#     shape     = shape_hat,
#     scale     = scale_hat,
#     q025_fit  = q025_fit,
#     q50_fit   = q50_fit,
#     q975_fit  = q975_fit,
#     converged = TRUE
#   )
# }
# res <- fit_gamma_3quantiles(q025 = 16, q50 = 36, q975 = 42)
# print(res)
# 
# x <- rgamma(10000, shape = res$shape, scale = res$scale)
# hist(x, breaks = 20)
# sum(x < 16)
# sum(x < 36)
# sum(x < 42)
# 
# #####
# fit_lognormal_3quantiles <- function(q025_target = 14,
#                                      q50_target  = 29,
#                                      q975_target = 36) {
#   
#   # Objective: SSE between target and fitted quantiles
#   obj_fun <- function(par) {
#     meanlog <- par[1]
#     sdlog   <- exp(par[2])  # ensure positivity
#     
#     # guard against zero/NaN
#     if (sdlog <= 0) return(1e12)
#     
#     qs <- suppressWarnings(
#       qlnorm(c(0.025, 0.50, 0.975),
#              meanlog = meanlog,
#              sdlog   = sdlog)
#     )
#     if (any(is.nan(qs))) return(1e12)
#     
#     targets <- c(q025_target, q50_target, q975_target)
#     sum((qs - targets)^2)
#   }
#   
#   # start at log(median), sdlog=1
#   init_par <- c(log(q50_target), log(1))
#   
#   fit <- optim(
#     par    = init_par,
#     fn     = obj_fun,
#     method = "L-BFGS-B",
#     lower  = c(-Inf, log(1e-8)),   # sdlog ≥ 1e-8
#     upper  = c( Inf, log(1e8) )     # sdlog ≤ 1e8
#   )
#   
#   meanlog_hat <- fit$par[1]
#   sdlog_hat   <- exp(fit$par[2])
#   qs_fit      <- qlnorm(c(0.025, 0.50, 0.975),
#                         meanlog = meanlog_hat,
#                         sdlog   = sdlog_hat)
#   
#   list(
#     meanlog    = meanlog_hat,
#     sdlog      = sdlog_hat,
#     sse        = fit$value,
#     q025_fit   = qs_fit[1],
#     q50_fit    = qs_fit[2],
#     q975_fit   = qs_fit[3],
#     converged  = (fit$convergence == 0)
#   )
# }
# res <- fit_lognormal_3quantiles(q025_target = 16,
#                                 q50_target  = 36,
#                                 q975_target = 42)
# print(res)
# 
# x <- rlnorm(10000, meanlog = res$meanlog, sdlog = res$sdlog)
# hist(x, breaks = 20)
# sum(x < 16)
# sum(x < 36)
# sum(x < 42)
# 
# fit_gev_3quantiles <- function(q025_target,
#                                q50_target,
#                                q975_target) {
#   
#   # GEV quantile function
#   q_gev <- function(p, loc, scale, shape) {
#     if (abs(shape) < 1e-8) {
#       # limit as shape→0 → Gumbel
#       loc - scale * log(-log(p))
#     } else {
#       loc + scale/shape * ((-log(p))^(-shape) - 1)
#     }
#   }
#   
#   # Objective: SSE between model and target quantiles
#   obj_fun <- function(par) {
#     loc   <- par[1]
#     scale <- exp(par[2])   # ensure >0
#     shape <- par[3]
#     
#     # compute the three quantiles
#     qs <- c(
#       q_gev(0.025, loc, scale, shape),
#       q_gev(0.50,  loc, scale, shape),
#       q_gev(0.975, loc, scale, shape)
#     )
#     
#     # if any NaN/Inf, penalize heavily
#     if (any(!is.finite(qs))) return(1e12)
#     
#     sum((qs - c(q025_target, q50_target, q975_target))^2)
#   }
#   
#   # sensible starting values
#   init_loc   <- q50_target
#   init_scale <- log((q975_target - q025_target) / 2)
#   init_shape <- 0.1
#   
#   init_par <- c(init_loc, init_scale, init_shape)
#   
#   fit <- optim(
#     par    = init_par,
#     fn     = obj_fun,
#     method = "L-BFGS-B",
#     lower  = c(-Inf, log(1e-8), -5),  # loc unbounded, scale ≥1e-8, shape ≥ -5
#     upper  = c( Inf, log(1e8),  5)   # scale ≤1e8, shape ≤ 5
#   )
#   
#   # back‐transform and compute fitted quantiles
#   loc_hat   <- fit$par[1]
#   scale_hat <- exp(fit$par[2])
#   shape_hat <- fit$par[3]
#   
#   q_fit <- c(
#     q_gev(0.025, loc_hat, scale_hat, shape_hat),
#     q_gev(0.50,  loc_hat, scale_hat, shape_hat),
#     q_gev(0.975, loc_hat, scale_hat, shape_hat)
#   )
#   
#   list(
#     location   = loc_hat,
#     scale      = scale_hat,
#     shape      = shape_hat,
#     q025_fit   = q_fit[1],
#     q50_fit    = q_fit[2],
#     q975_fit   = q_fit[3],
#     sse        = fit$value,
#     converged  = (fit$convergence == 0)
#   )
# }
# res <- fit_gev_3quantiles(q025_target = 16,
#                           q50_target  = 36,
#                           q975_target = 42)
# print(res)
# 
# # (re)define the GEV quantile function
# q_gev <- function(p, loc, scale, shape) {
#   if (abs(shape) < 1e-8) {
#     loc - scale * log(-log(p))
#   } else {
#     loc + scale/shape * ((-log(p))^(-shape) - 1)
#   }
# }
# 
# # sampler via inverse‐CDF
# rgev_custom <- function(n, loc, scale, shape) {
#   u <- runif(n)
#   q_gev(u, loc, scale, shape)
# }
# 
# # draw 10k samples
# x <- rgev_custom(
#   n     = 10000,
#   loc   = res$location,
#   scale = res$scale,
#   shape = res$shape
# )
# 
# 
# x <- rgev(
#   n     = 100000,
#   loc   = res$location,
#   scale = res$scale,
#   shape = res$shape
# )
# 
# hist(x, breaks = 100, main = "Samples from fitted GEV (evd::rgev)")
# sum(x < 16)
# sum(x < 36)
# sum(x < 42)
# # quick histogram and tail counts
# hist(x, breaks = 50, main = "Samples from fitted GEV")
# sum(x < 16)   # count below 16
# sum(x < 36)   # count below 36
# sum(x < 42)   # count below 42
