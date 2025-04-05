x <- as.vector(epi_curve$counts)[1:80]

results_list <- lapply(1:64, function(n) {
  tryCatch({
    # Remove the first n points
    y <- x[-(1:n)]
    
    # Times: t = 1,2,3,...
    t <- seq_along(y)
    
    # Fit y(t) = exp(b * t)
    fit <- nls(y ~ exp(b * t), start = list(b = 0.1))
    
    # Extract b
    b_est <- coef(fit)["b"]
    
    data.frame(
      n       = n,
      b       = b_est,
      n_points = length(y)  # how many data points we actually fit
    )
  }, error = function(e) {
    # If fitting fails, return NA for b
    data.frame(
      n       = n,
      b       = NA,
      n_points = length(x) - n
    )
  })
})

# Combine results into one data frame
results_df <- do.call(rbind, results_list)
head(results_df)

exp(results_df$b * 19)


