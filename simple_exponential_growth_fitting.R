horto_df <- readRDS("data/processed_HortoData.rds") %>%
  filter(!is.na(zone_peal)) %>%
  filter(final_yfv_result != "negative")
epi_curve <- incidence::incidence(horto_df$date_collection)
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

tau <- log(2) / results_df$b

plot(x[63:80])

exp(results_df$b * 19)
(1 + results_df$b * 19)
(1 + results_df$b * 17) * (1 + results_df$b * 2)

1 + (log(2) * 19) / 6.217760
### all of this is implying the R0 is lower - doubling time of approx 6ish days.
### With Tg of 19 days (exponentially distributed, as in SIR model), implies R0 of 3ish.




