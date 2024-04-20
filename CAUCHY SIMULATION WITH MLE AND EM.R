# Function to calculate negative log-likelihood of Cauchy distribution
neg_log_likelihood_cauchy <- function(params, data) {
  # Extract parameters
  location <- params[1]
  scale <- params[2]
  
  # Calculate log likelihood
  ll <- sum(log(dcauchy(data, location = location, scale = scale)))
  
  # Return negative log likelihood
  return(-ll)
}

# Generate data with heavier tails using a different distribution
heavy_tails_data <- rcauchy(1000, location = 1, scale = 1)

# Perform MLE using optim() with different initial parameters and optimization method
mle_result <- optim(par = c(0, 1), fn = neg_log_likelihood_cauchy, data = heavy_tails_data, method = "L-BFGS-B")

# Perform EM using normalmixEM() function from mixtools package
em_result <- normalmixEM(heavy_tails_data, k = 2, maxit = 3000)

# Print MLE and EM parameter estimates
cat("MLE Parameter Estimates:\n")
cat("Mean:", mle_result$par[1], "\n")
cat("Scale:", mle_result$par[2], "\n\n")

cat("EM Parameter Estimates:\n")
print(summary(em_result))