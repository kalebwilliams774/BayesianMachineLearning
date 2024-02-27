#Question 8

# Set parameters
theta_true <- 0  # True parameter value
k_squared <- 1    # Variance of nature's prior N(0, k^2)
tau_squared_values <- seq(0.1, 10, by = 0.1)  # Values of tau^2 to investigate
num_samples <- 1000  # Number of samples to draw for each tau^2

# Initialize vector to store Bayes risk estimates
bayes_risk <- numeric(length(tau_squared_values))

# Simulate and estimate Bayes risk for each value of tau^2
for (i in 1:length(tau_squared_values)) {
  
  set.seed(i)
  
  tau_squared <- tau_squared_values[i]
  
  # Generate observations from Y ~ N(theta, 1)
  Y <- rnorm(n = num_samples, mean = theta_true, sd = 1)
  
  # Calculate posterior mean E(theta|Y) for each observation
  posterior_mean <- (tau_squared / (1 + tau_squared)) * Y
  
  # Calculate squared differences between true parameter value and posterior mean
  squared_diff <- (theta_true - posterior_mean)^2
  
  # Calculate average squared difference (empirical Bayes risk)
  bayes_risk[i] <- mean(squared_diff)
}

# Plot the Bayes risk as a function of tau^2
plot(tau_squared_values, bayes_risk, type = "l", xlab = expression(tau^2), ylab = "Bayes Risk (MSE)",
     main = "Bayes Risk vs. Prior Variance")

png('BayesRisk.png')