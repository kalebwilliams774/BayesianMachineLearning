#Exercise 5.2
# Given parameters

set.seed(123)

n_A <- n_B <- 16
mu_0 <- 75
sigma_0 <- 10
y_bar_A <- 75.2
s_A <- 7.3
y_bar_B <- 77.5
s_B <- 8.1
kappa_nu_values <- c(1, 2, 4, 8, 16, 32)

# Function to calculate Pr(theta_A < theta_B) based on Monte Carlo sampling
calculate_probability <- function(kappa_nu) {
  kappa_0 <- nu_0 <- kappa_nu
  
  # Number of Monte Carlo samples
  num_samples <- 10000
  
  # Generate samples from the normal distribution for both groups
  theta_A_samples <- rnorm(num_samples, (kappa_0*mu_0+n_A*y_bar_A)/(kappa_0+n_A), (1/(kappa_0+n_A))*(kappa_0*sigma_0^2+(n_A-1)*s_A^2+(kappa_0*n_A)/(kappa_0+n_A)*(y_bar_A-mu_0)^2))
  
  theta_B_samples <- rnorm(num_samples, (kappa_0*mu_0+n_B*y_bar_B)/(kappa_0+n_A), (1/(kappa_0+n_A))*(kappa_0*sigma_0^2+(n_B-1)*s_A^2+(kappa_0*n_B)/(kappa_0+n_B)*(y_bar_B-mu_0)^2))
  
  # Calculate the difference between samples
  diff_samples <- theta_A_samples - theta_B_samples
  
  # Calculate the probability that theta_A is less than theta_B
  prob <- mean(diff_samples < 0)
  
  return(prob)
}

# Calculate probabilities for different (kappa_0 = nu_0) values
probabilities <- sapply(kappa_nu_values, calculate_probability)

#Plot the probabilities
plot(kappa_nu_values, probabilities, type = 'o', xlab = 'kappa_0 = nu_0', ylab = 'Pr(theta_A < theta_B)')
grid()