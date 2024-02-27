#Exercise 6.1

set.seed(123)

#Data for men over 30 with a bachelors degree ya, and data for men over 30
#With children without a bachelors degree yb 

ya <- c(1, 0, 0, 1, 2, 2, 1, 5, 2, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0 ,1, 1, 
        2, 1, 3, 2, 0, 0, 3, 0, 0, 0, 2, 1, 0, 2, 1,
        0, 0, 1, 3, 0, 1, 1, 0, 2, 0, 0, 2, 2, 1, 3, 0, 0, 
        0, 1, 1)

yb <- c(2, 2, 1, 1, 2, 2, 1, 2, 1, 0, 2, 1, 1, 2, 0, 2, 2, 0, 2, 1, 0, 0, 3, 6,
        1, 6, 4, 0, 3, 2, 0, 1, 0, 0, 0, 3, 0, 0, 0, 0, 0, 1, 0, 4, 2, 1, 0, 0, 
        1, 0, 3, 2, 5, 0, 1, 1, 2, 1, 2, 1, 2, 0, 0, 0, 2, 1, 0, 2, 0, 2, 4, 1,
        1, 1, 2, 0, 1, 1, 1, 1, 0, 2, 3, 2, 0, 2, 1, 3, 1, 3, 2, 2, 3, 2, 0, 0, 
        0, 1, 0, 0, 0, 1, 2, 0, 3, 3, 0, 1, 2, 2, 2, 0, 6, 0, 0, 0, 2, 0, 1, 1,
        1, 3, 3, 2, 1, 1, 0, 1, 0, 0, 2, 0, 2, 0, 1, 0, 2, 0, 0, 2, 2, 4, 1, 2, 
        3, 2, 0, 0, 0, 1, 0, 0, 1, 5, 2, 1, 3, 2, 0, 2, 1, 1, 3, 0, 5, 0, 0, 2, 
        4, 3, 4, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 2, 0, 0, 1, 1, 0, 2, 1, 3, 3, 2, 
        2, 0, 0, 2, 3, 2, 4, 3, 3, 4, 0, 3, 0, 1, 0, 1, 2, 3, 4, 1, 2, 6, 2, 1, 
        2, 2)

a_theta = 2; b_theta = 1
a_gamma_values = c(8,6,32,64,128);b_gamma_values = c(8,6,32,64,128)

sum_a = sum(ya)
sum_b = sum(yb)

n_a = length(ya)
n_b = length(yb)

num_iterations = 5000

#Applying a Gibbs sampler for both theta and gamma in order to generate samples

shape_theta = sum_a + sum_b + a_theta
rate_theta = n_a+n_b+b_theta

shape_gamma = sum_b + a_gamma 
rate_gamma = b_gamma + n_b

#Given that theta is a Gamma distribution and gamma is a Gamma distribution our
#Initial guesses can be 2 and 1 respectively

PHI <- matrix(nrow=num_iterations , ncol =2)
PHI[1 ,] <- phi <- c(a_theta/b_theta, 1)

for (i in seq_along(a_gamma_values)) {
  a_gamma <- a_gamma_values[i]
  b_gamma <- b_gamma_values[i]
  
  # Initialize matrix to store samples for this iteration
  PHI <- matrix(nrow=num_iterations, ncol=2)
  
  # Set initial values
  PHI[1,] <- c(a_theta/b_theta, 1)
  
  # Gibbs Sampling
  for (s in 2:num_iterations) {
    # Sample theta given gamma
    shape_theta <- sum_a + sum_b + PHI[s-1, 1]
    rate_theta <- n_a + n_b + b_theta
    theta <- rgamma(1, shape=shape_theta, rate=rate_theta)
    
    # Sample gamma given theta
    shape_gamma <- sum_b + PHI[s-1, 1]
    rate_gamma <- b_gamma + n_b
    gamma <- rgamma(1, shape=shape_gamma, rate=rate_gamma)
    
    # Store samples
    PHI[s,] <- c(theta, gamma)
  }
  
  # Calculate and print expected difference
  expected_difference <- mean(PHI[, 2] * PHI[, 1] - PHI[, 1])
  cat("For a_gamma =", a_gamma, "and b_gamma =", b_gamma, "\n")
  cat("Expected difference (θB - θA):", expected_difference, "\n\n")
}

