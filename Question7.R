#Question 7

#We will use a binomial sampling and a beta conjugate prior

#We will use alpha = 0.5, beta = 0.5
#We will use small sample size of 10 and 2 successes as binomial parameters

# Initialize vectors to store upper and lower portions of credible intervals
credible_upper_middle <- c()
credible_lower_middle <- c()

credible_upper_tail <- c()
credible_lower_tail <- c()

calculate_credible_interval <- function(n, k, alpha, beta, confidence_level) {
  # Update posterior parameters
  alpha_posterior <- alpha + k
  beta_posterior <- beta + n - k
  
  # Calculate quantiles of the posterior distribution
  lower_quantile <- qbeta((1 - confidence_level) / 2, alpha_posterior, beta_posterior)
  upper_quantile <- qbeta(1 - (1 - confidence_level) / 2, alpha_posterior, beta_posterior)
  
  # Calculate credible interval
  credible_interval <- c(lower_quantile, upper_quantile)
  
  return(credible_interval)
}

# Set parameters
n <- 10  # Sample size
k <- 2   # Number of successes
alpha_prior <- 0.5  # Shape parameter of the prior
beta_prior <- 0.5   # Shape parameter of the prior
confidence_level <- 0.8  # Confidence level for credible interval

# Generate datasets with true parameter values in the tail and middle of the prior
true_values <- c(0.10, 0.50, 0.90)  # True parameter values

# Number of data sets we wish to generate
num_datasets <- 1000

#Initializing counter variables
coverage_middle <-0
coverage_tail <- 0

# Generate datasets and calculate credible intervals
for (true_value in true_values) {
  for (i in 1:num_datasets) {
    # Setting seed to change every iteration
    set.seed(i)
    
    # Generate dataset
    data <- rbinom(n, 1, true_value)
    
    # Calculate credible interval
    result <- calculate_credible_interval(n, sum(data), alpha_prior, beta_prior, confidence_level)
    credible_interval <- result
    
    # Check if true value is in the credible interval
    if (true_value < 0.2 || true_value > 0.8) {
      if (credible_interval[1] <= true_value && credible_interval[2] >= true_value) {
        coverage_middle <- coverage_middle + 1
        credible_upper_middle <- c(credible_upper_middle, credible_interval[2])
        credible_lower_middle <- c(credible_lower_middle, credible_interval[1])
      }
    } else {
      if (credible_interval[1] <= true_value && credible_interval[2] >= true_value) {
        coverage_tail <- coverage_tail + 1
        credible_upper_tail <- c(credible_upper_tail, credible_interval[2])
        credible_lower_tail <- c(credible_lower_tail, credible_interval[1])
      }
    }
  }
}

proportion_coverage_tail <- coverage_tail / (num_datasets * length(true_values))
proportion_coverage_middle <- coverage_middle / (num_datasets * length(true_values))

proportion_coverage_tail

proportion_coverage_middle

#Variability in credible intervals

upper_credible_middle_mean = mean(credible_upper_middle)
lower_credible_middle_mean = mean(credible_lower_middle)

upper_credible_tail_mean = mean(credible_upper_tail)
lower_credible_tail_mean = mean(credible_lower_tail)

upper_credible_middle_var = var(credible_upper_middle)
lower_credible_middle_var = var(credible_lower_middle)

upper_credible_tail_var = var(credible_upper_tail)
lower_credible_tail_var = var(credible_lower_tail)

variability_df1 <- data.frame('Names'=c('Upper Middle Mean','Lower Middle Mean','Upper Tail Mean','Lower Tail Mean','Upper Middle Variance','Lower Middle Variance','Upper Tail Variance','Lower Tail Variance'),
                              'Values'=c(upper_credible_middle_mean,lower_credible_middle_mean,upper_credible_tail_mean,lower_credible_middle_mean,upper_credible_middle_var,lower_credible_middle_var,upper_credible_tail_var,lower_credible_tail_var))


#b) 

# Initialize counter for coverage analysis
coverage_prior_sampling <- 0

# Generate datasets and calculate credible intervals using prior sampling
for (i in 1:num_datasets) {
  # Setting seed to change every iteration
  set.seed(i)
  
  # Sample parameter value from the prior distribution
  theta <- rbeta(1, alpha_prior, beta_prior)
  
  # Generate dataset
  data <- rbinom(n, 1, theta)
  
  # Calculate credible interval using the sampled parameter value
  result <- calculate_credible_interval(n, sum(data), alpha_prior, beta_prior, confidence_level)
  credible_interval <- result
  
  # Check if the sampled parameter value is in the credible interval
  if (credible_interval[1] <= theta && credible_interval[2] >= theta) {
    coverage_prior_sampling <- coverage_prior_sampling + 1
  }
}

# Calculate proportion of coverage
proportion_coverage_prior_sampling <- coverage_prior_sampling / num_datasets

proportion_coverage_prior_sampling

