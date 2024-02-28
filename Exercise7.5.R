#Exercise 7.5
#a)

yA <- c(25.33, 26.77, 22.76, 20.94, 25.40, 22.49, 24.54, 20.40, 21.85, 22.21, 
        26.21, 22.07, 29.09, 25.00, 22.96, 22.13, 22.91, 25.04, 27.43, 24.34, 
        24.08, 25.71, 21.58, 22.13, 26.28, 28.02, 25.24, 24.80, 22.41, 26.08, 
        24.29, 25.52, 23.18, 26.67, 24.39, 21.94, 24.93, 22.23, 23.48, 23.48, 
        25.91,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)

yB <- c(26.45, 27.53, 20.02, 22.83, 28.05, 23.67, 23.25, 21.38, 24.39, 23.77, 
        27.35, 21.44, 27.14, 27.35, 26.61, 25.12, 26.64, 23.01, 27.00, 25.36, 
        24.63, 26.77, 23.06, 22.54, 24.36, 24.14,NA, NA, NA, NA, NA, NA, NA, NA, 
        NA, NA, NA, NA, NA, NA, NA, 27.48, 20.32, 24.47, 24.99, 25.41, 27.81, 
        26.24, 26.08, 24.67, 23.74, 26.23, 26.89, 22.62, 21.19, 26.21, 26.06, 22.36)

#Calculating empirical estimates for mean, variance and correlation for both data sets

theta_A = mean(yA, na.rm = TRUE)
theta_B = mean(yB,na.rm=TRUE)

var_A = var(yA,na.rm=TRUE)
var_B = var(yB,na.rm = TRUE)

#Need to remove the NA values from the data sets

complete_cases <- complete.cases(yA, yB)
yA_complete <- yA[complete_cases]
yB_complete <- yB[complete_cases]

rho <- cor(yA_complete, yB_complete)


#Creating functions to impute the missing data

impute_A <- function(yB, theta_A, theta_B, rho, var_A, var_B) {
  missing_A <- is.na(yA)
  imputed_A <- theta_A + (yB[missing_A] - theta_B)*rho*sqrt(var_A/var_B)
  return(imputed_A)
}

impute_B <- function(yA, theta_A, theta_B, rho, var_A, var_B) {
  missing_B <- is.na(yB)
  imputed_B<-theta_B + (yA[missing_B]-theta_A)*rho*sqrt(var_B/var_A)
  return(imputed_B)
}



#Calculating imputed values 

yA_imputed <- impute_A(yB, theta_A, theta_B, var_A, var_B, rho)

yB_imputed <- impute_B(yA, theta_A, theta_B, var_A, var_B, rho)

for (i in length(yA_imputed))
{
  yA[is.na(yA)] <- yA_imputed
}

for(i in length(yB_imputed))
{
  yB[is.na(yB)] <- yB_imputed
}

#t-test 

t.test(yA,yB,paired = TRUE)

#Gibbs sampling using Jeffrey's prior
#First must calculate the parameters for each of the posterior distributions for theta and sigma^2

mu0<-c ( 50 , 50 )
L0<-matrix (c(625, 312.5 , 312.5 , 625 ), nrow=2, ncol =2)
nu0<-4
S0<-matrix(c(625 , 312.5 , 312.5 , 625) , nrow=2, ncol =2)

Y <- matrix(nrow=length(yA),ncol=2)

Y[,1] = yA
Y[,2] = yB

library(MASS) # For rmvnorm and rwish functions
library(MCMCpack)
library(abind)


# Define prior parameters
mu0 <- rep(0, ncol(Y))  # Prior mean
L0 <- diag(ncol(Y))     # Prior precision matrix
S0 <- diag(ncol(Y))     # Prior scale matrix
nu0 <- ncol(Y)          # Degrees of freedom for the Wishart distribution

n <- dim(Y)[1]
ybar <- colMeans(Y)
Sigma <- cov(Y)
THETA <- SIGMA <- NULL

set.seed(1)
for (s in 1:5000) {
  # Update theta
  Ln <- solve(solve(L0) + n * solve(Sigma))
  mun <- Ln %*% (solve(L0) %*% mu0 + n * solve(Sigma) %*% ybar)
  theta <- rmvnorm(1, mun, Ln)
  
  # Update Sigma
  Sn <- S0 + t(Y - matrix(theta, nrow = n, ncol = length(theta))) %*% (Y - matrix(theta, nrow = n, ncol = length(theta)))
  Sigma <- rwish(nu0 + n, Sn)
  
  # Save results
  THETA <- rbind(THETA, theta)
  SIGMA <- abind(SIGMA, Sigma, along = 3)
}

# Extract samples of theta_A and theta_B
theta_A_samples <- THETA[1, ]
theta_B_samples <- THETA[2, ]

# Compute the differences between corresponding samples of theta_A and theta_B
theta_diff <- theta_A_samples - theta_B_samples

# Compute posterior mean for theta_A - theta_B
posterior_mean <- mean(theta_diff)

# Compute 95% posterior confidence interval for theta_A - theta_B
posterior_ci <- quantile(theta_diff, c(0.025, 0.975))

# Print results
cat("Posterior mean for theta_A - theta_B:\n")
print(posterior_mean)

cat("\n95% posterior confidence interval for theta_A - theta_B:\n")
print(posterior_ci)