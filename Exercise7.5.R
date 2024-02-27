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

#For theta parameters

ybar_A = (1/length(yA))*sum(yA)
ybar_B = (1/length(yB))*sum(yB)

sigma2_A = 1/var_A
sigma2_B = 1/var_B 

#For 1/sigma^2 parameters

shape_A = (length(yA)-1)/2
shape_B = (length(yB)-1)/2

rate_A = (1/2)*sum((yA-ybar_A)^2)
rate_B = (1/2)*sum((yB-ybar_B)^2)


### starting values
S<-10000
PHI<-matrix ( nrow=S , ncol =4)
PHI[1 ,] <-phi<-c (theta_A,theta_B,var_A,var_B )
###
### Gibbs  sampling
set.seed(1)
for ( s in 2:S ) {
  #generate a new theta value from its full conditional
  
  phi[1]<- rnorm(1,ybar_A,sigma2_A)

  phi[2]<- rnorm(1,ybar_B,sigma2_B)
  
 
  #generate a new 1/sigmaˆ2 value from its full conditional

  phi[3]<-rgamma(1,shape_A,rate_A)
  
  phi[4]<-rgamma(1,shape_B,rate_B)
 
PHI[s,]<-phi
}

#Calculating posterior mean

theta_minus = PHI[,1]-PHI[,2]

post_mean_minus = mean(theta_minus)

#Finding 95% confidence interval, since we know that the difference in the thetas will be normal

t.test(PHI[,1],PHI[,2],paired=TRUE)


