#Exercise 4.8 a), Importing the data sets for men over 30 with a child and
#Bachelors degree. As well as men over 30 with a child and no bachelors degree

#Men over 30 with children and bachelors degree data
y1 <- c(1, 0, 0, 1, 2, 2, 1, 5, 2, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 
        0, 0, 1, 1, 2, 1, 3, 2, 0, 0, 3, 0, 0, 0, 2, 1, 0, 2, 1,0
        , 0, 1, 3, 0, 1, 1, 0, 2, 0, 0, 2, 2, 1, 3, 0, 0, 0, 1, 1)

#Men over 30 with children with no bachelors degree data
y2 <- c(2, 2, 1, 1, 2, 2, 1, 2, 1 ,0, 2, 1, 1, 2, 0, 2, 2, 0, 
        2, 1, 0, 0, 3, 6, 1, 6, 4, 0, 3, 2, 0, 1, 0, 0, 0, 3, 0,
        0, 0, 0, 0, 1, 0, 4, 2, 1, 0, 0, 1, 0, 3, 2, 5, 0, 1, 1, 2, 1, 2, 1, 2, 0, 0, 
        0, 2, 1, 0, 2, 0, 2, 4, 1, 1, 1, 2, 0, 1, 1, 1, 1, 0, 2, 3 ,2, 0, 2, 1, 3, 1, 
        3, 2, 2, 3, 2, 0, 0, 0, 1, 0, 0, 0, 1, 2, 0, 3, 3, 0, 1, 2, 2, 2,0, 6, 0, 0, 
        0, 2, 0, 1, 1, 1, 3, 3, 2, 1, 1, 0, 1, 0, 0, 2, 0, 2, 0, 1, 0, 2, 0, 0, 2, 2, 
        4, 1, 2, 3, 2, 0, 0,0, 1, 0, 0, 1, 5, 2, 1, 3, 2, 0, 2, 1, 1, 3, 0, 5, 0, 0, 2, 
        4 ,3, 4, 0 ,0 ,0 ,0 ,0 ,0 ,2 ,2 ,0 ,0 ,2 ,0 ,0, 1,1, 0, 2, 1, 3, 3, 2, 2, 0, 0, 
        2, 3 ,2, 4, 3, 3, 4, 0, 3, 0, 1, 0, 1, 2, 3, 4, 1 ,2 ,6 ,2 ,1 ,2, 2)

a= 2 ; b=1
sy1 = sum(y1) ; n1 = length(y1)
sy2 = sum(y2) ; n2 = length(y2)

#To obtain MC samples we must sample from the posterior to get values for the 
#Parameters from each data set and use that as the parameter for the sampling
#Distribution

#For data set of men over 30 with children and bachelors degree
#Posterior for both data sets is a Gamma
#We will take 5000 samples as instructed 

set.seed(123)

mc.theta1 <- rgamma(5000, a +sy1, b +n1)
y1.mc <- rpois(5000, mc.theta1)

#For data set of men over 30 with children and bachelors degree

mc.theta2 <- rgamma(5000, a +sy2, b +n2)
y2.mc <- rpois(5000, mc.theta2)

#Plotting MC samples for data set of men over 30 with children and bachelors 
#Degree

library(tidyverse)

#Creating data frame to utilize ggplot

MC.samples.df <- data.frame('theta1'= c(mc.theta1),'y1' = c(y1.mc),
                            'theta2'= c(mc.theta2),'y2' = c(y2.mc))

plot1_48 = ggplot(data = MC.samples.df, aes(x=y1.mc))+
  geom_histogram(binwidth=0.5) +
  labs(
    x = 'Sampled Value for Average Number of Children from Men Over 30 With 
    Children and a Bachelors Degree',
    y='Frequency'
  )

#Plotting MC samples for data set of men over 30 with children and no bachelors 
#Degree
#Creating data frame to utilize ggplot

plot2_48 = ggplot(data = MC.samples.df, aes(x=y2.mc))+
  geom_histogram(binwidth=0.5) +
  labs(
    x = 'Sampled Value for Average Number of Children from Men Over 30 With 
    Children and No Bachelors Degree',
    y='Frequency'
  )


#Saving the plots 

ggsave('PosteriorPredictive1_48.png', plot = plot1_48)

ggsave('PosteriorPredictive2_48.png', plot = plot2_48)


#Calculating the difference in Y_A, Y_B and theta_A, theta_B

Y_diff <- MC.samples.df$y2 - MC.samples.df$y1

theta_diff <- MC.samples.df$theta2 - MC.samples.df$theta1


quantiles_theta_diff <- quantile(theta_diff, c(0.025, 0.975))

quantiles_Y_diff <- quantile(Y_diff, c(0.025, 0.975))

#Calculating the mean number of children predicted by posterior predictive

mean1 = mean(y1.mc)

mean2 = mean(y2.mc)

mean1

mean2

#Finding the empirical distribution of the  men with no bachelors degree data

png('Emperical438.png')

ecdf<-(table(c(y2,0:9))-1 )/sum(table(y2))

pois_probs <- dpois(0:9,1.4)

plot(0:9+.1,ecdf,type="h",lwd=5,xlab="number of children",
     ylab=expression(paste("Pr(",italic(Y[i]==y[i]),")",sep="")),col="gray",
     ylim=c(0,0.4))
axis(2,at=c(0,0.2,0.4,0.6,0.8,1))
points(pois_probs, ,lwd=5,col="black",type="h")

legend(1.8,.35,
       legend=c("Poisson Distribution","Emperical Distribution"),
       lwd=c(2,2),col=
         c("black","gray"),bty="n",cex=.8)

#d) Simulating 218 samples from a poisson distribution using each of the 
#simulated parameter values in mc.thetaB 
set.seed(123)

num_simulations=5000
nB=218

count_0s <- numeric(num_simulations)
count_1s <- numeric(num_simulations)

# Simulate data for each θB-value
for (i in 1:num_simulations) {
  # Simulate Poisson distribution for each θB-value
  simulated_data <- rpois(nB, lambda = mc.theta2[i])
  # Count occurrences of 0s and 1s
  count_0s[i] <- sum(simulated_data == 0)
  count_1s[i] <- sum(simulated_data == 1)
}

#Finding number of people from MC simulated y2 data set have 1 and 0 children

obs.count.0s = sum(y2 == 0)

obs.counts.1s = sum(y2== 1)

#Plotting the number of 1's vs the number of 0's

png('Count10.png')

plot(count_0s,count_1s,xlab='Number of People With 0 Chidlren',ylab='Number of People With 1 Child',ylim = c(40,100))
points(obs.count.0s,obs.counts.1s,pch = 20, col = "red")

