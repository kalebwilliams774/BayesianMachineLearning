
#Question 3.7, finding posterior and conditional distributions of special
#Education students be selected for pilot training

#Defining initial conditions give within the question

n1 = 12
y1 = 2



#Posterior distribution was found to be a beta(1,12)
#Defining parameter values

a = 3
b = 14

#Posterior mean, variance and mode for beta 

post_mean = a/(a+b)

post_sd = sqrt((a*b)/(((a+b)^2)*(a+b+1)))

post_mode = (a-1)/(a+b-2)

post_paramater_df = data.frame(post_mean,post_sd,post_mode)

#Plotting the posterior distribution the paramater, a beta(3,14) distirbution

library(tidyverse)

x<-seq(from =0,to=1,by=0.01)

beta314 <- as.numeric(length(x))

for (i in seq_along(x)){
  beta314[i] = dbeta(x[i],3,14)
}

post_df = data.frame(x,beta314)


p1 = ggplot(data=post_df,aes(x=x,y=beta314))+
  geom_point() +
  theme_bw() +
  labs(
    x = 'Parameter from 0 to 1',
    y = 'Beta(3,14) Posterior Distribution Values'
  )
ggsave(filename='PosteriorDistributionPlot.png',plot=p1)

#Posterior predictive distributions is given by a Beta(y2 +3, 292-y2)
#There are 278 children in Y2, therefore y2 can vary between 0 and 278

y2 = seq(from=0,to=278, by= 1)

d <- numeric(length(y2))

for (i in seq_along(y2)){
  d[i] = (8/147)*(choose(278,y2[i])*choose(15,2))/(choose(293,y2[i]+2))
}

m_ppd = mean(d) #Posterior predictive mean

sd_ppd = sd(d) #Posterior predictive standard deviation

#Plotting the posterior predictive distribution

posterior_pred_df <- data.frame(y2,d)

p2 = ggplot(data = posterior_pred_df,aes(x=y2,y=d)) +
  geom_point() +
  theme_bw() +
  labs(
    x = 'Binomial Inputs From 0 to 278',
    y = 'Posterior Predictive Probabilities'
  )

ggsave(filename ='PosteriorPredictiveDistribution.png',plot= p2)

#Calculating the distribution of y2 with the value of probability parameter
#As the MLE 2/15

py2_values <- as.numeric(length(y2))

for (i in seq_along(y2)) {
  py2_values[i] = choose(278,y2[i])*((2/15)^(y2[i]))*((13/15)^(278-y2[i]))
}

py2_dataframe <-data.frame(y2,py2_values)

#Calculating the mean and standard deviation of the distribution

m_py2 = mean(py2_values)

sd_py2 = sd(py2_values)

#Plotting the distribution of y2 given the probability parameter is 2/15

p3 = ggplot(data = py2_dataframe,aes(x=y2,y=py2_values)) +
  geom_point() +
  labs(
    x = 'Possible Values of Y_2 (0,278)',
    y = 'Values of Distribution of Y_2'
  ) +
  theme_bw()

ggsave(filename ='DistributionOfY2.png',plot= p3)