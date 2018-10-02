# If we take a sample of 1000 beads when the probability of being "1" is 0.45.
N <- 1000
expected_X_hat <- 0.45
expected_sd <- sqrt(expected_X_hat*(1-expected_X_hat)/N)

#Checking with Monte Carlo
p <- 0.45
B <- 10000
X_hat <- replicate(B,{
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p,p))
  mean(X)
})

mean(X_hat)
sd(X_hat)

#Plotting to confirm normality
library(gridExtra)
library(tidyverse)

p1 <- data.frame(X_hat=X_hat) %>% ggplot(aes(X_hat))+
  geom_histogram(binwidth = 0.005, color = "black")

p2 <- data.frame(X_hat=X_hat) %>% ggplot(aes(sample=X_hat))+
  stat_qq(dparams = list(mean=mean(X_hat), sd =sd(X_hat)))+
  geom_abline()+
  ylab("X_hat")+
  xlab("Theoretical normal")

grid.arrange(p1,p2,nrow=1)

#Monte Carlo for Confience intervals

B <- 10000

inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat * (1-X_hat)/N)
  between(p,X_hat - 2*SE_hat, X_hat + 2*SE_hat)
})
mean(inside)

#Monte Carlo with multiple polls

d <- 0.039
Ns <- c(1298,533,1342,897,774,254,812,324,1291,1056,2172,516)
p <- (d+1)/2

confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat+2*SE_hat)-1
})

polls <- data.frame(poll=1:ncol(confidence_intervals),
                    t(confidence_intervals),
                    sample_size=Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

#What if we aggregated the pools
sum(polls$sample_size)
d_hat <- polls %>% 
  summarize(avg=sum(estimate*sample_size)/sum(sample_size)) %>% 
  .$avg

p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))
moe

#Predicting the election
library(dslabs)
rm(list=ls())
data("polls_us_election_2016")
polls <-  polls_us_election_2016 %>% 
  filter(state == "U.S." & 
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+")| is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

d_hat <- polls %>% summarize(d_hat = sum(spread * samplesize)/sum(samplesize)) %>% 
  .$d_hat

p_hat <- (d_hat+1)/2
moe <- 1.96*2*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
moe

# Bayesian theorem

prev <- 0.00025
N <- 100000
outcome <- sample(c("Disease", "Healthy"),N, replace=TRUE, prob = c(prev, 1-prev))

N_D <- sum(outcome=="Disease")
N_D

N_H <- sum(outcome=="Healthy")
N_H

accuracy <- 0.99
test <- vector("character",N)
test[outcome=="Disease"] <- sample(c("+","-"),N_D, replace=TRUE, prob=c(accuracy,1-accuracy))
test[outcome=="Healthy"] <- sample(c("-","+"),N_H, replace=TRUE, prob=c(accuracy,1-accuracy))

table(outcome,test)

# Here we compute the probability f election winning

I <- 5 #number of pollsters
J <- 6 #number of polls per pollsters
N <- 2000 #sample size
d <- 0.021 #actual spread
p <- (d+1)/2
h <- rnorm(I,0,0.025)
X <- sapply(1:I, function(i){
  d+h[i]+rnorm(J,0,2*sqrt(p*(1-p)/N))
})

