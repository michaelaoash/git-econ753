## Run with  R CMD BATCH --vanilla gibbs-normal-parameters.R &

## Birth weight data from UC Berkeley
babiesI <- read.table("http://www.stat.berkeley.edu/users/statlabs/data/babiesI.data", header = TRUE)
## babiesI <- read.table("babiesI.data", header = TRUE)

## 2000 burn-in and 5000 post-burn-in iterations
burnin <- 2000
h <- rep(NA, 7000)
mu <- rep(NA, 7000)
delta <- rep(NA, 7000)

## The truth
with(babiesI, mean(bwt))
with(babiesI, 1 / var(bwt))
with(babiesI, sd(bwt))

## Draw a sample from the population
bwt_smpl <- with(babiesI, sample(bwt, size = 20, replace = FALSE))

(n <- length(bwt_smpl))
(ybar <- mean(bwt_smpl))
sd(bwt_smpl)

## Priors
mu0 <- 110
alpha0 <- 30
delta0 <- 6000
(h0 <- (alpha0 / 2) / (delta0 / 2))
(1 / h0)
sqrt(1 / h0)

alpha1 <- alpha0 + n
delta[1] <- delta0 + sum((bwt_smpl - mu0)^2)
h[1] <- rgamma(n = 1, shape = alpha1 / 2, rate = delta[1] / 2)
mu[1] <- rnorm(n = 1, mean = (h0*mu0 + h0*n*ybar) / (h0 + h0*n), sd = sqrt (1 / (h0 + h0*n)))

## Here are the draws from the simulated distribution
for(i in 2:length(mu)) {
  mu[i] <- rnorm(n=1, 
                 mean = (h0*mu0 + h[i-1]*n*ybar) / (h0 + h[i-1]*n), 
                 sd=sqrt (1 / (h0 + h[i-1] * n)))
  delta[i] = (delta0 + sum((bwt_smpl - mu[i])^2))
  h[i] <- rgamma(n = 1, shape = alpha1 / 2, rate = delta[i] / 2)
}

## Report the mean and the 95 percent credibility interval of the
## posterior of mu
mean(mu[(burnin+1):length(mu)])
quantile(mu[(burnin+1):length(mu)], probs=c(0.025, 0.975))

## Report the mean and 95 percent c.i. of the posterior of the
## precision, the variance, and the standard deviation
mean(h[(burnin+1):length(h)])
quantile(h[(burnin+1):length(h)], probs=c(0.025, 0.975))

(1 / mean(h[(burnin+1):length(h)]))
quantile((1 / h[(burnin+1):length(h)]), probs=c(0.025, 0.975))

sqrt(1 / mean(h[(burnin+1):length(h)]))
quantile(sqrt(1 / h[(burnin+1):length(h)]), probs=c(0.025, 0.975))

## Prior of mean (mu)
pi_mu0 <- dnorm(x_mu <- seq(from = 70, to = 150, by = (150-70) / (length(mu)-burnin)), mean = mu0, sd = sqrt(1 / h0))

## Likelihood from sample


## Posterior of mean (mu) with prior overlay
hist(mu[(burnin+1):length(mu)], xlim=c(70, 150), main="", xlab="")
lines(x_mu, 30000*pi_mu0)
title(expression(paste("Prior and Posterior Distribution of ", mu)), xlab=expression(mu))

## Prior of precision (h)
pi_h0 <- dgamma(x.h <- seq(from=(1 / 30)^2, to=(1 / 10)^2, by=((1 / 10)^2-(1 / 30)^2) / (length(mu) - burnin)), 
                shape=alpha0 / 2, rate=delta0 / 2)
## Posterior of precision (h) with prior overlay
hist(h[(burnin+1):length(h)], xlim=c((1 / 30)^2, (1 / 10)^2), main="", xlab="")
lines(x.h, 2.7*pi_h0)
title(expression(paste("Prior and Posterior Distribution of h")), xlab=expression(paste("h")))

## Prior of standard deviation (sqrt(1 / h))
## Posterior of standard deviation (sqrt(1 / h)) with prior overlay
hist(sqrt(1 / h[(burnin+1):length(h)]), xlim=c(10, 30), main="", xlab="")
lines(sqrt(1 / x.h), 2.5*pi_h0)
title(expression(paste("Prior and Posterior Distribution of ", sigma)), xlab=expression(sigma))
