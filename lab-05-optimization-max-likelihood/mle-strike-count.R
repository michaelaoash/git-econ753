## Analyze strike count data (Exercise from Greene)
## These are June only data.  See readme.txt
library(tidyverse)

## Real industrial production
rlip <- data.frame(year=1968:1976,
                   ip=c(0.01138, 0.02299, -0.03957, -0.05467, 0.00535, 0.07427, 0.06450, -0.10443, -0.00700))

## Strikes with durations
df68 <- data.frame(year=1968, strike_duration=c(7, 9, 13, 14, 26, 29, 52, 130)                  )
df69 <- data.frame(year=1969, strike_duration=c(9, 37, 41, 49, 52, 119)                         )
df70 <- data.frame(year=1970, strike_duration=c(3, 17, 19, 28, 72, 99, 104, 114, 152, 153, 216) )
df71 <- data.frame(year=1971, strike_duration=c(15, 61, 98)                                     )
df72 <- data.frame(year=1972, strike_duration=c(2, 25, 85)                                      )
df73 <- data.frame(year=1973, strike_duration=c(3, 10)                                          )
df74 <- data.frame(year=1974, strike_duration=c(1,2,3,3,3,4,8,11,22,23,27,32,33,35,43,43,44,100))
df75 <- data.frame(year=1975, strike_duration=c(5, 49)                                          )
df76 <- data.frame(year=1976, strike_duration=c(2,12,12,21,21,27,38,42,117)                     )

kiefer <- merge(rbind(df68, df69, df70, df71, df72, df73, df74, df75, df76),rlip)

kiefer <- kiefer %>%
    group_by(year) %>%
    summarize(strike_count = n(), ip = mean(ip))


## One parameter Poisson (no covariates)

with(kiefer, mean(strike_count))
with(kiefer, var(strike_count)) ## really rejects poisson (mean != var)
with(kiefer, log(mean(strike_count)))

## Run using generalized linear model glm() function with packaged poisson
summary(constant_glm <- glm(strike_count ~ 1, data = kiefer, family = poisson))
logLik(constant_glm)
predict(constant_glm, type="link")
predict(constant_glm, type = "response")


## Poisson regression models log(lambda) (not lambda)
## To recover lambda, exponentiate the estimate of log(lambda)

## log(lambda) = X beta, in this case log(lambda) = beta We write the
## expression for the log likelihood (and must remember to set the optimizer to maximize)
(y <- kiefer$strike_count)
(cons <- rep(1, length(kiefer$strike_count)))
(x <- kiefer$ip)
(t <- kiefer$year - 1968)


poissonll <- function(beta) {
    sum( -exp(beta*cons)  +  y*beta*cons)
}

## Use optimizer to find beta, the log lambda that maximizes log likelihood  (try two different optimizers)
(poissonll_opt  <- optimize(poissonll, interval=c(0,5), maximum=TRUE))
(poissonll_opt  <- optim(par=1,  poissonll, control = list(fnscale=-1), hessian=TRUE))
## Note that the parameter estimates match glm but the maximized log likelihood
## itself does not match glm because the ln(y!) component has been
## excluded


## Beta_hat and s.e. beta_hat
(poissonll_opt$par)
sqrt(-solve(poissonll_opt$hessian))
## Lambda_hat = exp(beta_hat)
exp(poissonll_opt$par)


## Computes ln(Y!) which is a formal piece of the ln L function (that does not depend on parameters)
lnfac  <- function(yi) {
    sum = 0 
    for (j in 1:yi) {
        sum = sum + log(j) }
    sum
}

## Log likelihood function including the formal ln(Y!) stuff.  Needed to match the glm results for the value of the objective
poissonll <- function(beta) {
    sum(-exp(beta*cons) + y * beta * cons - sapply(y, lnfac))
}

## Use optimizer to find beta, the log lambda that maximizes log likelihood  (try two different optimizers)
(poissonll_opt  <- optimize(poissonll, interval=c(0,5), maximum=TRUE))
(poissonll_opt  <- optim(par=1, poissonll, control=list(fnscale=-1)))
## Lambda
exp(poissonll_opt$par)

## Log likelihood
poissonll_opt$value
## The MLE outperforms alternatives
poissonll(log(5))
poissonll(poissonll_opt$par)
poissonll(log(9))

## Likelihood
exp(poissonll_opt$value)


## Two parameter Poisson (constant and one covariate)
## log(lambda) = XB
summary(kiefer2_glm <- glm(strike_count ~ ip, data = kiefer, family = poisson))
## Recover the expected arrival rate in the bivarate case
predict(kiefer2_glm,type="link")
predict(kiefer2_glm,type="response")
## Log likelihood function for two-parameter model
poissonll2 <- function(beta) {
    sum(-exp(beta[1]*cons + beta[2]*x) + y*(beta[1]*cons + beta[2] * x))
}
(poissonll2_opt  <- optim(par=c(3,-7),  poissonll2, control=list(fnscale=-1), hessian=TRUE))

rlip$predicted_count <- exp(poissonll2_opt$par[1] + poissonll2_opt$par[2] * rlip$ip)
with(rlip, plot(year, predicted_count))
with(rlip, lines(year, predicted_count))


## Beta_hat and s.e. beta_hat
(poissonll2_opt$par)
sqrt(diag(-solve(poissonll2_opt$hessian)))


## Three parameter Poisson (constant and two covariates)
## log(lambda) = XB
summary(kiefer3_glm <- glm(strike_count ~ ip + I(year-1968), data = kiefer, family = poisson))
## Log likelihood function for three-parameter model
poissonll3 <- function(beta) {
    sum(-exp(beta[1]*cons + beta[2]*x + beta[3]*t) + y * (beta[1]*cons + beta[2]*x + beta[3]*t))
    }
(poissonll3_opt  <- optim(par=c(3,-7,-0.10),  poissonll3, control=list(fnscale=-1), hessian=TRUE))

## Beta_hat and s.e. beta_hat
(poissonll3_opt$par)
sqrt(diag(-solve(poissonll3_opt$hessian)))
