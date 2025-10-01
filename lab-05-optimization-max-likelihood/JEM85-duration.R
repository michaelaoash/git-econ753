library(tidyverse)
library(here)

## Analyze strike duration data (from Kennan, cited by Greene)

kennan_wide <- read_table(here("lab-05-optimization-max-likelihood", "JEM85.txt"),
                          col_name = c("yrmo", "strikes", "IP",
                                       "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18"),
                          skip = 1
                          )

kennan <- kennan_wide %>%
    pivot_longer(cols = starts_with("D"), values_to = "duration") %>%
    filter(!is.na(duration)) %>%
    mutate(
        year = 1900 + yrmo %/% 100,
        month = yrmo %% 100,
        date = as.Date(paste0(as.character(year), "-", str_pad(as.character(month),2), "-01"), "%Y-%m-%d")
        )

## One parameter Poisson (no covariates)
with(kennan, mean(duration))
with(kennan, var(duration)) ## really rejects poisson (mean != var)
with(kennan, log(mean(duration)))

## Run using generalized linear model glm() function with packaged poisson
summary(constant.glm <- glm(duration ~ 1, data = kennan, family = poisson))
logLik(constant.glm)
predict(constant.glm, type = "response")

## Poisson regression models log(lambda) (not lambda)
## To recover lambda, exponentiate the estimate of log(lambda)

## log(lambda) = X beta, in this case log(lambda) = beta We write the
## expression for the log likelihood (and must remember to set the optimizer to maximize)
(y <- kennan$duration)
(cons <- rep(1, length(kennan$duration)))
(x <- kennan$IP)
(t <- kennan$year - 1968)

poissonll <- function(beta) {
    sum(-exp(beta*cons)  +  y*beta*cons)
}

## Use optimizer to find beta, the log lambda that maximizes log likelihood  (try two different optimizers)
(poissonll.opt  <- optimize(poissonll, interval=c(0,5), maximum=TRUE))
(poissonll.opt  <- optim(par=1,  poissonll, control = list(fnscale=-1), hessian=TRUE))
## Note that the parameter estimates match glm but the maximized log likelihood
## itself does not match glm because the ln(y!) component has been
## excluded


## Beta_hat and s.e. beta_hat
(poissonll.opt$par)
sqrt(-solve(poissonll.opt$hessian))
## Lambda_hat = exp(beta_hat)
exp(poissonll.opt$par)


## Computes ln(Y!) which is a formal piece of the ln L function (that does not depend on parameters)
lnfac  <- function(yi) {
    sum = 0 
    for (j in 1:yi) {
        sum = sum + log(j) }
    sum
}

## Log likelihood function including the formal ln(Y!) stuff.  Needed to match the glm results for the value of the objective
poissonll <- function(beta) {
    sum(-exp(beta * cons) + y * beta * cons - sapply(y, lnfac))
}

## Use optimizer to find beta, the log lambda that maximizes log likelihood  (try two different optimizers)
(poissonll.opt  <- optimize(poissonll, interval = c(0, 5), maximum = TRUE))
(poissonll.opt  <- optim(par = 1, poissonll, control = list(fnscale = -1)))
## Lambda
exp(poissonll.opt$par)

## Log likelihood
poissonll.opt$value
## The MLE outperforms alternatives
poissonll(log(40))
poissonll(poissonll.opt$par)
poissonll(log(49))

## Likelihood
exp(poissonll.opt$value)


## Two parameter Poisson (constant and one covariate)
## log(lambda) = X beta
summary(kennan2.glm <- glm(duration ~ IP, data = kennan, family = poisson))
## Recover the expected arrival rate in the bivarate case
predict(kennan2.glm,type="response")
## Log likelihood function for two-parameter model
poissonll2 <- function(beta) {
    sum(-exp(beta[1]*cons + beta[2]*x) + y*(beta[1]*cons + beta[2] * x))
}
(poissonll2.opt  <- optim(par=c(3,-7),  poissonll2, control = list(fnscale=-1), hessian=TRUE))

## Beta_hat and s.e. beta_hat
(poissonll2.opt$par)
sqrt(diag(-solve(poissonll2.opt$hessian)))


rlip <- kennan %>% group_by(yrmo) %>% summarize(IP = max(IP), date = max(date))
rlip$predicted_duration = exp(poissonll2.opt$par[1] + poissonll2.opt$par[2]*rlip$IP)
kennan %>% ggplot(aes(x = date, y = duration)) + geom_point() + geom_line(data = rlip, aes(x = date, y = predicted_duration))


## Three parameter Poisson (constant and two covariates)
## log(lambda) = XB
summary(kennan3.glm <- glm(duration ~ IP + I(year-1968) , data = kennan, family = poisson))
## Log likelihood function for three-parameter model
poissonll3 <- function(beta) {
    sum(-exp(beta[1]*cons + beta[2]*x + beta[3]*t) + y * (beta[1]*cons + beta[2]*x + beta[3]*t))
    }
(poissonll3.opt  <- optim(par=c(3,-7,-0.10),  poissonll3, control=list(fnscale=-1), hessian=TRUE))

## Beta_hat and s.e. beta_hat
(poissonll3.opt$par)
sqrt(diag(-solve(poissonll3.opt$hessian)))
