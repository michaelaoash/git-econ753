library(tidyverse)
library(here)

## Analyze strike count data (from Kennan, Exercise from Greene)

kennan_wide <- read_table(here("lab-05-optimization-max-likelihood", "JEM85.txt"),
                          col_name = c("yrmo", "strike_count", "IP",
                                       "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18"),
                          skip = 1
                          )

kennan <- kennan_wide %>%
    select(yrmo, strike_count, IP) %>%
    mutate(
        year = 1900 + yrmo %/% 100,
        month = yrmo %% 100,
        date = as.Date(paste0(as.character(year), "-", str_pad(as.character(month),2), "-01"), "%Y-%m-%d")
    )

kennan %>% select(yrmo, strike_count, IP) %>% print(n = 200, row.names = FALSE)

## N.B. No IP data available for months with no strikes
kennan <- kennan %>%
    filter(!is.na(IP))

summary(kennan)

## One parameter Poisson (no covariates)
with(kennan, mean(strike_count))
with(kennan, var(strike_count)) ## really rejects poisson (mean != var)
with(kennan, log(mean(strike_count)))

kennan %>%
    ggplot() + geom_bar(aes(x = strike_count)) + 
    scale_x_continuous(breaks = seq(0, 21, 1)) +
    geom_point(data = transform(data.frame(x = c(0:21), y = length(kennan$strike_count) * dpois(c(0:21), with(kennan, mean(strike_count))))),
               aes(x, y), color="red", size = 6, alpha = 0.25) 



## Run using generalized linear model glm() function with packaged poisson
summary(constant_glm <- glm(strike_count ~ 1, data = kennan, family = poisson))
logLik(constant_glm)
predict(constant_glm, type = "link")
predict(constant_glm, type = "response")

## Poisson regression models log(lambda) (not lambda)
## To recover lambda, exponentiate the estimate of log(lambda)

## log(lambda) = X beta, in this case log(lambda) = beta We write the
## expression for the log likelihood (and must remember to set the optimizer to maximize)
(y <- kennan$strike_count)
(cons <- rep(1, length(kennan$strike_count)))
(x <- kennan$IP)
(t <- kennan$year - 1968)

poissonll <- function(beta) {
    sum(-exp(beta*cons)  +  y*beta*cons)
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
    if (yi > 0) {
        for (j in 1:yi) {
            sum = sum + log(j) }
        }
    sum
}

## Log likelihood function including the formal ln(Y!) stuff.  Needed to match the glm results for the value of the objective
poissonll <- function(beta) {
    sum(-exp(beta * cons) + y * beta * cons - sapply(y, lnfac))
}

## Use optimizer to find beta, the log lambda that maximizes log likelihood  (try two different optimizers)
(poissonll_opt  <- optimize(poissonll, interval = c(0, 5), maximum = TRUE))
(poissonll_opt  <- optim(par = 1, poissonll, control = list(fnscale = -1)))
## Lambda
exp(poissonll_opt$par)

## Log likelihood
poissonll_opt$value
## The MLE outperforms alternatives
poissonll(log(4))
poissonll(poissonll_opt$par)
poissonll(log(8))

## Likelihood
exp(poissonll_opt$value)


## Two parameter Poisson (constant and one covariate)
## log(lambda) = X beta
summary(kennan2_glm <- glm(strike_count ~ IP, data = kennan, family = poisson))
## Recover the expected arrival rate in the bivarate case
predict(kennan2_glm, type = "link")
predict(kennan2_glm, type = "response")
## Log likelihood function for two-parameter model
poissonll2 <- function(beta) {
    sum(-exp(beta[1]*cons + beta[2]*x) + y*(beta[1]*cons + beta[2] * x))
}
(poissonll2_opt  <- optim(par = c(2, 3),  poissonll2, control = list(fnscale = -1), hessian = TRUE))

## Beta_hat and s.e. beta_hat
(poissonll2_opt$par)
sqrt(diag(-solve(poissonll2_opt$hessian)))


kennan$predicted_strike_count <- exp(poissonll2_opt$par[1] + poissonll2_opt$par[2]*kennan$IP)
kennan %>% ggplot(aes(x = date, y = strike_count)) + geom_point() + geom_line(data = rlip, aes(x = date, y = predicted_strike_count))


## Three parameter Poisson (constant and two covariates)
## log(lambda) = XB
summary(kennan3_glm <- glm(strike_count ~ IP + I(year - 1968), data = kennan, family = poisson))
## Log likelihood function for three-parameter model
poissonll3 <- function(beta) {
    sum(-exp(beta[1]*cons + beta[2]*x + beta[3]*t) + y * (beta[1]*cons + beta[2]*x + beta[3]*t))
    }
(poissonll3_opt  <- optim(par=c(3,-7,-0.10),  poissonll3, control=list(fnscale=-1), hessian=TRUE))

## Beta_hat and s.e. beta_hat
(poissonll3_opt$par)
sqrt(diag(-solve(poissonll3_opt$hessian)))
