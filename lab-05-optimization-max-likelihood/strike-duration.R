## Analyze strike duration data (Exercise from Greene)

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


## One parameter Poisson (no covariates)

with(kiefer, mean(strike_duration))
with(kiefer, log(mean(strike_duration)))
with(kiefer, var(strike_duration))

summary(constant.glm <- glm(strike_duration ~ 1  , data=kiefer, family=poisson))
logLik(constant.glm)
predict(constant.glm,type="response")

## Poisson regression models log(lambda) (not lambda)
## To recover lambda, exponentiate the estimate of log(lambda)

## log(lambda) = XB, in this case log(lambda) = B We write the
## expression for the log likelihood (and must remember to set the optimizer to maximize)
poissonll <- function(beta) {
    y <- kiefer$strike_duration
    cons <- rep(1, length(kiefer$strike_duration))
    sum( -exp(beta*cons) + y * beta * cons )
}

## Use optimizer to find beta, the log lambda that maximizes log likelihood  (try two different optimizers)
(poissonll.opt  <- optimize( poissonll, interval=c(0,5), maximum=TRUE  ))
(poissonll.opt  <- optim( par=1,  poissonll, control = list(fnscale=-1), hessian=TRUE ))
## Note that the parameter estimates match glm but the log likelihood
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
    y <- kiefer$strike_duration
    cons <- rep(1, length(kiefer$strike_duration))
    sum( -exp(beta*cons) + y * beta * cons - sapply(y, lnfac))
}

## Use optimizer to find beta, the log lambda that maximizes log likelihood  (try two different optimizers)
(poissonll.opt  <- optimize( poissonll, interval=c(0,5), maximum=TRUE  ))
(poissonll.opt  <- optim( par=1,  poissonll, control=list(fnscale=-1) ))
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
## log(lambda) = XB
summary(kiefer2.glm <- glm(strike_duration ~ ip  , data=kiefer, family=poisson))
## Recover the expected arrival rate in the bivarate case
predict(kiefer2.glm,type="response")
## Log likelihood function for two-parameter model
poissonll2 <- function(beta) {
    y <- kiefer$strike_duration
    cons <- rep(1, length(kiefer$strike_duration))
    x <- kiefer$ip
    sum(  -exp(beta[1]*cons + beta[2] * x) + y * (beta[1]*cons + beta[2] * x) )
    }
(poissonll2.opt  <- optim(par=c(3,-7),  poissonll2, control=list(fnscale=-1), hessian=TRUE ))

rlip$predicted_duration = exp(poissonll2.opt$par[1] + poissonll2.opt$par[2]*rlip$ip)
with(rlip, plot(year, predicted_duration))
with(rlip, lines(year, predicted_duration))


## Beta_hat and s.e. beta_hat
(poissonll2.opt$par)
sqrt(diag(-solve(poissonll2.opt$hessian)))



## Three parameter Poisson (constant and two covariates)
## log(lambda) = XB
summary(kiefer3.glm <- glm(strike_duration ~ ip + I(year-1968) , data=kiefer, family=poisson))
## Log likelihood function for three-parameter model
poissonll3 <- function(beta) {
    y <- kiefer$strike_duration
    cons <- rep(1, length(kiefer$strike_duration))
    x <- kiefer$ip
    t <- kiefer$year - 1968
    sum(  -exp(beta[1]*cons + beta[2]*x + beta[3]*t) + y * (beta[1]*cons + beta[2]*x + beta[3]*t) )
    }
(poissonll3.opt  <- optim(par=c(3,-7,-0.10),  poissonll3, control=list(fnscale=-1), hessian=TRUE ))

## Beta_hat and s.e. beta_hat
(poissonll3.opt$par)
sqrt(diag(-solve(poissonll3.opt$hessian)))
