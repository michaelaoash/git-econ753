## Analyze strike duration data (Exercise from Greene)

## Real industrial production
rlip <- data.frame(year=1968:1976,
                   x=c(0.01138, 0.02299, -0.03957, -0.05467, 0.00535, 0.07427, 0.06450, -0.10443, -0.00700))

## Strikes with durations
df68 <- data.frame(year=1968, strike=c(7, 9, 13, 14, 26, 29, 52, 130)                  )
df69 <- data.frame(year=1969, strike=c(9, 37, 41, 49, 52, 119)                         )
df70 <- data.frame(year=1970, strike=c(3, 17, 19, 28, 72, 99, 104, 114, 152, 153, 216) )
df71 <- data.frame(year=1971, strike=c(15, 61, 98)                                     )
df72 <- data.frame(year=1972, strike=c(2, 25, 85)                                      )
df73 <- data.frame(year=1973, strike=c(3, 10)                                          )
df74 <- data.frame(year=1974, strike=c(1,2,3,3,3,4,8,11,22,23,27,32,33,35,43,43,44,100))
df75 <- data.frame(year=1975, strike=c(5, 49)                                          )
df76 <- data.frame(year=1976, strike=c(2,12,12,21,21,27,38,42,117)                     )

kiefer <- merge(rbind(df68, df69, df70, df71, df72, df73, df74, df75, df76),rlip)


## One parameter Poisson (no covariates)

with(kiefer, mean(strike))
with(kiefer, log(mean(strike)))
with(kiefer, var(strike))

summary(constant.glm <- glm(strike ~ 1  , data=kiefer, family=poisson))
logLik(constant.glm)
predict(constant.glm,type="response")


## log(lambda) = XB, in this case log(lambda) = B
poissonll <- function(beta) {
    y <- kiefer$strike
    cons <- rep(1, length(kiefer$strike))
    -sum( -exp(beta*cons) + y * beta * cons ) }

## Find log lambda that minimizes negative log likelihood
(poissonll.opt  <- optimize( poissonll, interval=c(0,5)  ))
(poissonll.opt  <- optim( 1,  poissonll ))
## Lambda
exp(poissonll.opt$par)



## Computes ln(Y!) which is a formal piece of the ln L function (that does not depend on parameters)
lnfac  <- function(yi) {
    sum = 0 
    for (j in 1:yi) {
        sum = sum + log(j) }
    sum
    }

## Log likelihood function including the formal ln(Y!)
poissonll <- function(beta) {
    y <- kiefer$strike
    cons <- rep(1, length(kiefer$strike))
    -sum( -exp(beta*cons) + y * beta * cons - sapply(y, lnfac)) }

## Find log lambda that minimizes negative log likelihood
(poissonll.opt  <- optimize( poissonll, interval=c(0,5)  ))
(poissonll.opt  <- optim( 1,  poissonll ))
## Lambda
exp(poissonll.opt$par)

## Log likelihood
-poissonll.opt$value
## Max outperforms alternatives
-poissonll(log(40))
-poissonll(poissonll.opt$par)
-poissonll(log(49))

## Likelihood
exp(-poissonll.opt$value)



## Two parameter Poisson (constant and one covariate)
## log(lambda) = XB
summary(kiefer.glm <- glm(strike ~ x  , data=kiefer, family=poisson))
poissonll2 <- function(beta) {
    y <- kiefer$strike
    cons <- rep(1, length(kiefer$strike))
    x <- kiefer$x
    -sum(  -exp(beta[1]*cons + beta[2] * x) + y * (beta[1]*cons + beta[2] * x) )
    }
(poissonll2.opt  <- optim(par=c(3,-7),  poissonll2 ))


## Three parameter Poisson (constant and two covariates)
## log(lambda) = XB
summary(kiefer.glm <- glm(strike ~ x + I(year-1968)  , data=kiefer, family=poisson))
poissonll3 <- function(beta) {
    y <- kiefer$strike
    cons <- rep(1, length(kiefer$strike))
    x <- kiefer$x
    t <- kiefer$year - 1968
    -sum(  -exp(beta[1]*cons + beta[2] * x + beta[3]*t) + y * (beta[1]*cons + beta[2] * x + beta[3]*t) )
    }
(poissonll3.opt  <- optim(par=c(3,-7,-0.10),  poissonll3 ))
