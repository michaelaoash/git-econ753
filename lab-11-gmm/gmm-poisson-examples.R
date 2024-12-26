options(scipen=1000)
rm(list = ls())

## V1 hit data
data(package="VGAM", V1)
v1 <- rep(V1$hits, V1$ofreq)

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


## Select the series
Y <- kiefer$strike_duration ## Note very high deviation between mean and variance
Y <- v1
Y <- rpois(n=30,lambda=2.5)

## Poisson: univariate, one parameter

## Alternative estimators of lambda
(m1 <- mean(Y))
(m2 <- mean( Y^2 - mean(Y)^2 ) )  ## Population variance? Or adjust to sample variance throughout?

## Standard error of the mean
sqrt(m2)/sqrt(length(Y))

## MLE estimate of lambda
summary(constant.glm <- glm(Y ~ 1, family=poisson))
exp(coef(constant.glm)[1])



## First moment is lambda = empirical mean, Second moment is lambda = empirical variance
## Are there three options in variance:  mean(Y)^2, mean(Y)*lambda, lambda^2
poissonGMM_Q <- function(lambda, Y, W) {
    W <- W
    f <- matrix(c(mean(Y) - lambda, mean(Y^2 - (mean(Y))^2) - lambda), ncol=1 )
    t(f) %*% W %*% f
}

## Generate optimal weighting matrix Wstar = Finv from first-step values
poissonGMM_Finv <- function(lambda,Y) {
    first_moment_deviation <- Y - lambda
    second_moment_deviation <- Y^2 - (mean(Y))^2 - lambda
    F11 <- mean( first_moment_deviation^2 )
    Fc <- mean( first_moment_deviation * second_moment_deviation )
    F22 <- mean( second_moment_deviation^2 )
    F <- matrix(c(F11, Fc, Fc, F22), ncol=2)
    print(Finv <- solve(F))
    Finv
}

## Jacobian for the two Poisson moment conditions (df1/dlambda, df2/dlambda)
G <- function(lambda) {
    matrix(c(-1, -1*lambda -1), ncol=1)
}

## Variance of the estimator
V_GMM <- function(G, Finv, Y) {
    n <- length(Y)
    1/n * solve(t(G) %*% Finv %*% G)
    }


## Catch the results
results <- matrix(,nrow=4,ncol=4)
colnames(results) <- c("1st moment", "2nd moment", "Identity", "Two Step")
rownames(results) <- c("lambda", "se", "J-stat", "chi2")


W <- diag(2)
W[2,2] = 0  ## All weight on first moment condition
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y ))
(results[1,1] <- poissonGMM.opt$minimum)
(results[3,1] <- length(Y) * poissonGMM.opt$objective)
Finv <- poissonGMM_Finv(poissonGMM.opt$minimum,Y)
Finv[1,2] = 0
Finv[2,1] = 0
Finv[2,2] = 0
(results[2,1] <- sqrt(V_GMM(G(poissonGMM.opt$minimum), Finv, Y)))

W <- diag(2)
W[1,1] = 0 ## All weight on second moment condition
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y ))
(results[1,2] <- poissonGMM.opt$minimum)
(results[3,2] <- length(Y) * poissonGMM.opt$objective)
Finv <- poissonGMM_Finv(poissonGMM.opt$minimum,Y)
Finv[1,1] = 0
Finv[1,2] = 0
Finv[2,1] = 0
(results[2,2] <- sqrt(V_GMM(G(poissonGMM.opt$minimum), Finv, Y)))

W <- diag(2)  ## Identity, equal weights on both moments
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y ))
(results[1,3] <- lambdafirst <- poissonGMM.opt$minimum)
(results[3,3] <- length(Y) * poissonGMM.opt$objective)
(results[2,3] <- sqrt(V_GMM(G(lambdafirst), poissonGMM_Finv(lambdafirst,Y), Y)))

## Optimal weights in second step
Wstar <- poissonGMM_Finv(lambdafirst, Y = Y )
poissonGMM.opt <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=Wstar, Y = Y )
(results[1,4] <- lambdastar <- poissonGMM.opt$minimum)
(results[3,4] <- length(Y) * poissonGMM.opt$objective)
Finv <- poissonGMM_Finv(lambdastar, Y)
(results[2,4] <- sqrt(V_GMM(G(lambdastar), Finv, Y)))


results[4,] <- pchisq(results[3,],1, lower.tail=FALSE)

round(results,3)













## Assumes lambda = mean(Y) in the second expression.
poissonGMM_Q <- function(lambda,Y,W) {
    W <- W
    f <- matrix(c(mean(Y) - lambda, mean(Y^2) - lambda^2 - lambda), ncol=1) 
    t(f) %*% W %*% f
}
## Generate optimal weighting matrix Wstar = Finv from first-step values
poissonGMM_Finv <- function(lambda,Y) {
    first_moment_deviation <- Y - lambda
    second_moment_deviation <- Y^2 - lambda^2 - lambda
    F11 <- mean( first_moment_deviation^2 )
    Fc <- mean( first_moment_deviation * second_moment_deviation )
    F22 <- mean( second_moment_deviation^2 )
    print(F <- matrix(c(F11, Fc, Fc, F22), ncol=2))
    print(Finv <- solve(F))
    Finv
}

W <- diag(2)
W[2,2] = 0  ## All weight on first moment condition
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y ))
sqrt(V_GMM(G(poissonGMM.opt$minimum), W, Y))

W <- diag(2)
W[1,1] = 0 ## All weight on second moment condition
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y ))
sqrt(V_GMM(G(poissonGMM.opt$minimum), W, Y))

W <- diag(2)  ## Identity, equal weights on both moments
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y ))
sqrt(V_GMM(G(poissonGMM.opt$minimum), W, Y))
lambdafirst <- poissonGMM.opt$minimum

## Optimal weights in second step
Wstar <- poissonGMM_Finv(lambdafirst, Y = Y )
(lambdastar <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=Wstar, Y = Y )$minimum)
sqrt(V_GMM(G(lambdastar), Wstar, Y))







## Another variant for defining the moments 
poissonGMM_Q <- function(lambda,Y,W) {
    W <- W
    f <- matrix(c(mean(Y) - lambda, mean((Y - lambda)^2) - lambda), ncol=1)
    t(f) %*% W %*% f
}
## Generate optimal weighting matrix Wstar = Finv from first-step values
poissonGMM_Finv <- function(lambda,Y) {
    first_moment_deviation <- Y - lambda
    second_moment_deviation <- Y^2 - Y*lambda + lambda^2 - lambda
    F11 <- mean( first_moment_deviation^2 )
    Fc <- mean( first_moment_deviation * second_moment_deviation )
    F22 <- mean( second_moment_deviation^2 )
    print(F <- matrix(c(F11, Fc, Fc, F22), ncol=2))
    print(Finv <- solve(F))
    Finv
}

W <- diag(2)
W[2,2] = 0  ## All weight on first moment condition
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y ))
sqrt(V_GMM(G(poissonGMM.opt$minimum), W, Y))

W <- diag(2)
W[1,1] = 0 ## All weight on second moment condition
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y ))
sqrt(V_GMM(G(poissonGMM.opt$minimum), W, Y))

W <- diag(2)  ## Identity, equal weights on both moments
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y ))
sqrt(V_GMM(G(poissonGMM.opt$minimum), W, Y))
lambdafirst <- poissonGMM.opt$minimum

## Optimal weights in second step
Wstar <- poissonGMM_Finv(lambdafirst, Y = Y )
(lambdastar <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=Wstar, Y = Y )$minimum)
sqrt(V_GMM(G(lambdastar), Wstar, Y))
