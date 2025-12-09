library(tidyverse)
library(here)
options(scipen=1000)
rm(list = ls())


## V1 hit data
data(package="VGAM", V1)
v1 <- rep(V1$hits, V1$ofreq)


kennan_wide <- read_table(here("lab-05-optimization-max-likelihood", "JEM85.txt"),
                          col_name = c("yrmo", "strike_count", "IP",
                                       "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18"),
                          skip = 1)
                         

kennan <- kennan_wide %>%
    select(yrmo, strike_count, IP) %>%
    mutate(
        year = 1900 + yrmo %/% 100,
        month = yrmo %% 100,
        date = as.Date(paste0(as.character(year), "-", str_pad(as.character(month),2), "-01"), "%Y-%m-%d")
    )
   

kennan %>% select(yrmo, strike_count, IP) %>% print(n = 200, row.names = FALSE)


## Select the series
Y <- kennan$strike_count ## Note very high deviation between mean and variance
Y <- v1
Y <- rpois(n = 30, lambda = 2.5)

## Poisson: univariate, one parameter

## Alternative estimators of lambda
(m1 <- mean(Y))
(m2 <- mean(Y^2 - mean(Y)^2))  ## Population variance? Or adjust to sample variance throughout?

## Standard error of the mean
sqrt(m2) / sqrt(length(Y))

## MLE estimate of lambda
summary(constantGLM <- glm(Y ~ 1, family = poisson))
exp(coef(constantGLM)[1])


## First moment is lambda = empirical mean, Second moment is lambda = empirical variance
## Are there three options in variance:  mean(Y)^2, mean(Y)*lambda, lambda^2
poissonGMM_Q <- function(lambda, Y, W) {
    W <- W
    f <- matrix(c(mean(Y) - lambda, mean(Y^2 - (mean(Y))^2) - lambda), ncol=1)
    t(f) %*% W %*% f
}

## Generate optimal weighting matrix Wstar = Finv from first-step values
poissonGMM_Finv <- function(lambda, Y) {
    first_moment_deviation <- Y - lambda
    second_moment_deviation <- Y^2 - (mean(Y))^2 - lambda
    F11 <- mean(first_moment_deviation^2)
    Fc <- mean(first_moment_deviation * second_moment_deviation)
    F22 <- mean(second_moment_deviation^2)
    F <- matrix(c(F11, Fc, Fc, F22), ncol=2)
    print(Finv <- solve(F))
    Finv
}

## Jacobian for the two Poisson moment conditions (df1/dlambda, df2/dlambda)
G <- function(lambda) {
    matrix(c(-1, -1 * lambda - 1), ncol = 1)
}

## Variance of the estimator
V_GMM <- function(G, Finv, Y) {
    n <- length(Y)
    1 / n * solve(t(G) %*% Finv %*% G)
    }


## Catch the results
results <- matrix(, nrow = 4, ncol = 4)
colnames(results) <- c("1st moment", "2nd moment", "Identity", "Two Step")
rownames(results) <- c("lambda", "se", "J-stat", "chi2")


W <- diag(2)
W[2,2] <- 0  ## All weight on first moment condition
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y))
(results[1,1] <- poissonGMM.opt$minimum)
(results[3,1] <- length(Y) * poissonGMM.opt$objective)
Finv <- poissonGMM_Finv(poissonGMM.opt$minimum,Y)
Finv[1,2] = 0
Finv[2,1] = 0
Finv[2,2] = 0
(results[2,1] <- sqrt(V_GMM(G(poissonGMM.opt$minimum), Finv, Y)))

W <- diag(2)
W[1,1] <- 0 ## All weight on second moment condition
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y))
(results[1,2] <- poissonGMM.opt$minimum)
(results[3,2] <- length(Y) * poissonGMM.opt$objective)
Finv <- poissonGMM_Finv(poissonGMM.opt$minimum,Y)
Finv[1,1] = 0
Finv[1,2] = 0
Finv[2,1] = 0
(results[2,2] <- sqrt(V_GMM(G(poissonGMM.opt$minimum), Finv, Y)))

W <- diag(2)  ## Identity, equal weights on both moments
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y))
(results[1,3] <- lambdafirst <- poissonGMM.opt$minimum)
(results[3,3] <- length(Y) * poissonGMM.opt$objective)
(results[2,3] <- sqrt(V_GMM(G(lambdafirst), poissonGMM_Finv(lambdafirst,Y), Y)))

## Optimal weights in second step
Wstar <- poissonGMM_Finv(lambdafirst, Y = Y)
poissonGMM.opt <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=Wstar, Y = Y)
(results[1,4] <- lambdastar <- poissonGMM.opt$minimum)
(results[3,4] <- length(Y) * poissonGMM.opt$objective)
Finv <- poissonGMM_Finv(lambdastar, Y)
(results[2,4] <- sqrt(V_GMM(G(lambdastar), Finv, Y)))


results[4,] <- pchisq(results[3,],1, lower.tail=FALSE)

round(results,3)






## Above uses lambda = mean(Y) in the second expression.
## Instead compute lambda to make both true
poissonGMM_Q <- function(lambda, Y, W) {
    W <- W
    f <- matrix(c(mean(Y) - lambda, mean(Y^2) - lambda^2 - lambda), ncol=1) 
    t(f) %*% W %*% f
}
## Generate optimal weighting matrix Wstar = Finv from first-step values
poissonGMM_Finv <- function(lambda, Y) {
    first_moment_deviation <- Y - lambda
    second_moment_deviation <- Y^2 - lambda^2 - lambda
    F11 <- mean(first_moment_deviation^2)
    Fc <- mean(first_moment_deviation * second_moment_deviation)
    F22 <- mean(second_moment_deviation^2)
    print(F <- matrix(c(F11, Fc, Fc, F22), ncol=2))
    print(Finv <- solve(F))
    Finv
}

W <- diag(2)
W[2,2] <- 0  ## All weight on first moment condition
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y))
sqrt(V_GMM(G(poissonGMM.opt$minimum), W, Y))

W <- diag(2)
W[1,1] <- 0 ## All weight on second moment condition
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y))
sqrt(V_GMM(G(poissonGMM.opt$minimum), W, Y))

W <- diag(2)  ## Identity, equal weights on both moments
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y))
sqrt(V_GMM(G(poissonGMM.opt$minimum), W, Y))
lambdafirst <- poissonGMM.opt$minimum

## Optimal weights in second step
Wstar <- poissonGMM_Finv(lambdafirst, Y = Y)
(lambdastar <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=Wstar, Y = Y)$minimum)
sqrt(V_GMM(G(lambdastar), Wstar, Y))





## Another variant for defining the moments 
poissonGMM_Q <- function(lambda, Y, W) {
    W <- W
    f <- matrix(c(mean(Y) - lambda, mean((Y - lambda)^2) - lambda), ncol=1)
    t(f) %*% W %*% f
}
## Generate optimal weighting matrix Wstar = Finv from first-step values
poissonGMM_Finv <- function(lambda,Y) {
    first_moment_deviation <- Y - lambda
    second_moment_deviation <- Y^2 - Y*lambda + lambda^2 - lambda
    F11 <- mean(first_moment_deviation^2)
    Fc <- mean(first_moment_deviation * second_moment_deviation)
    F22 <- mean(second_moment_deviation^2)
    print(F <- matrix(c(F11, Fc, Fc, F22), ncol=2))
    print(Finv <- solve(F))
    Finv
}

W <- diag(2)
W[2,2] <- 0  ## All weight on first moment condition
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y))
sqrt(V_GMM(G(poissonGMM.opt$minimum), W, Y))

W <- diag(2)
W[1,1] <- 0 ## All weight on second moment condition
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval=c(min(Y),max(Y^2)), W=W, Y = Y))
sqrt(V_GMM(G(poissonGMM.opt$minimum), W, Y))

W <- diag(2)  ## Identity, equal weights on both moments
(poissonGMM.opt  <- optimize(poissonGMM_Q, interval = c(min(Y),max(Y^2)), W=W, Y = Y))
sqrt(V_GMM(G(poissonGMM.opt$minimum), W, Y))
lambdafirst <- poissonGMM.opt$minimum

## Optimal weights in second step
Wstar <- poissonGMM_Finv(lambdafirst, Y = Y)
(lambdastar <- optimize(poissonGMM_Q, interval = c(min(Y), max(Y^2)), W = Wstar, Y = Y)$minimum)
sqrt(V_GMM(G(lambdastar), Wstar, Y))
