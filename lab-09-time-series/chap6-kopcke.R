##pdf(width = 11, paper = "USr")

## Kopcke private fixed capital investment
## https://fred.stlouisfed.org/release?rid=356

library(ggplot2)
library(tidyverse)
library(lmtest)
library(reshape2)
library(nlme)
library(here)
library(dynlm)
options(scipen = 10000)

rm(list = ls())

ld <- function(x) c(x[2:(length(x))], NA)

cochrane_orcutt <- function(mod) {
     X <- model.matrix(mod)
     y <- model.response(model.frame(mod))
     e <- residuals(mod)
     n <- length(e)
     names <- colnames(X)
     rho <- sum(e[1:(n-1)] * e[2:n]) / sum(e^2)
     ## cheat and know the answer (then it works)
     ## rho <- 0.868
     y <- y[2:n] - rho * y[1:(n-1)]
     X <- X[2:n, ] - rho * X[1:(n-1), ]
     mod <- lm(y ~ X - 1)
     result <- list()
     result$coefficients <- coef(mod)
     names(result$coefficients) <- names
     summary <- summary(mod, corr = F)
     result$cov <- (summary$sigma^2) * summary$cov.unscaled
     dimnames(result$cov) <- list(names, names)
     result$sigma <- summary$sigma
     result$rho <- rho
     class(result) <- "cochrane_orcutt"
     result
    }



kopcke <- read.csv(here("lab-09-time-series/chap6-kopcke.csv"))

kopcke_dt <- as.yearqtr(paste(kopcke$rdate %/% 10, kopcke$rdate %% 10), "%Y %q")

kopcke_z <- zoo(kopcke, order.by = kopcke_dt)
kopcke.ts <- ts(kopcke, start = c(1952, 1), end = c(1986, 4), frequency = 4, class = "matrix")

## plot(cbind(kopcke_z$je/kopcke_z$js, kopcke_z$ie/kopcke_z$is, kopcke_z$kelag/kopcke_z$kslag))

kopcke.df0 <- data.frame(time = time(kopcke_z), Year = as.numeric(as.yearqtr(time(kopcke_z))), kopcke_z)
kopcke.df1 <- mutate(kopcke.df0,
                     jes = je / js, ies  =  ie / is, ces  =  ce / cs,
                     ke = ld(kelag), ks = ld(kslag), kes = ke / ks,
                     k = ke + ks,
                     ky  =  (ke + ks) / y,
                     ien = ie - 0.15 * kelag, isn = is - 0.05 * kslag,
                     ien_alt = lead(kelag) - kelag, isn_alt = lead(kslag) - kslag,
                     ck =  (ce * kelag + cs * kslag) / (kelag + kslag))


kopcke_z2 <- zoo(subset(kopcke.df1, select = -time), order.by = kopcke_dt)
## zoo makes window() flaky on start/end
## frequency(kopcke_z2)
## time(window(kopcke_z2, start = "1954 Q1", end = "1977 Q4"))  ## Good
## time(window(kopcke_z2, start = c(1954, 1), end = c(1977, 4), frequency = 4)) ## Bad

kopcke.melt <- melt(kopcke.df1, id.vars = c("Year"))

## To set the series order for plotting
kopcke.melt$variable <- factor(kopcke.melt$variable,
                               levels  =  c("time", "rdate", "ck", "js", "je", "f", "is", "ie", "kelag", "kslag", "k", "y", "ky", "u", "q", "cs", "ce", "jes", "ies", "ke", "ks", "kes", "ien", "isn"))

n <- ggplot(subset(kopcke.melt, variable=="jes" | variable=="ies" | variable=="kes"), aes(x = Year, y = value, color = variable)) + facet_wrap(~ variable, ncol = 1, scales = "free") + geom_line()
print(n)

n <- ggplot(subset(kopcke.melt, variable=="ien" | variable=="isn"),
            aes(x = Year, y = value, color = variable)) +
    facet_wrap(~ variable, ncol = 1, scales = "free") +
    geom_line()
print(n)


n <- ggplot(subset(kopcke.melt, variable=="ien" | variable=="ien_alt"),
            aes(x = Year, y = value, color = variable)) +
    facet_wrap(~ variable, ncol = 1, scales = "fixed") +
    geom_line()
print(n)


n <- ggplot(subset(kopcke.melt, variable=="ke" | variable=="ks"),
            aes(x = Year, y = value, color = variable)) +
    facet_wrap(~ variable, ncol = 1, scales = "free") +
    geom_line()
print(n)

## Output, capital stock and capital-to-output ratio
n <- ggplot(subset(kopcke.melt, variable=="ky" | variable == "y" | variable == "k"),
            aes(x = Year, y = value, color = variable)) +
    facet_wrap(~ variable, ncol = 1, scales = "free") +
    geom_line()
print(n)

## Net investment, capital stock, and cost of capital (equipment)
n <- ggplot(subset(kopcke.melt, variable=="ien" | variable=="ke" | variable=="ce"),
            aes(x = Year, y = value, color = variable)) +
    facet_wrap(~ variable, ncol = 1, scales = "free") +
    geom_line()
print(n)


## dev.new()

## Cost of capital
n <- ggplot(subset(kopcke.melt, variable=="cs" | variable=="ce"),
            aes(x = Year, y = value, color = variable)) +
    facet_wrap(~ variable, ncol = 1, scales = "free") +
    geom_line()
print(n)

## dev.new()

## Q
n <- ggplot(subset(kopcke.melt, variable=="q"),
            aes(x = Year, y = value, color = variable)) +
    facet_wrap(~ variable, ncol = 1, scales = "free") +
    geom_line()
print(n)

## Q and Capacity Utilization
n <- ggplot(subset(kopcke.melt, variable=="q" | variable=="u"),
            aes(x = Year, y = value, color = variable)) +
    facet_wrap(~ variable, ncol = 1, scales = "free") +
    geom_line()
print(n)

## Q and Cost of Capital
n <- ggplot(subset(kopcke.melt, variable=="ck" | variable=="q"),
            aes(x = Year, y = value, color = variable)) +
    facet_wrap(~ variable, ncol = 1, scales = "free") +
    geom_line()
print(n)

## Q and Cost of Capital Scatterplot
n <- ggplot(kopcke.df1, aes(x = ck, y = q)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
print(n)

## Q and Capacity Utilization Scatterplot
n <- ggplot(kopcke.df1, aes(x = u, y = q)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
print(n)




## 6e2a Estimate equation 6.14 (equipment e and structures s)
summary(eqn614e <- dynlm(data = kopcke_z2, ie ~ L(y, 0:1) + L(ie, 1), start = c(1956, 1), end = c(1986, 4)))
dwtest(eqn614e)
(lambda <- 1 - coef(eqn614e)[4])
(mu <- coef(eqn614e)[2] / lambda)
(delta <- coef(eqn614e)[3] / (mu * lambda) + 1)

summary(eqn614s <- dynlm(data = kopcke_z2, is ~ L(y, 0:1) + L(is, 1), start = c(1956, 1), end = c(1986, 4)))
dwtest(eqn614s)
(lambda <- 1 - coef(eqn614s)[4])
(mu <- coef(eqn614s)[2] / lambda)
(delta <- coef(eqn614s)[3] / (mu * lambda) + 1)



lmqfd <- function(y, X, rho){
    yqfd <- y[2:length(y)] - rho * y[1:length(y)-1]
    Xqfd <- X[2:length(y), ] - rho * X[1:length(y)-1, ]
    return(lm(yqfd ~ 0 + Xqfd))
    }



hildreth_lu <- function(mod) {
    rmses <- data.frame(rho = double(), rmse = double())
    y <- model.response(model.frame(mod))
    X <- model.matrix(mod)
    for (rho in seq(0, 1, 0.01)) {
        lmqfd.lm <- lmqfd(y, X, rho)
        rmse <- sqrt(sum((resid(lmqfd.lm))^2)/(length(y)-1))
        rmses <- bind_rows(rmses, data.frame(rho, rmse))
    }
    rhostar <- with(rmses, rmses[rmse==min(rmse), "rho"])
    print("rhostar is")
    print(rhostar)
    print(summary(lmqfd.lm <<- lmqfd(y, X, rhostar)))
    print(dwtest(lmqfd(y, X, rhostar)))
    return(rmses)
}



(eqn614s_ols  <- lm(data = dplyr::filter(kopcke.df1, Year > 1956),
                    is ~ y + dplyr::lag(y) + dplyr::lag(is)))
(lambda <- 1 - coef(eqn614s_ols)[4])
(mu <- coef(eqn614s_ols)[2] / lambda)
(delta <- coef(eqn614s_ols)[3] / (mu * lambda) + 1)

dwtest(eqn614s_ols)
(eqn614s_co  <- cochrane_orcutt(eqn614s_ols))
## dwtest(eqn614s_co)
(lambda <- 1 - coef(eqn614s_co)[4])
(mu <- coef(eqn614s_co)[2] / lambda)
(delta <- coef(eqn614s_co)[3] / (mu * lambda) + 1)

rmses <- hildreth_lu(eqn614s_ols)
summary(lmqfd.lm)
(lambda <- 1 - coef(lmqfd.lm)[4])
(mu <- coef(lmqfd.lm)[2] / lambda)
(delta <- coef(lmqfd.lm)[3] / (mu * lambda) + 1)
rmses %>% ggplot(aes(x = rho, y = rmse)) + geom_line()


## Try without constant
(eqn614s_ols  <- lm(data = dplyr::filter(kopcke.df1, Year > 1956),
                    is ~ 0 + y + dplyr::lag(y) + dplyr::lag(is)))
(lambda <- 1 - coef(eqn614s_ols)[3])
(mu <- coef(eqn614s_ols)[1] / lambda)
(delta <- coef(eqn614s_ols)[2] / (mu * lambda) + 1)
dwtest(eqn614s_ols)
(eqn614s_co  <- cochrane_orcutt(eqn614s_ols))
## dwtest(eqn614s_co)
(lambda <- 1 - coef(eqn614s_co)[3])
(mu <- coef(eqn614s_co)[1] / lambda)
(delta <- coef(eqn614s_co)[2] / (mu * lambda) + 1)
rmses <- hildreth_lu(eqn614s_ols)
summary(lmqfd.lm)
(lambda <- 1 - coef(lmqfd.lm)[3])
(mu <- coef(lmqfd.lm)[1] / lambda)
(delta <- coef(lmqfd.lm)[2] / (mu * lambda) + 1)
rmses %>% ggplot(aes(x = rho, y = rmse)) + geom_line()






(eqn614e_ols  <- lm(data = dplyr::filter(kopcke.df1, Year > 1956),
                    ie ~ y + dplyr::lag(y) + dplyr::lag(ie)))
(lambda <- 1 - coef(eqn614e_ols)[4])
(mu <- coef(eqn614e_ols)[2] / lambda)
(delta <- coef(eqn614e_ols)[3] / (mu * lambda) + 1)
dwtest(eqn614e_ols)
(eqn614e_co  <- cochrane_orcutt(eqn614e_ols))
## dwtest(eqn614e_co)
(lambda <- 1 - coef(eqn614e_co)[4])
(mu <- coef(eqn614e_co)[2] / lambda)
(delta <- coef(eqn614e_co)[3] / (mu * lambda) + 1)
rmses <- hildreth_lu(eqn614e_ols)
summary(lmqfd.lm)
(lambda <- 1 - coef(lmqfd.lm)[4])
(mu <- coef(lmqfd.lm)[2] / lambda)
(delta <- coef(lmqfd.lm)[3] / (mu * lambda) + 1)
rmses %>% ggplot(aes(x = rho, y = rmse)) + geom_line()




## Try without constant
(eqn614e_ols  <- lm(data = dplyr::filter(kopcke.df1, Year > 1956),
                    ie ~ 0+ y + dplyr::lag(y) + dplyr::lag(ie)))
(lambda <- 1 - coef(eqn614e_ols)[3])
(mu <- coef(eqn614e_ols)[1] / lambda)
(delta <- coef(eqn614e_ols)[2] / (mu * lambda) + 1)
dwtest(eqn614e_ols)
(eqn614e_co  <- cochrane_orcutt(eqn614e_ols))
## dwtest(eqn614e_co)
(lambda <- 1 - coef(eqn614e_co)[3])
(mu <- coef(eqn614e_co)[1] / lambda)
(delta <- coef(eqn614e_co)[2] / (mu * lambda) + 1)

rmses <- hildreth_lu(eqn614e_ols)
summary(lmqfd.lm)
(lambda <- 1 - coef(lmqfd.lm)[3])
(mu <- coef(lmqfd.lm)[1] / lambda)
(delta <- coef(lmqfd.lm)[2] / (mu * lambda) + 1)
rmses %>% ggplot(aes(x = rho, y = rmse)) + geom_line()



















## The gls syntax does not take lag()
summary(gls(ie ~ (y) + kelag, data = kopcke_z2, correlation = corARMA(p = 1, q = 0)))
summary(gls(ie ~ (y) + kelag, data = kopcke_z2, correlation = corAR1()))

## Be very careful with signs in the lag operator.  Here it works on a zoo object
## Just to demonstrate that we are doing lags correctly
with(kopcke_z2, arima(ie, xreg = cbind(stats::lag(y, -1:-3), kelag), order = c(0, 0, 0)))  ## Right
summary(dynlm(data = kopcke_z2, ie ~ L(y, 1:3) + kelag)) ## Right

with(kopcke_z2, arima(ie, xreg = cbind(stats::lag(y, -1), kelag), order = c(1, 0, 0)))  ## Right

with(kopcke_z2, arima(ie, xreg = cbind(stats::lag(y, -1:-3), kelag), order = c(1, 0, 0)))  ## Right

with(window(kopcke_z2, start = "1954 Q1", end = "1977 Q4"), arima(ie, xreg = cbind(stats::lag(y, -1:-3), kelag), order = c(1, 0, 0)))  ## Right

## Cash flow model
with(window(kopcke_z2, start = "1958 Q1", end = "1973 Q3"), arima(ie, xreg = cbind(stats::lag(f/je, -1:-3), kelag), order = c(1, 0, 0)))
with(window(kopcke_z2, start = "1956 Q1", end = "1979 Q4"), arima(ie, xreg = cbind(stats::lag(f/je, -1:-7), kelag), order = c(1, 0, 0)))
## To get 1956Q1-1979Q4 with seven lags, need to include back to 1954Q2
(flow.ie.iii <- with((window(kopcke_z2, start = "1954 Q2", end = "1979 Q4")), arima(ie, xreg = cbind(stats::lag(f/je, 0:-7), kelag), order = c(1, 0, 0))))
(flow.is.iii <- with((window(kopcke_z2, start = "1954 Q2", end = "1979 Q4")), arima(is, xreg = cbind(stats::lag(f/js, 0:-7), kelag), order = c(1, 0, 0))))


## With contemporaneous y
summary(dynlm(data = kopcke_z2, ie ~ L(y, 0:3) + kelag))
summary(dynlm(data = kopcke_z2, ie ~ y + L(y, 1:3) + kelag))

## dynlm uses the start/end syntax from window()
summary(accelerator_3 <- dynlm(data = kopcke_z2, ie ~ L(y, 0:3) + kelag, start = "1954 Q1", end = "1977 Q4"))
dwtest(accelerator_3)

summary(accelerator_4 <- dynlm(data = kopcke_z2, ie ~ L(y, 1:4) + kelag, start = c(1954, 1), end = c(1977, 4)))
dwtest(accelerator_4)

summary(autoregressive_2 <- dynlm(data = kopcke_z2, ie ~ L(ie, 1:2), start = c(1954, 1), end = c(1977, 4)))
dwtest(autoregressive_2)

summary(autoregressive_6 <- dynlm(data = kopcke_z2, ie ~ L(ie, 1:6), start = c(1954, 1), end = c(1977, 4)))
dwtest(autoregressive_6)


## Back out depreciation rates for equipment based on gross investment and capital stock, 1953 and 1986
subset(kopcke.df1, Year >= 1952.75 & Year < 1954, select = c(ie, kelag, Year))
(ke52_4 <- kopcke.df1["1952 Q4", "kelag"])
(ke53_4 <- kopcke.df1["1953 Q4", "kelag"])
(ie53 <- (kopcke.df1["1953 Q1", "ie"] + kopcke.df1["1953 Q2", "ie"] + kopcke.df1["1953 Q3", "ie"] + kopcke.df1["1953 Q4", "ie"]) / 4)
(delta53 <- 1 - (ke53_4 - ie53) / ke52_4)

subset(kopcke.df1, Year >= 1985.75 & Year < 1987, select = c(ie, kelag, Year))
(ke85_4 <- kopcke.df1["1985 Q4", "kelag"])
(ke86_4 <- kopcke.df1["1986 Q4", "kelag"])
(ie86 <- (kopcke.df1["1986 Q1", "ie"] + kopcke.df1["1986 Q2", "ie"] + kopcke.df1["1986 Q3", "ie"] + kopcke.df1["1986 Q4", "ie"]) / 4)
(delta86 <- 1 - (ke86_4 - ie86) / ke85_4)

## Back out depreciation rates for structures based on gross investment and capital stock, 1953 and 1986
subset(kopcke.df1, Year >= 1952.75 & Year < 1954, select = c(is, kslag, Year))
(ks52_4 <- kopcke.df1["1952 Q4", "kslag"])
(ks53_4 <- kopcke.df1["1953 Q4", "kslag"])
(is53 <- (kopcke.df1["1953 Q1", "is"] + kopcke.df1["1953 Q2", "is"] + kopcke.df1["1953 Q3", "is"] + kopcke.df1["1953 Q4", "is"]) / 4)
(delta53 <- 1 - (ks53_4 - is53) / ks52_4)

subset(kopcke.df1, Year >= 1985.75 & Year < 1987, select = c(is, kslag, Year))
(ks85_4 <- kopcke.df1["1985 Q4", "kslag"])
(ks86_4 <- kopcke.df1["1986 Q4", "kslag"])
(is86 <- (kopcke.df1["1986 Q1", "is"] + kopcke.df1["1986 Q2", "is"] + kopcke.df1["1986 Q3", "is"] + kopcke.df1["1986 Q4", "is"]) / 4)
(delta86 <- 1 - (ks86_4 - is86) / ks85_4)
