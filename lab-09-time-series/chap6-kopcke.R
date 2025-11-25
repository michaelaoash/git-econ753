## pdf(width = 11, paper = "USr")

## Kopcke private fixed capital investment
## https://fred.stlouisfed.org/release?rid=356

library(ggplot2)
library(tidyverse)
library(lmtest)
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



kopcke <- read_csv(here("lab-09-time-series/chap6-kopcke.csv"))

kopcke <- kopcke %>%
    mutate(
        kopcke_dt = as.yearqtr(paste(rdate %/% 10, rdate %% 10), "%Y %q"),
        date = as.Date(kopcke_dt),
        )

rownames(kopcke) <- kopcke$kopcke_dt

kopcke <- mutate(kopcke,
                     jes = je / js, ies  =  ie / is, ces  =  ce / cs,
                     ke = ld(kelag), ks = ld(kslag), kes = ke / ks,
                     k = ke + ks,
                     ky  =  (ke + ks) / y,
                     ien = ie - 0.15 * kelag, isn = is - 0.05 * kslag,
                     ien_alt = lead(kelag) - kelag, isn_alt = lead(kslag) - kslag,
                     ck =  (ce * kelag + cs * kslag) / (kelag + kslag))


kopcke.melt <- kopcke %>% pivot_longer(cols = -c(rdate, kopcke_dt, date))


n <- ggplot(subset(kopcke.melt, name=="jes" | name=="ies" | name=="kes"),
            aes(x = date, y = value, color = name)) +
    facet_wrap(~ name, ncol = 1, scales = "free") +
    geom_line()
print(n)

n <- ggplot(subset(kopcke.melt, name=="ien" | name=="isn"),
            aes(x = date, y = value, color = name)) +
    facet_wrap(~ name, ncol = 1, scales = "free") +
    geom_line()
print(n)


n <- ggplot(subset(kopcke.melt, name=="ien" | name=="ien_alt"),
            aes(x = date, y = value, color = name)) +
    facet_wrap(~ name, ncol = 1, scales = "fixed") +
    geom_line()
print(n)


n <- ggplot(subset(kopcke.melt, name=="ke" | name=="ks"),
            aes(x = date, y = value, color = name)) +
    facet_wrap(~ name, ncol = 1, scales = "free") +
    geom_line()
print(n)

## Output, capital stock and capital-to-output ratio
n <- ggplot(subset(kopcke.melt, name=="ky" | name == "y" | name == "k"),
            aes(x = date, y = value, color = name)) +
    facet_wrap(~ name, ncol = 1, scales = "free") +
    geom_line()
print(n)

## Net investment, capital stock, and cost of capital (equipment)
n <- ggplot(subset(kopcke.melt, name=="ien" | name=="ke" | name=="ce"),
            aes(x = date, y = value, color = name)) +
    facet_wrap(~ name, ncol = 1, scales = "free") +
    geom_line()
print(n)

## dev.new()

## Cost of capital
n <- ggplot(subset(kopcke.melt, name=="cs" | name=="ce"),
            aes(x = date, y = value, color = name)) +
    facet_wrap(~ name, ncol = 1, scales = "free") +
    geom_line()
print(n)

## dev.new()

## Q and Capacity Utilization
n <- ggplot(subset(kopcke.melt, name=="q" | name=="u"),
            aes(x = date, y = value, color = name)) +
    facet_wrap(~ name, ncol = 1, scales = "free") +
    geom_line()
print(n)

## Q and Cost of Capital
n <- ggplot(subset(kopcke.melt, name=="ck" | name=="q"),
            aes(x = date, y = value, color = name)) +
    facet_wrap(~ name, ncol = 1, scales = "free") +
    geom_line()
print(n)

## Q and Cost of Capital Scatterplot
n <- ggplot(kopcke, aes(x = ck, y = q)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
print(n)

## Q and Capacity Utilization Scatterplot
n <- ggplot(kopcke, aes(x = u, y = q)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
print(n)

## Create a "zoo" object for use with dynlm()
kopcke_z2 <- zoo(kopcke %>% select(ie, is, je, js, kelag, kslag, f, y), order.by = kopcke$kopcke_dt)

## 6e2a Estimate equation 6.14 (equipment e and structures s)
summary(eqn614e <- dynlm(data = kopcke_z2, ie ~ L(y, 0:1) + L(ie, 1), start = c(1956, 1), end = c(1986, 4)))
summary(eqn614e <- dynlm(data = kopcke_z2, ie ~ L(y, 0:1) + L(ie, 1)))
dwtest(eqn614e)
(lambda <- 1 - coef(eqn614e)[["L(ie, 1)"]])
(mu <- coef(eqn614e)[["L(y, 0:1)0"]] / lambda)
(delta <- coef(eqn614e)[["L(y, 0:1)1"]] / (mu * lambda) + 1)

summary(eqn614s <- dynlm(data = kopcke_z2, is ~ L(y, 0:1) + L(is, 1), start = c(1956, 1), end = c(1986, 4)))
dwtest(eqn614s)
(lambda <- 1 - coef(eqn614s)[["L(is, 1)"]])
(mu <- coef(eqn614s)[["L(y, 0:1)0"]] / lambda)
(delta <- coef(eqn614s)[["L(y, 0:1)1"]] / (mu * lambda) + 1)


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



(eqn614s_ols  <- lm(data = filter(kopcke, year(date) > 1956),
                    is ~ y + lag(y) + lag(is)))
(lambda <- 1 - coef(eqn614s_ols)[["lag(is)"]])
(mu <- coef(eqn614s_ols)[["y"]] / lambda)
(delta <- coef(eqn614s_ols)[["lag(y)"]] / (mu * lambda) + 1)

dwtest(eqn614s_ols)
(eqn614s_co  <- cochrane_orcutt(eqn614s_ols))
## dwtest(eqn614s_co)
(lambda <- 1 - coef(eqn614s_co)[["lag(is)"]])
(mu <- coef(eqn614s_co)[["y"]] / lambda)
(delta <- coef(eqn614s_co)[["lag(y)"]] / (mu * lambda) + 1)


## Whoops these are kind of garbage!!
rmses <- hildreth_lu(eqn614s_ols)
summary(lmqfd.lm)
(lambda <- 1 - coef(lmqfd.lm)[["Xqfdlag(is)"]])
(mu <- coef(lmqfd.lm)[["Xqfdy"]] / lambda)
(delta <- coef(lmqfd.lm)[["Xqfdlag(y)"]] / (mu * lambda) + 1)
rmses %>% ggplot(aes(x = rho, y = rmse)) + geom_line()



## Try without constant
(eqn614s_ols  <- lm(data = filter(kopcke, year(date) > 1956),
                    is ~ 0 + y + lag(y) + lag(is)))
(lambda <- 1 - coef(eqn614s_ols)[["lag(is)"]])
(mu <- coef(eqn614s_ols)[["y"]] / lambda)
(delta <- coef(eqn614s_ols)[["lag(y)"]] / (mu * lambda) + 1)
dwtest(eqn614s_ols)
(eqn614s_co  <- cochrane_orcutt(eqn614s_ols))
## dwtest(eqn614s_co)
(lambda <- 1 - coef(eqn614s_co)[["lag(is)"]])
(mu <- coef(eqn614s_co)[["y"]] / lambda)
(delta <- coef(eqn614s_co)[["lag(y)"]] / (mu * lambda) + 1)

## And again kind of no good
rmses <- hildreth_lu(eqn614s_ols)
summary(lmqfd.lm)
(lambda <- 1 - coef(lmqfd.lm)[["Xqfdlag(is)"]])
(mu <- coef(lmqfd.lm)[["Xqfdy"]] / lambda)
(delta <- coef(lmqfd.lm)[["Xqfdlag(y)"]] / (mu * lambda) + 1)
rmses %>% ggplot(aes(x = rho, y = rmse)) + geom_line()




(eqn614e_ols  <- lm(data = filter(kopcke, year(date) > 1956),
                    ie ~ y + lag(y) + lag(ie)))
(lambda <- 1 - coef(eqn614e_ols)[["lag(ie)"]])
(mu <- coef(eqn614e_ols)[["y"]] / lambda)
(delta <- coef(eqn614e_ols)[["lag(y)"]] / (mu * lambda) + 1)
dwtest(eqn614e_ols)
(eqn614e_co  <- cochrane_orcutt(eqn614e_ols))
## dwtest(eqn614e_co)
(lambda <- 1 - coef(eqn614e_co)[["lag(ie)"]])
(mu <- coef(eqn614e_co)[["y"]] / lambda)
(delta <- coef(eqn614e_co)[["lag(y)"]] / (mu * lambda) + 1)

## No good!!
rmses <- hildreth_lu(eqn614e_ols)
summary(lmqfd.lm)
(lambda <- 1 - coef(lmqfd.lm)[["Xqfdlag(ie)"]])
(mu <- coef(lmqfd.lm)[["Xqfdy"]] / lambda)
(delta <- coef(lmqfd.lm)[["Xqfdlag(y)"]] / (mu * lambda) + 1)
rmses %>% ggplot(aes(x = rho, y = rmse)) + geom_line()




## Try without constant
(eqn614e_ols  <- lm(data = filter(kopcke, year(date) > 1956),
                    ie ~ 0+ y + lag(y) + lag(ie)))
(lambda <- 1 - coef(eqn614e_ols)[["lag(ie)"]])
(mu <- coef(eqn614e_ols)[["y"]] / lambda)
(delta <- coef(eqn614e_ols)[["lag(y)"]] / (mu * lambda) + 1)
dwtest(eqn614e_ols)
(eqn614e_co  <- cochrane_orcutt(eqn614e_ols))
## dwtest(eqn614e_co)
(lambda <- 1 - coef(eqn614e_co)[["lag(ie)"]])
(mu <- coef(eqn614e_co)[["y"]] / lambda)
(delta <- coef(eqn614e_co)[["lag(y)"]] / (mu * lambda) + 1)

## No good
rmses <- hildreth_lu(eqn614e_ols)
summary(lmqfd.lm)
(lambda <- 1 - coef(lmqfd.lm)[["Xqfdlag(ie)"]])
(mu <- coef(lmqfd.lm)[["Xqfdy"]] / lambda)
(delta <- coef(lmqfd.lm)[["Xqfdlag(y)"]] / (mu * lambda) + 1)
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
subset(kopcke, year(date) >= 1952.75 & year(date) < 1954, select = c(ie, kelag, date))
(ke52_4 <- kopcke %>% filter(kopcke_dt == "1952 Q4") %>% pull(kelag))
(ke53_4 <- kopcke %>% filter(kopcke_dt == "1953 Q4") %>% pull(kelag))
ie53 <- kopcke %>% filter(kopcke_dt %in% c("1953 Q1", "1953 Q2", "1953 Q3", "1953 Q4")) %>% pull(ie) %>% sum()/4
(delta53 <- 1 - (ke53_4 - ie53) / ke52_4)


subset(kopcke, date >= 1985.75 & date < 1987, select = c(ie, kelag, date))
(ke85_4 <- kopcke %>% filter(kopcke_dt == "1985 Q4") %>% pull(kelag))
(ke86_4 <- kopcke %>% filter(kopcke_dt == "1986 Q4") %>% pull(kelag))
ie86 <- kopcke %>% filter(kopcke_dt %in% c("1986 Q1", "1986 Q2", "1986 Q3", "1986 Q4")) %>% pull(ie) %>% sum()/4
(delta86 <- 1 - (ke86_4 - ie86) / ke85_4)



## Back out depreciation rates for structures based on gross investment and capital stock, 1953 and 1986
subset(kopcke, date >= 1952.75 & date < 1954, select = c(is, kslag, date))
(ks52_4 <- kopcke %>% filter(kopcke_dt == "1952 Q4") %>% pull(kslag))
(ks53_4 <- kopcke %>% filter(kopcke_dt == "1953 Q4") %>% pull(kslag))
is53 <- kopcke %>% filter(kopcke_dt %in% c("1953 Q1", "1953 Q2", "1953 Q3", "1953 Q4")) %>% pull(is) %>% sum()/4
(delta53 <- 1 - (ks53_4 - is53) / ks52_4)

subset(kopcke, date >= 1985.75 & date < 1987, select = c(is, kslag, date))
(ks85_4 <- kopcke %>% filter(kopcke_dt == "1985 Q4") %>% pull(kslag))
(ks86_4 <- kopcke %>% filter(kopcke_dt == "1986 Q4") %>% pull(kslag))
is86 <- kopcke %>% filter(kopcke_dt %in% c("1986 Q1", "1986 Q2", "1986 Q3", "1986 Q4")) %>% pull(is) %>% sum()/4
(delta86 <- 1 - (ks86_4 - is86) / ks85_4)
