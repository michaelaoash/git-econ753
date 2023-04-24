## Warning: do not use tidyverse; will disrupt lag operators
pdf(file="chap6-distributed-lag.pdf",height=0,width=0,paper="USr")
library(ggplot2)
library(dynlm)
library(plyr)
library(lmtest)
library(reshape2)
library(nlme)
options(scipen=10000, width=200)
options(rlang_backtrace_on_error = "none")

rm(list=ls())

ld<-function(x)c( x[2:(length(x))],NA )


kopcke <- read.csv("chap6-kopcke.csv")

kopcke.dt <- as.yearqtr(paste(kopcke$rdate %/% 10, kopcke$rdate%%10),"%Y %q")

kopcke.z <- zoo(kopcke,order.by=kopcke.dt)
kopcke.ts <- ts(kopcke,start=c(1952,1),end=c(1986,4),frequency=4,class="matrix")


kopcke.df0 <- data.frame(time=time(kopcke.z), Year=as.numeric(as.yearqtr(time(kopcke.z))),kopcke.z)
kopcke.df1 <- mutate(kopcke.df0,
                     jes=je/js, ies = ie/is, ces = ce / cs,
                     ke = ld(kelag), ks=ld(kslag),kes=ke/ks,
                     ien=ie-0.15*kelag, isn=is-0.05*kslag,
                     ck= (ce*kelag + cs*kslag)/(kelag + kslag) )

kopcke.z2 <- zoo(subset(kopcke.df1,select=-time),order.by=kopcke.dt)

cor(dplyr::transmute(data.frame(kopcke.z2), is, y, y1=dplyr::lag(y), y2=dplyr::lag(y,2), y3=dplyr::lag(y,3)) , use="pairwise")

summary(dynlm(data=kopcke.z2, ie ~ L(y,0:12) + kelag, start=c(1956,1),end=c(1979,4)  ) )

summary(dynlm(data=kopcke.z2, ie ~ L(y,0:12) + kelag, start=c(1956,1),end=c(1986,4)  ) )

kopcke.dlag <- dplyr::mutate(data.frame(kopcke.z2),
                 z0 = y + dplyr::lag(y,1) + dplyr::lag(y,2) + dplyr::lag(y,3) + dplyr::lag(y,4) + dplyr::lag(y,5) + dplyr::lag(y,6) +
                     dplyr::lag(y,7) + dplyr::lag(y,8) + dplyr::lag(y,9) + dplyr::lag(y,10) + dplyr::lag(y,11),
                 z1 = 0*y + 1*dplyr::lag(y,1) + 2*dplyr::lag(y,2) + 3*dplyr::lag(y,3) + 4*dplyr::lag(y,4) + 5*dplyr::lag(y,5) + 6*dplyr::lag(y,6) +
                     7*dplyr::lag(y,7) + 8*dplyr::lag(y,8) + 9*dplyr::lag(y,9) + 10*dplyr::lag(y,10) + 11*dplyr::lag(y,11),
                 z2 = 0^2*y + 1^2*dplyr::lag(y,1) + 2^2*dplyr::lag(y,2) + 3^2*dplyr::lag(y,3) + 4^2*dplyr::lag(y,4) + 5^2*dplyr::lag(y,5) + 6^2*dplyr::lag(y,6) +
                     7^2*dplyr::lag(y,7) + 8^2*dplyr::lag(y,8) + 9^2*dplyr::lag(y,9) + 10^2*dplyr::lag(y,10) + 11^2*dplyr::lag(y,11),
                 z3 = 0^3*y + 1^3*dplyr::lag(y,1) + 2^3*dplyr::lag(y,2) + 3^3*dplyr::lag(y,3) + 4^3*dplyr::lag(y,4) + 5^3*dplyr::lag(y,5) + 6^3*dplyr::lag(y,6) +
                     7^3*dplyr::lag(y,7) + 8^3*dplyr::lag(y,8) + 9^3*dplyr::lag(y,9) + 10^3*dplyr::lag(y,10) + 11^3*dplyr::lag(y,11),)

kopcke.dlag.z <- zoo(kopcke.dlag,order.by=kopcke.dt)


summary(dlag.lm <- dynlm(ie ~ z0 + z1 + z2 + z3, data=kopcke.dlag.z,start=c(1956,1),end=c(1979,4) ))
summary(dlag.lm <- dynlm(ie ~ z0 + z1 + z2 + z3, data=kopcke.dlag.z,start=c(1956,1),end=c(1986,4) ))
summary(dlag.lm <- dynlm(ie ~ z0 + z1 + z2 + z3 + kelag, data=kopcke.dlag.z,start=c(1956,1),end=c(1986,4) ))
summary(dlag.lm <- dynlm(ie ~ z0 + z1 + z2 + z3 + kelag, data=kopcke.dlag.z,start=c(1956,1),end=c(1979,4) ))


b  <- attr(car::linearHypothesis(model=dlag.lm, c("z0 + 0*z1 + 0*z2 + 0*z3"), verbose=TRUE),"value" )
b  <- c(b,attr(car::linearHypothesis(model=dlag.lm, c("z0 + 1*z1 + 1*z2 + 1*z3"), verbose=TRUE),"value"))
b  <- c(b,attr(car::linearHypothesis(model=dlag.lm, c("z0 + 2*z1 + 4*z2 + 8*z3"), verbose=TRUE),"value"))
b  <- c(b,attr(car::linearHypothesis(model=dlag.lm, c("z0 + 3*z1 + 9*z2 + 27*z3"), verbose=TRUE),"value"))
b  <- c(b,attr(car::linearHypothesis(model=dlag.lm, c("z0 + 4*z1 + 16*z2 + 64*z3"), verbose=TRUE),"value"))
b  <- c(b,attr(car::linearHypothesis(model=dlag.lm, c("z0 + 5*z1 + 25*z2 + 125*z3"), verbose=TRUE),"value"))
b  <- c(b,attr(car::linearHypothesis(model=dlag.lm, c("z0 + 6*z1 + 36*z2 + 216*z3"), verbose=TRUE),"value"))
b  <- c(b,attr(car::linearHypothesis(model=dlag.lm, c("z0 + 7*z1 + 49*z2 + 343*z3"), verbose=TRUE),"value"))
b  <- c(b,attr(car::linearHypothesis(model=dlag.lm, c("z0 + 8*z1 + 64*z2 + 512*z3"), verbose=TRUE),"value"))
b  <- c(b,attr(car::linearHypothesis(model=dlag.lm, c("z0 + 9*z1 + 81*z2 + 729*z3"), verbose=TRUE),"value"))
b  <- c(b,attr(car::linearHypothesis(model=dlag.lm, c("z0 + 10*z1 + 100*z2 + 1000*z3"), verbose=TRUE),"value"))
b  <- c(b,attr(car::linearHypothesis(model=dlag.lm, c("z0 + 11*z1 + 121*z2 + 1331*z3"), verbose=TRUE),"value"))

se  <- sqrt(attr(car::linearHypothesis(model=dlag.lm, c("z0 + 0*z1 + 0*z2 + 0*z3"), verbose=TRUE),"vcov" ))
se  <- c(se,sqrt(attr(car::linearHypothesis(model=dlag.lm, c("z0 + 1*z1 + 1*z2 + 1*z3"), verbose=TRUE),"vcov")))
se  <- c(se,sqrt(attr(car::linearHypothesis(model=dlag.lm, c("z0 + 2*z1 + 4*z2 + 8*z3"), verbose=TRUE),"vcov")))
se  <- c(se,sqrt(attr(car::linearHypothesis(model=dlag.lm, c("z0 + 3*z1 + 9*z2 + 27*z3"), verbose=TRUE),"vcov")))
se  <- c(se,sqrt(attr(car::linearHypothesis(model=dlag.lm, c("z0 + 4*z1 + 16*z2 + 64*z3"), verbose=TRUE),"vcov")))
se  <- c(se,sqrt(attr(car::linearHypothesis(model=dlag.lm, c("z0 + 5*z1 + 25*z2 + 125*z3"), verbose=TRUE),"vcov")))
se  <- c(se,sqrt(attr(car::linearHypothesis(model=dlag.lm, c("z0 + 6*z1 + 36*z2 + 216*z3"), verbose=TRUE),"vcov")))
se  <- c(se,sqrt(attr(car::linearHypothesis(model=dlag.lm, c("z0 + 7*z1 + 49*z2 + 343*z3"), verbose=TRUE),"vcov")))
se  <- c(se,sqrt(attr(car::linearHypothesis(model=dlag.lm, c("z0 + 8*z1 + 64*z2 + 512*z3"), verbose=TRUE),"vcov")))
se  <- c(se,sqrt(attr(car::linearHypothesis(model=dlag.lm, c("z0 + 9*z1 + 81*z2 + 729*z3"), verbose=TRUE),"vcov")))
se  <- c(se,sqrt(attr(car::linearHypothesis(model=dlag.lm, c("z0 + 10*z1 + 100*z2 + 1000*z3"), verbose=TRUE),"vcov")))
se  <- c(se,sqrt(attr(car::linearHypothesis(model=dlag.lm, c("z0 + 11*z1 + 121*z2 + 1331*z3"), verbose=TRUE),"vcov")))

ggplot(data.frame(b,se), aes(x = 1:12, y=b, ymin=b-2*se, ymax=b+2*se)) +
    geom_point() + geom_errorbar() +
    scale_x_continuous(breaks=0:12)



(b0 = dlag.lm$coefficients['z0'] + 0*dlag.lm$coefficients['z1'] + 0^2*dlag.lm$coefficients['z2'] + 0^3*dlag.lm$coefficients['z3'])
(b1 = dlag.lm$coefficients['z0'] + 1*dlag.lm$coefficients['z1'] + 1^2*dlag.lm$coefficients['z2'] + 1^3*dlag.lm$coefficients['z3'])
(b2 = dlag.lm$coefficients['z0'] + 2*dlag.lm$coefficients['z1'] + 2^2*dlag.lm$coefficients['z2'] + 2^3*dlag.lm$coefficients['z3'])
(b3 = dlag.lm$coefficients['z0'] + 3*dlag.lm$coefficients['z1'] + 3^2*dlag.lm$coefficients['z2'] + 3^3*dlag.lm$coefficients['z3'])
(b4 = dlag.lm$coefficients['z0'] + 4*dlag.lm$coefficients['z1'] + 4^2*dlag.lm$coefficients['z2'] + 4^3*dlag.lm$coefficients['z3'])
(b5 = dlag.lm$coefficients['z0'] + 5*dlag.lm$coefficients['z1'] + 5^2*dlag.lm$coefficients['z2'] + 5^3*dlag.lm$coefficients['z3'])
(b6 = dlag.lm$coefficients['z0'] + 6*dlag.lm$coefficients['z1'] + 6^2*dlag.lm$coefficients['z2'] + 6^3*dlag.lm$coefficients['z3'])
(b7 = dlag.lm$coefficients['z0'] + 7*dlag.lm$coefficients['z1'] + 7^2*dlag.lm$coefficients['z2'] + 7^3*dlag.lm$coefficients['z3'])
(b8 = dlag.lm$coefficients['z0'] + 8*dlag.lm$coefficients['z1'] + 8^2*dlag.lm$coefficients['z2'] + 8^3*dlag.lm$coefficients['z3'])
(b9 = dlag.lm$coefficients['z0'] + 9*dlag.lm$coefficients['z1'] + 9^2*dlag.lm$coefficients['z2'] + 9^3*dlag.lm$coefficients['z3'])
(b10 = dlag.lm$coefficients['z0'] + 10*dlag.lm$coefficients['z1'] + 10^2*dlag.lm$coefficients['z2'] + 10^3*dlag.lm$coefficients['z3'])
(b11 = dlag.lm$coefficients['z0'] + 11*dlag.lm$coefficients['z1'] + 11^2*dlag.lm$coefficients['z2'] + 11^3*dlag.lm$coefficients['z3'])








## 6e2a Estimate equation 6.14 (equipment e and structures s)
summary(eqn614e <- dynlm(data=kopcke.z2, ie ~ L(y,0:1) + L(ie,1), start=c(1956,1),end=c(1986,4)  ) )
dwtest(eqn614e)
(lambda <- 1 - coef(eqn614e)[4])
(mu <- coef(eqn614e)[2] / lambda)
(delta <- coef(eqn614e)[3] / (mu * lambda) + 1)

summary(eqn614s <- dynlm(data=kopcke.z2, is ~ L(y,0:1) + L(is,1), start=c(1956,1),end=c(1986,4)  ) )
dwtest(eqn614s)
(lambda <- 1 - coef(eqn614s)[4])
(mu <- coef(eqn614s)[2] / lambda)
(delta <- coef(eqn614s)[3] / (mu * lambda) + 1)



(eqn614s_ols  <- lm(data=dplyr::filter(kopcke.df1, Year>1956),
                    is ~ y + dplyr::lag(y) + dplyr::lag(is) ))
dwtest(eqn614s_ols)
(eqn614s_co  <- cochrane.orcutt(eqn614s_ols, max.iter=500))
dwtest(eqn614s_co)
(lambda <- 1 - coef(eqn614s_co)[4])
(mu <- coef(eqn614s_co)[2] / lambda)
(delta <- coef(eqn614s_co)[3] / (mu * lambda) + 1)


(eqn614e_ols  <- lm(data=dplyr::filter(kopcke.df1, Year>1956),
                    ie ~ y + dplyr::lag(y) + dplyr::lag(ie) ))
dwtest(eqn614e_ols)
(eqn614e_co  <- cochrane.orcutt(eqn614e_ols))
dwtest(eqn614e_co)
(lambda <- 1 - coef(eqn614e_co)[4])
(mu <- coef(eqn614e_co)[2] / lambda)
(delta <- coef(eqn614e_co)[3] / (mu * lambda) + 1)






## The gls syntax does not take lag()
summary(gls(ie ~ (y) + kelag , data=kopcke.z2, correlation=corARMA(p=1,q=0)))
summary(gls(ie ~ (y) + kelag , data=kopcke.z2, correlation=corAR1()))

## Be very careful with signs in the lag operator.  Here it works on a zoo object
## Just to demonstrate that we are doing lags correctly
with(kopcke.z2,arima(ie, xreg=cbind(lag(y,-1:-3),kelag), order=c(0,0,0)))  ## Right 
summary(dynlm(data=kopcke.z2, ie ~ L(y,1:3) + kelag)) ## Right

with(kopcke.z2,arima(ie, xreg=cbind(lag(y,-1),kelag), order=c(1,0,0)))  ## Right

with(kopcke.z2,arima(ie, xreg=cbind(lag(y,-1:-3),kelag), order=c(1,0,0)))  ## Right

with(window(kopcke.z2,start="1954 Q1",end="1977 Q4"),arima(ie, xreg=cbind(lag(y,-1:-3),kelag), order=c(1,0,0)))  ## Right

## Cash flow model
with(window(kopcke.z2,start="1958 Q1",end="1973 Q3"),arima( ie , xreg=cbind(lag(f/je,-1:-3),kelag), order=c(1,0,0)))
with(window(kopcke.z2,start="1956 Q1",end="1979 Q4"),arima( ie , xreg=cbind(lag(f/je,-1:-7),kelag), order=c(1,0,0)))
## To get 1956Q1-1979Q4 with seven lags, need to include back to 1954Q2
(flow.ie.iii <- with((window(kopcke.z2,start="1954 Q2",end="1979 Q4")),arima( ie , xreg=cbind(lag(f/je,0:-7),kelag), order=c(1,0,0))))
(flow.is.iii <- with((window(kopcke.z2,start="1954 Q2",end="1979 Q4")),arima( is , xreg=cbind(lag(f/js,0:-7),kelag), order=c(1,0,0))))


## With contemporaneous y
summary(dynlm(data=kopcke.z2, ie ~ L(y,0:3) + kelag))
summary(dynlm(data=kopcke.z2, ie ~ y + L(y,1:3) + kelag))

## dynlm uses the start/end syntax from window()
summary( accelerator.3 <- dynlm(data=kopcke.z2, ie ~ L(y,0:3) + kelag, start="1954 Q1", end="1977 Q4"))
dwtest(accelerator.3)

summary( accelerator.4 <- dynlm(data=kopcke.z2, ie ~ L(y,1:4) + kelag, start=c(1954,1), end=c(1977,4) ))
dwtest(accelerator.4)

summary( autoregressive.2 <- dynlm(data=kopcke.z2, ie ~ L(ie,1:2), start=c(1954,1), end=c(1977,4) ))
dwtest(autoregressive.2)

summary(autoregressive.6 <- dynlm(data=kopcke.z2, ie ~ L(ie,1:6), start=c(1954,1), end=c(1977,4) ))
dwtest(autoregressive.6)


## Back out depreciation rates for equipment based on gross investment and capital stock, 1953 and 1986
subset(kopcke.df1,Year>=1952.75 & Year<1954,select=c(ie,kelag,Year))
(ke52.4 <- kopcke.df1["1952 Q4","kelag"])
(ke53.4 <- kopcke.df1["1953 Q4","kelag"])
(ie53 <- (kopcke.df1["1953 Q1","ie"] + kopcke.df1["1953 Q2","ie"] + kopcke.df1["1953 Q3","ie"] + kopcke.df1["1953 Q4","ie"])/4 )
(delta53 <- 1 - (ke53.4 - ie53) / ke52.4)

subset(kopcke.df1,Year>=1985.75 & Year<1987,select=c(ie,kelag,Year))
(ke85.4 <- kopcke.df1["1985 Q4","kelag"])
(ke86.4 <- kopcke.df1["1986 Q4","kelag"])
(ie86 <- (kopcke.df1["1986 Q1","ie"] + kopcke.df1["1986 Q2","ie"] + kopcke.df1["1986 Q3","ie"] + kopcke.df1["1986 Q4","ie"])/4 )
(delta86 <- 1 - (ke86.4 - ie86) / ke85.4)

## Back out depreciation rates for structures based on gross investment and capital stock, 1953 and 1986
subset(kopcke.df1,Year>=1952.75 & Year<1954,select=c(is,kslag,Year))
(ks52.4 <- kopcke.df1["1952 Q4","kslag"])
(ks53.4 <- kopcke.df1["1953 Q4","kslag"])
(is53 <- (kopcke.df1["1953 Q1","is"] + kopcke.df1["1953 Q2","is"] + kopcke.df1["1953 Q3","is"] + kopcke.df1["1953 Q4","is"])/4 )
(delta53 <- 1 - (ks53.4 - is53) / ks52.4)

subset(kopcke.df1,Year>=1985.75 & Year<1987,select=c(is,kslag,Year))
(ks85.4 <- kopcke.df1["1985 Q4","kslag"])
(ks86.4 <- kopcke.df1["1986 Q4","kslag"])
(is86 <- (kopcke.df1["1986 Q1","is"] + kopcke.df1["1986 Q2","is"] + kopcke.df1["1986 Q3","is"] + kopcke.df1["1986 Q4","is"])/4 )
(delta86 <- 1 - (ks86.4 - is86) / ks85.4)

