## Warning: do not use tidyverse; will disrupt lag operators
pdf(width=11,paper="USr")
library(ggplot2)
library(dynlm)
library(plyr)
library(lmtest)
library(reshape2)
library(nlme)
options(scipen=10000)

rm(list=ls())

ld<-function(x)c( x[2:(length(x))],NA )

cochrane.orcutt <- function(mod){
     X <- model.matrix(mod)
     y <- model.response(model.frame(mod))
     e <- residuals(mod)
     n <- length(e)
     names <- colnames(X)
     rho <- sum(e[1:(n-1)]*e[2:n])/sum(e^2)
     ## cheat and know the answer (then it works)
     ## rho <- 0.868
     y <- y[2:n] - rho * y[1:(n-1)]
     X <- X[2:n,] - rho * X[1:(n-1),]
     mod <- lm(y ~ X - 1)
     result <- list()
     result$coefficients <- coef(mod)
     names(result$coefficients) <- names
     summary <- summary(mod, corr = F)
     result$cov <- (summary$sigma^2) * summary$cov.unscaled
     dimnames(result$cov) <- list(names, names)
     result$sigma <- summary$sigma
     result$rho <- rho
     class(result) <- 'cochrane.orcutt'
     result
     }



kopcke <- read.csv("chap6-kopcke.csv")

kopcke.dt <- as.yearqtr(paste(kopcke$rdate %/% 10, kopcke$rdate%%10),"%Y %q")

kopcke.z <- zoo(kopcke,order.by=kopcke.dt)
kopcke.ts <- ts(kopcke,start=c(1952,1),end=c(1986,4),frequency=4,class="matrix")

## plot(cbind(kopcke.z$je/kopcke.z$js,kopcke.z$ie/kopcke.z$is,kopcke.z$kelag/kopcke.z$kslag))

kopcke.df0 <- data.frame(time=time(kopcke.z), Year=as.numeric(as.yearqtr(time(kopcke.z))),kopcke.z)
kopcke.df1 <- mutate(kopcke.df0,
                     jes=je/js, ies = ie/is, ces = ce / cs,
                     ke = ld(kelag), ks=ld(kslag),kes=ke/ks,
                     ien=ie-0.15*kelag, isn=is-0.05*kslag,
                     ck= (ce*kelag + cs*kslag)/(kelag + kslag) )



kopcke.z2 <- zoo(subset(kopcke.df1,select=-time),order.by=kopcke.dt)
## zoo makes window() flaky on start/end 
## frequency(kopcke.z2)
## time(window(kopcke.z2,start="1954 Q1",end="1977 Q4"))  ## Good
## time(window(kopcke.z2,start=c(1954,1),end=c(1977,4),frequency=4)) ## Bad

kopcke.melt <- melt(kopcke.df1, id.vars=c("Year"))

## To set the series order for plotting
kopcke.melt$variable <- factor(kopcke.melt$variable, levels = c("time","rdate","ck","js","je","f","is","ie","kelag","kslag","y","u","q","cs","ce","jes","ies","ke","ks","kes","ien","isn" ))

n <- ggplot(subset(kopcke.melt, variable=="jes" | variable=="ies" | variable=="kes"),aes(x=Year,y=value,color=variable)) + facet_wrap( ~ variable,ncol=1,scales="free" ) + geom_line()
print(n)

n <- ggplot(subset(kopcke.melt, variable=="ien" | variable=="isn"),aes(x=Year,y=value,color=variable)) + facet_wrap( ~ variable,ncol=1,scales="free" ) + geom_line()
print(n)

## dev.new()

n <- ggplot(subset(kopcke.melt, variable=="cs" | variable=="ce"),aes(x=Year,y=value,color=variable)) + facet_wrap( ~ variable,ncol=1,scales="free" ) + geom_line()
print(n)

## dev.new()

## Q
n <- ggplot(subset(kopcke.melt, variable=="q"),aes(x=Year,y=value,color=variable)) + facet_wrap( ~ variable,ncol=1,scales="free" ) + geom_line()
print(n)

## Q and Capacity Utilization
n <- ggplot(subset(kopcke.melt, variable=="q" | variable=="u"),aes(x=Year,y=value,color=variable)) + facet_wrap( ~ variable,ncol=1,scales="free" ) + geom_line()
print(n)

## Q and Cost of Capital
n <- ggplot(subset(kopcke.melt, variable=="ck" | variable=="q"),aes(x=Year,y=value,color=variable)) + facet_wrap( ~ variable,ncol=1,scales="free" ) + geom_line()
print(n)

## Q and Cost of Capital Scatterplot
n <- ggplot(kopcke.df1,aes(x=ck,y=q)) + geom_point() + geom_smooth(method="lm",se=FALSE)
print(n)

## Q and Capacity Utilization Scatterplot
n <- ggplot(kopcke.df1,aes(x=u,y=q)) + geom_point() + geom_smooth(method="lm",se=FALSE)
print(n)


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
(eqn614s_co  <- cochrane.orcutt(eqn614s_ols))
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

