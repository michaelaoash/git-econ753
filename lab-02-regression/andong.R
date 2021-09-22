## ZAP
## Based on Zhu, Ash, and Pollin (2006) https://doi.org/10.1080/0269217032000148645
## Frisch Waugh Lovell partitioned regression model
library(tidyverse)
library(haven)
andong <- read_dta("sbegnew.dta")

## Clean by deleting observations with any missing data
andong <-subset(andong, !is.na(gyp) & !is.na(tori))

## attach() is bad form but makes it easier to access variables.
## Challenge: rewrite with good tidyverse syntax
attach(andong)

frisch_waugh_lovell <- function(df, y, x, control) {
    ## Frisch Waugh Lovell Partitioned Regression
    ## Shih-Yen Pan & Michael Ash (2021)
    ## df is the dataframe, y is the dependent variable, x is the key independent variable
    ## control is a list of control variables
    control <- (paste(control, collapse = " + ") )

    ## Bivariate regression for comparison
    reg_bi <- as.formula(paste(y, " ~ ", x))
    print("Bivariate Regression")
    print(summary(lm_bi <- lm(reg_bi, df)))

    ## Multivariate regression for comparison
    reg_mvr <- as.formula(paste(y, " ~ ", x, " + ", control))
    print("Multivariate Regression")
    print(summary(lm_mvr <- lm(reg_mvr, df)))

    ## residualize y on the control variables
    reg_ycontrol <- as.formula(paste(y, " ~ ", control))
    u_y <- resid(lm(reg_ycontrol, df))

    ## residualize x on the control variables
    reg_xcontrol <- as.formula(paste(x, " ~ ", control))
    u_x <- resid(lm(reg_xcontrol, df))

    ## Frisch-Waugh-Lovell regression
    lm_u <- lm(u_y ~ 0 + u_x)
    print("Partitioned Regression")
    print(summary(lm_u))
    
    plot(df[[x]],df[[y]], main="Scatterplot and bivariate regression", xlab=x, ylab=y)
    abline(lm_bi)
    dev.new()
    plot(u_x,u_y, main="Residualized scatterplot and partitioned regression", xlab=paste("residualized", x ), ylab= paste("residualized", y))
    abline(lm_u)
}

## list of control variables
mycontrols <- c("lrgdp", "lsec", "revcoup", "govi", "pii", "bmpi", "bpyi")

frisch_waugh_lovell(andong, "gyp", "tori", mycontrols)







## ggplot(andong, aes(y=gyp, x=tori)) + geom_point() + geom_text(label=name) + geom_smooth(method="lm")
## ggplot(data.frame(toriu,gypu,name), aes(x=toriu, y=gypu) ) + geom_point() + geom_text(label=name) + geom_smooth(method="lm")

## Not implemented: test omitting outliers
## plot(toriu,gypu)
## text(toriu,gypu,name)
## print("Left-click each country to omit; right-click when done.")
## omit <- identify(toriu,gypu,plot=FALSE)
## name[omit]
## except <- andong[-omit]

