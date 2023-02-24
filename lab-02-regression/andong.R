## ZAP
## Based on Zhu, Ash, and Pollin (2006) https://doi.org/10.1080/0269217032000148645
## Frisch Waugh Lovell partitioned regression model
library(tidyverse)
library(haven)
library(lmtest)
options(scipen=1000)

andong <- read_dta("sbegnew.dta")

frisch_waugh_lovell <- function(df, y, x, label, control) {
    ## Frisch Waugh Lovell Partitioned Regression
    ## Shih-Yen Pan & Michael Ash (2021)
    ## df is the dataframe, y is the dependent variable, x is the key independent variable
    ## control is a list of control variables
    df  <- drop_na(df, any_of(c(y,x,control)))
    df  <- select(df,c(label,y,x,control))
    print(df)
    control <- (paste(control, collapse = " + ") )
    
    ## Bivariate regression for comparison
    reg_bi <- as.formula(paste(y, " ~ ", x))
    print("Bivariate Regression")
    print(coeftest(lm_bi <- lm(reg_bi, df)))

    df  <- mutate(df,
                  y_bi = predict(lm_bi)
                  )

    ## Multivariate regression for comparison
    reg_mvr <- as.formula(paste(y, " ~ ", x, " + ", control))
    print("Multivariate Regression")
    print(coeftest(lm_mvr <- lm(reg_mvr, df)))

    ## residualize y on the control variables
    reg_ycontrol <- as.formula(paste(y, " ~ ", control))
    uy.lm  <- lm(reg_ycontrol, df)

    ## residualize x on the control variables
    reg_xcontrol <- as.formula(paste(x, " ~ ", control))
    ux.lm <- lm(reg_xcontrol, df)

    df  <- mutate(df,
                  u_y = resid(uy.lm),
                  u_x = resid(ux.lm)
                  )

    ## Frisch-Waugh-Lovell regression
    fwl.lm <- lm(u_y ~ 0 + u_x, data=df)
    print("Partitioned Regression")
    print(coeftest(fwl.lm))
    print(df)

    dev.new()

    print(ggplot(data=df, aes_string(x=x, y=y) ) +
          geom_point() +
          geom_text(aes_string(label=label),hjust=0) +
          geom_smooth(method="lm") +
          labs(title="Bivariate regression"))

    dev.new()

    print(ggplot(data=df, aes(x=u_x, y=u_y) ) +
          geom_point() +
          geom_text(aes_string(label=label),hjust=0) +
          geom_smooth(method="lm") +
          labs(title="Partitioned Regression"))
}



## list of control variables
mycontrols <- c("lrgdp", "lsec", "revcoup", "govi", "pii", "bmpi", "bpyi")

frisch_waugh_lovell(andong, "gyp", "tori", "country", mycontrols)

frisch_waugh_lovell(filter(andong, !(country %in% c("TWN","KOR")  )  )  , "gyp", "tori", "country", mycontrols)



## Not implemented: test omitting outliers
## plot(toriu,gypu)
## text(toriu,gypu,name)
## print("Left-click each country to omit; right-click when done.")
## omit <- identify(toriu,gypu,plot=FALSE)
## name[omit]
## except <- andong[-omit]

