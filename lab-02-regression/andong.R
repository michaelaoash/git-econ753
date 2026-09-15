## ZAP
## Based on Zhu, Ash, and Pollin (2006) https://doi.org/10.1080/0269217032000148645
## Replication of Levine and Zervos (1998)
## Frisch Waugh Lovell partitioned regression model
library(tidyverse)
library(haven)
library(lmtest)
library(sandwich)
options(scipen=1000)

library(here)
andong <- read_dta(here("lab-02-regression", "sbegnew.dta"))

## Data dictionary for "Stock Markets, Banks, and Economic Growth"
## GYP     Average Annual GDP Growth Rate 1976-1993
## LRGDP   Initial Output
## LSEC    Secondary-School Enrollment
## REVCOUP Revolutions and Coups
## GOVI    Initial value of Government
## PII     Initial value of Inflation
## BMPI    Initial value of Black Market Premium
## BANKI   Initial value of Bank Credit
## TORI    Initial Turnover Ratio
## TVTI    Initial Value Traded
## MCAPI   Initial Capitalization
## VOLI    Initial Volatility
## CAPM    CAPM Integration
## APM     APT Integration.
## Variables without the subscript "I" indicate that the value is averaged over the sample period, instead of an initial value, unless otherwise noted.



frisch_waugh_lovell <- function(df, y, x, label, control) {
    ## Frisch Waugh Lovell Partitioned Regression
    ## Shih-Yen Pan & Michael Ash (2021)
    ## df is the dataframe, y is the dependent variable, x is the key independent variable
    ## control is a list of control variables

    #' @importFrom rlang .data

    df  <- tidyr::drop_na(df, tidyselect::any_of(c(y, x, control)))
    df  <- dplyr::select(df, tidyselect::all_of(c(label, y, x, control)))
    print(df)
    control <- (paste(control, collapse = " + "))

    ## Bivariate regression for comparison
    reg_bi <- as.formula(paste(y, " ~ ", x))
    print("Bivariate Regression")
    print(coeftest(lm_bi <- lm(reg_bi, df)))

    df  <- dplyr::mutate(df,
                         y_bi = predict(lm_bi)
                         )

    ## Multivariate regression for comparison
    reg_mvr <- as.formula(paste(y, " ~ ", x, " + ", control))
    print("Multivariate Regression")
    print(coeftest(lm(reg_mvr, df)))

    ## residualize y on the control variables
    print(paste(y, " ~ ", control))
    reg_ycontrol <- as.formula(paste(y, " ~ ", control))
    print(reg_ycontrol)
    
    uy_lm  <- lm(reg_ycontrol, df)

    ## residualize x on the control variables
    reg_xcontrol <- as.formula(paste(x, " ~ ", control))
    ux_lm <- lm(reg_xcontrol, df)

    df  <- dplyr::mutate(df,
                  u_y = resid(uy_lm),
                  u_x = resid(ux_lm)
                  )

    ## Frisch-Waugh-Lovell regression
    fwl_lm <- lm(u_y ~ 0 + u_x, data = df)
    print("Partitioned Regression")
    print(coeftest(fwl_lm))
    print(df)

    dev.new()

    print(ggplot2::ggplot(data = df, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
          ggplot2::geom_point() +
          ggplot2::geom_text(ggplot2::aes(label = .data[[label]]), hjust = 0) +
          ggplot2::geom_smooth(method = "lm") +
          ggplot2::labs(title = "Bivariate regression"))

    dev.new()

    print(ggplot2::ggplot(data = df, ggplot2::aes(x = .data[["u_x"]], y = .data[["u_y"]])) +
          ggplot2::geom_point() +
          ggplot2::geom_text(ggplot2::aes(label = .data[[label]]), hjust = 0) +
          ggplot2::geom_smooth(method = "lm") +
          ggplot2::labs(title = "Partitioned Regression"))
}


summary(andong_lm <- lm(gyp ~ tori + lrgdp + lsec + revcoup + govi + pii + bmpi + bpyi, data = andong))
coeftest(andong_lm, vcov = vcovHC(andong_lm, type = "HC1"))
coeftest(andong_lm, vcov = vcovHC(andong_lm, type = "HC3"))


## list of control variables
mycontrols <- c("lrgdp", "lsec", "revcoup", "govi", "pii", "bmpi", "bpyi")

frisch_waugh_lovell(andong, "gyp", "tori", "country", mycontrols)

frisch_waugh_lovell(filter(andong, !(country %in% c("TWN", "KOR"))), "gyp", "tori", "country", mycontrols)


## Not implemented: test omitting outliers
## plot(toriu,gypu)
## text(toriu,gypu,name)
## print("Left-click each country to omit; right-click when done.")
## omit <- identify(toriu,gypu,plot=FALSE)
## name[omit]
## except <- andong[-omit]

