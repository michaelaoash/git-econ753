## Annualized inflation over the previous 1, 3, 6, 12, 24, and 36 months
library(tidyverse)
options(dplyr.width=Inf, dplyr.print_max=Inf, scipen=10000)
library(alfred)
library(lubridate)
library(reshape2)
library(ggplot2)

cpiaucsl <- get_fred_series("cpiaucsl")

inflation  <- cpiaucsl %>% mutate(
                               i01 = (cpiaucsl/lag(cpiaucsl))^12 - 1,
                               i03 = (cpiaucsl/lag(cpiaucsl,n=3))^4 - 1,
                               i06 = (cpiaucsl/lag(cpiaucsl,n=6))^2 - 1,
                               i12 = (cpiaucsl/lag(cpiaucsl,n=12)) - 1,
                               i24 = (cpiaucsl/lag(cpiaucsl,n=24))^(1/2) - 1,
                               i36 = (cpiaucsl/lag(cpiaucsl,n=36))^(1/3) - 1,
                               date = date %m+% months(1)
                               )

inflation  <- inflation %>% select(-cpiaucsl) %>% melt(id.vars="date")


inflation %>% filter(as.Date("2012-12-01")<=date) %>%
    ggplot(aes(x=date, y=value, group=variable, color=variable)) + geom_line() +
    scale_y_continuous(labels = scales::percent, breaks=c(-0.02,0,0.02,0.04,0.06,0.08)) +
    scale_x_date(date_breaks = "1 year", date_minor_breaks="1 month")



unrate <- get_fred_series("unrate")


