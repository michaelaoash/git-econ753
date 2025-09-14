## Annualized inflation over the previous 1, 3, 6, 12, 24, and 36 months
library(tidyverse)
options(dplyr.width = Inf, dplyr.print_max = Inf, scipen = 10000)
library(alfred)
library(fredr)
library(lubridate)
library(reshape2)
library(ggplot2)

cpiaucsl <- get_fred_series("cpiaucsl")  ## CPI-U all items seasonally adjusted

cpilfesl <- get_fred_series("cpilfesl")  ## CPI-U less fuel and food (i.e., "core") seasonally adjusted


## fredr_set_key("2c43987fff98daa62e436c00e39f99ff")
## params <- list(
##     series_id = c("cpiaucsl", "cpilfesl"),
##     frequency = c("m"),
##     aggregation_method = c("avg"),  ## alternatives: eop, sum, avg
##     observation_start = as.Date("1970-01-01")
## )
## inflation <- purrr::pmap(
##                  .l = params,
##                  .f = ~ fredr(series_id = ..1, frequency = ..2, aggregation_method = ..3, observation_start = ..4)) |>
##     list_rbind()


## All items
inflation  <- cpiaucsl %>% mutate(
                               i01 = (cpiaucsl / lag(cpiaucsl))^12 - 1,
                               i03 = (cpiaucsl / lag(cpiaucsl, n = 3))^4 - 1,
                               i06 = (cpiaucsl / lag(cpiaucsl, n = 6))^2 - 1,
                               i12 = (cpiaucsl / lag(cpiaucsl, n = 12)) - 1,
                               i24 = (cpiaucsl / lag(cpiaucsl, n = 24))^(1 / 2) - 1,
                               i36 = (cpiaucsl / lag(cpiaucsl, n = 36))^(1 / 3) - 1,
                               date = date %m+% months(1)
                               )

inflation  <- inflation %>% select(-cpiaucsl) %>% melt(id.vars = "date")


inflation %>%
    filter(as.Date("2012-12-01") <= date) %>%
    ggplot(aes(x = date, y = value, group = variable, color = variable)) + geom_line() +
    scale_y_continuous(labels = scales::percent, breaks = c(-0.02, 0, 0.02, 0.04, 0.06, 0.08)) +
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month")



## Core
inflation  <- cpilfesl %>% mutate(
                               i01 = (cpilfesl / lag(cpilfesl))^12 - 1,
                               i03 = (cpilfesl / lag(cpilfesl, n = 3))^4 - 1,
                               i06 = (cpilfesl / lag(cpilfesl, n = 6))^2 - 1,
                               i12 = (cpilfesl / lag(cpilfesl, n = 12)) - 1,
                               i24 = (cpilfesl / lag(cpilfesl, n = 24))^(1 / 2) - 1,
                               i36 = (cpilfesl / lag(cpilfesl, n = 36))^(1 / 3) - 1,
                               date = date %m+% months(1)
                               )

inflation  <- inflation %>% select(-cpilfesl) %>% melt(id.vars = "date")


inflation %>%
    filter(as.Date("2012-12-01") <= date) %>%
    ggplot(aes(x = date, y = value, group = variable, color = variable)) + geom_line() +
    scale_y_continuous(labels = scales::percent, breaks = c(-0.02, 0, 0.02, 0.04, 0.06, 0.08)) +
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month")
