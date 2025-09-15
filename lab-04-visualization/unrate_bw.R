pdf("unrate_bw_pdf",width=11,paper="USr")
## This script plots the black and white unemployment rates and the ratio
library(tis) ## for nberDates()
library(scales) ## for format control of dates on x axis
library(reshape2)
library(tidyverse) ## Includes advanced data management and graphics
library(haven) ## to read Stata and other data formats
library(labelled)
library(broom) ## Processing output
library(janitor) ## Advanced tabling commands
library(Hmisc) ## For some nicely formatted summary stats
library(fredr)

fredr_set_key("2c43987fff98daa62e436c00e39f99ff")

## Recession shading from 
## https://datamatters.blog/2011/08/15/recession-bars/#comments
## https://rpubs.com/FSl/609471
(nber <- data.frame(nberDates()) %>%
     mutate(
         Start = as.Date(paste(floor(Start / 10000), "-", floor(Start / 100) %% 100, "-", Start %% 100, sep = "")),
         End = as.Date(paste(floor(End / 10000), "-", floor(End / 100) %% 100, "-", End %% 100, sep = "")))
)
nber.trim <- subset(nber, Start > as.Date("1970-01-01"))

## Unemployment rates U and 04 "not seasonally adjusted"
## Unemployment rates S and 14 "seasonally adjusted"
## 6 "black"
## 3 "white"

params <- list(
    series_id = c("LNS14000006", "LNS14000003"),  ## alternative c("LNU04000006", "LNU04000003", "LNS14000006", "LNS14000003"),
    frequency = c("m"),   ## Be sure to label data monthly or quarterly
    aggregation_method = c("avg"),  ## alternatives: eop, sum
    observation_start = as.Date("1970-01-01")
)
bw <- purrr::pmap(
                 .l = params,
                 .f = ~ fredr(series_id = ..1, frequency = ..2, aggregation_method = ..3, observation_start = ..4)) |>
    list_rbind()


bw_wide <- bw %>%
    pivot_wider(id_cols = date, names_from = series_id) %>%
    filter(date >= as.Date("1972-01-01"))

bw_df <- mutate(bw_wide, ratio = LNS14000006 / LNS14000003, gap = LNS14000006 - LNS14000003)


bw_df[which(bw_df$LNS14000006 == min(bw_df$LNS14000006)),]
bw_df[which(bw_df$LNS14000003 == min(bw_df$LNS14000003)),]
bw_df[which(bw_df$LNS14000003 == min(bw_df$LNS14000006)),]
bw_df[which(bw_df$LNS14000006 == min(bw_df$LNS14000003)),]
bw_df[which(bw_df$LNS14000003 == max(bw_df$LNS14000003)),]
bw_df[which(bw_df$LNS14000006 == max(bw_df$LNS14000003)),]


bplot_triptych <- ggplot(melt(bw_df, id.vars = c("date"), variable.name = "race")) +
    geom_line(aes(x = date, y = value, group = race)) +
    facet_wrap(~ race, ncol = 1, scales = "free_y") +
    xlab("Year") +
    scale_x_date(date_breaks = "2 years", date_minor_breaks = "1 year", labels = date_format("%Y")) +
    labs(title = "Ratio of Black:White Unemployment\nMonthly Data SA")
print(bplot_triptych + geom_rect(data = nber.trim, aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), fill = "pink", alpha = 0.4))


bplot <- ggplot(bw_df) +
    xlab("Year") +
    scale_x_date(date_breaks  =  "2 years", date_minor_breaks  =  "1 year", labels = date_format("%Y")) +
    labs(title = "Ratio of Black:White Unemployment\nMonthly Data SA") + coord_cartesian(ylim = c(1, 3.5))
print(bplot +
      geom_line(aes(x = date, y = ratio)) +
      geom_rect(data = nber.trim, aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), fill = "pink", alpha = 0.4))


## Seasonally adjusted, black and white unemployment on same scales, ratio on separate scale
bplot_bw <- ggplot(filter(melt(bw_df, id.vars = "date", variable.name = "race"), !(race %in% c("gap", "ratio")))) +
    geom_line(aes(x = date,  y = value,  group = race,  linetype = race)) +
    xlab("Year") + ylab("Unemployment") +
    labs(title = "Black and White Unemployment\nMonthly Data SA") + coord_cartesian(ylim = c(0, 20)) + theme(legend.position = "top") +
    scale_x_date(date_breaks  =  "3 years",  date_minor_breaks  =  "1 year",  labels = date_format("%Y")) +
    geom_rect(data = nber.trim, aes(xmin = Start, xmax = End, ymin = -Inf,  ymax = +Inf), fill = "pink",  alpha = 0.4)
print(bplot_bw)

bplot_ratio <- ggplot(filter(melt(bw_df, id.vars = "date", variable.name = "race"), race == "ratio")) +
    geom_line(aes(x = date, y = value, group = race)) +
    xlab("Year") + ylab("Ratio") +
    labs(title = "Black:White Unemployment Ratio\nMonthly Data SA") + coord_cartesian(ylim = c(1.5, 3)) +
    scale_x_date(date_breaks = "3 years", date_minor_breaks = "1 year", labels = date_format("%Y")) +
    geom_rect(data = nber.trim, aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf), fill = "pink", alpha = 0.4)
print(bplot_ratio)

library(gridExtra)
grid.arrange(bplot_bw, bplot_ratio, ncol = 1)
library(cowplot)
plot_grid(bplot_bw, bplot_ratio, align = "v", nrow = 2, rel_heights = c(1.8, 1))
