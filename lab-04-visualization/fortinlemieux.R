pdf(file="fortinlemieux.pdf",family="Times",paper="USr",width=0,height=0)
library(dplyr)
library(ggplot2)
library(reshape2)
library(car)
library(haven)
options(scipen=10000)
options(digits=4)

rm(list=ls())

cpi79  <- 72.6
cpi88  <- 118.3

mw79  <-  2.90 * cpi79/cpi79
mw88  <- 3.35 * cpi79/cpi88


morg88  <- read_dta(file="morg88.dta")
morg88  <- filter(morg88,
                  sex==2,
                  age>=16 & age<=65,
                  I25c==0,
                  paidhre==1)

morg79  <- read_dta(file="morg79.dta")
morg79  <- filter(morg79,
                  sex==2,
                  age>=16 & age<=65,
                  I25c==0,
                  paidhre==1)

morg  <- bind_rows(morg79,morg88)

morg  <- mutate(morg,
                rearnhre = ifelse(year==1979, earnhre/100 * cpi79/cpi79, earnhre/100 * cpi79/cpi88),
                Year = factor(year))

df79  <- data.frame(x1=mw79, x2=mw79, y1=0.01, y2=2.7, Year=factor(1979))
df88  <- data.frame(x1=mw88, x2=mw88, y1=0.01, y2=2.7, Year=factor(1988))

ggplot(morg) + geom_density(aes(x=rearnhre, group=Year, linetype=Year),trim=TRUE) + scale_x_continuous(trans="log",breaks=c(2,5,10,25)) + theme(legend.position = "none") +
    coord_cartesian(xlim=c(1,30)) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = df79, alpha=0.5) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = df88, alpha=0.5) +
    annotate("text", x = 10, y = 0.2, label = "1988") + 
    annotate("text", x = 4, y = 1.1, label = "1979") +
    annotate("text", x = 2.5, y = 2.6, label = "Minimum wage") +
    annotate("text", x = 1.8, y = 2.1, label = "MW 1988") + 
    annotate("text", x = 2.6, y = 2.1, label = "MW 1979")
    
