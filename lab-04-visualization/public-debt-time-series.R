pdf(file="public-debt-time-series.pdf",family="Times",paper="USr",width=0,height=0)
library(dplyr)
options(dplyr.print_max = 1e9,dplyr.width = Inf)
library(ggplot2)
library(reshape2)
library(car)
library(haven)
options(scipen=10000)
options(digits=4)

rm(list=ls())

## Look at the public debt / GDP series

data_here  <- read_dta(file="DATA-3.dta")
data_here  <- mutate(data_here,
                  Year = year,
                  debtgdp_here = debtgdp_mauro,
                  dRGDP_here = dRGDP_mauro
                  )


highincomeoecd  <- c("Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Israel", "Japan", "Korea, Republic of", "Luxembourg", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States")

## Eastern European Countries 
easterneurope  <- c("Czech Republic", "Estonia", "Hungary", "Poland", "Slovak Republic", "Slovenia" )

superset  <- c("Australia", "Austria", "Belgium", "Canada", "Denmark", "Finland", "France", "Germany", "Greece", "Iceland", "Ireland", "Italy", "Japan", "Luxembourg", "Netherlands", "New Zealand", "Norway", "Portugal", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States")

debt.gg  <- ggplot(melt(summarize(group_by(subset(mutate(data_here,
                         haspubdebt = 1.0*(!is.na(debtgdp_here)),
                         g60 = 1.0*(debtgdp_here>60),
                         g90 = 1.0*(debtgdp_here>90)),
                         Country %in% superset & Year>=1880 & Year<2012,
                         select=c(Country,Year,haspubdebt,g60,g90)),Year),
          `Nonmissing` = sum(as.numeric(haspubdebt),na.rm=TRUE),
          `> 60%` = sum(as.numeric(g60),na.rm=TRUE),
          `> 90%` = sum(as.numeric(g90),na.rm=TRUE)),id.vars="Year",variable.name="Public Debt-to-GDP"),
          aes(x=Year,y=value,group=`Public Debt-to-GDP`,linetype=`Public Debt-to-GDP`)) + geom_line() +
    labs(y="Count of Countries") + theme(legend.position="bottom") + scale_x_continuous(breaks=seq(1880,2012,10))

## print(debt.gg)
          

gdp.gg  <- ggplot(melt(summarize(group_by(subset(data_here,
                                            Country %in% superset & Year>=1880 & Year<2012,
                                            select=c(Country,Year,dRGDP_here)),Year),
                            `First Quartile` = quantile(dRGDP_here,probs=0.25,na.rm=TRUE),
                            `Third Quartile` = quantile(dRGDP_here,probs=0.75,na.rm=TRUE)),
                  id.vars="Year",variable.name="GDP Growth Quartile"),
             aes(x=Year,y=value,group=`GDP Growth Quartile`,linetype=`GDP Growth Quartile`)) + geom_line() +
      labs(y="Growth in Real GDP Per Capita") + theme(legend.position="bottom") + scale_x_continuous(breaks=seq(1880,2012,10))

print(gdp.gg)

library(gridExtra)
grid.arrange(debt.gg,gdp.gg,ncol=1)


ggplot(filter(data_here, Country %in% superset & Year>=1946 & Year<=2009) , aes(x=debtgdp_here, y=dRGDP_here) ) + geom_point()

ggplot(filter(data_here, Country %in% superset & Year>=1955 & Year<=2009) , aes(x=debtgdp_here, y=dRGDP_here) ) + geom_point()

ggplot(filter(data_here, Country %in% superset & Year>=1955 & Year<=2009) , aes(x=debtgdp_here, y=dRGDP_here) ) + geom_point() + geom_smooth()

