pdf(file="nber_shading.pdf", paper="USr", height=0, width=0)

## Demonstrate
## downloading series from FRED
## ggplot for time series
## nber recession shading

library(tidyverse)
library(scales) ## for format control of dates on x axis
library(cowplot) ## For plot.grid
library(reshape2)
library(tis) ## for nberDates()
library(alfred)  ## get_fred_seriesf("...")
library(quantmod) ## Phase out   getSymbols("...", src="FRED") in favor of alfred 


(nber  <- mutate(data.frame(nberDates()),
       Start = as.Date(paste(floor(Start/10000),"-",floor(Start/100) %% 100,"-",Start %% 100,sep="")),
       End = as.Date(paste(floor(End/10000),"-",floor(End/100) %% 100,"-",End %% 100,sep=""))
       ))


nber.trim  <- subset(nber, Start > as.Date("1945-01-01"))
realgdppc  <- get_fred_series("a939rx0q048sbea")
bplot.realgdppc  <- ggplot(realgdppc ) + geom_line(aes(x=date,y=a939rx0q048sbea)) + labs(title="US Real Gross Domestic Product Per Capita", y="2012 dollars") +
    scale_x_date(date_breaks = "10 years", date_minor_breaks = "1 year", labels=date_format("%Y") ) +
    geom_rect(data=nber.trim,aes(xmin=Start,xmax=End,ymin=-Inf, ymax=+Inf),fill="pink", alpha=0.4)
print(bplot.realgdppc)


nber.trim  <- subset(nber, Start > as.Date("1945-01-01"))
unrate  <- get_fred_series("unrate")
ggplot(unrate ) + geom_line(aes(x=date,y=unrate)) +
    geom_rect(data=nber.trim,aes(xmin=Start,xmax=End,ymin=-Inf, ymax=+Inf),fill="pink", alpha=0.4)




nber.trim  <- subset(nber, Start > as.Date("1972-01-01"))
getSymbols("LNS14000006", src="FRED")
getSymbols("LNS14000003", src="FRED")

lns14000006 <- apply.quarterly(LNS14000006,mean)
lns14000003 <- apply.quarterly(LNS14000003,mean)

colnames(lns14000006) <- "black"
colnames(lns14000003) <- "white"

## Seasonally adjusted data
bw <- cbind(lns14000006, lns14000003)
bw.df <- data.frame(window(bw, start="1972-01-01",end=end(bw)))
bw.df <- mutate(bw.df, ratio = black / white, date=as.Date(rownames(bw.df)))

## Black and white unemployment rate on same scale
bplot.bw <- ggplot( filter(melt(bw.df,id.vars="date",variable.name="race"),race != "ratio" ) ) +
    geom_line(aes(x=date, y=value, group=race, linetype=race)) +
    xlab("Year") + ylab("Unemployment") +
    labs(title="Black and White Unemployment\nQuarterly Data SA") + coord_cartesian(ylim=c(0,20)) + theme(legend.position="top") +
    scale_x_date(date_breaks = "5 years", date_minor_breaks = "1 year", labels=date_format("%Y") ) + 
    geom_rect(data=nber.trim,aes(xmin=Start,xmax=End,ymin=-Inf, ymax=+Inf),fill="pink", alpha=0.4)

## Black and white unemployment rate ratio on separate scale
bplot.ratio <- ggplot( filter(melt(bw.df,id.vars="date",variable.name="race"),race == "ratio" )) +
    geom_line(aes(x=date, y=value, group=race)) +
    xlab("Year") + ylab("Ratio") + 
    labs(title="Black:White Unemployment Ratio")   + coord_cartesian(ylim=c(1.5,3)) +
    scale_x_date(date_breaks = "5 years", date_minor_breaks = "1 year", labels=date_format("%Y")) + 
    geom_rect(data=nber.trim,aes(xmin=Start,xmax=End,ymin=-Inf, ymax=+Inf),fill="pink", alpha=0.4)

plot_grid(bplot.bw, bplot.ratio, align = "v", nrow = 2, rel_heights = c(1.8, 1))

bplot.triptych <- ggplot(melt(bw.df,id.vars=c("date"), variable.name="race")) + 
    geom_line(aes(x=date, y=value, group=race)) +
    facet_wrap ( ~ race  , ncol=1, scales="free_y") + 
    xlab("Year") +
    scale_x_date(date_breaks = "5 years", date_minor_breaks = "1 year", labels=date_format("%Y")) +
    labs(title="Ratio of Black:White Unemployment\nQuarterly Data NSA") 
print(bplot.triptych + geom_rect(data=nber.trim, aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4))



