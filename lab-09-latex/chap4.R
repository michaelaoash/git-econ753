pdf(file="chap4.pdf",paper="USr",width=0,height=0)

## chap4.R
## R code for Berndt, Practice of Econometrics, Chapter 4 (Hedonic Pricing Models)
## michael ash ;
## 2009-2013



library(ggplot2)
library(dplyr)
library(reshape2)
library(grid)
library(gridExtra)

options(width=200)


## Last row is ^Z; so include nrows option
waugh <- read.csv("http://courses.umass.edu/econ753/berndt/chap4.dat/waugh",sep="",nrows=200)
cor(waugh)
summary(lm(PRICE ~ GREEN + NOSTALKS + DISPERSE,data=waugh))

## Chow Computer Price Index 
## Read the data from the website
## Last row is ^Z; so include nrows option
chow <- read.csv("http://courses.umass.edu/econ753/berndt/chap4.dat/chow",sep="",nrows=137)

## Create the dummy variables for years
D <- with(chow, model.matrix(~ 0 + factor(YEAR)))
colnames(D) <- gsub(pattern="factor\\(YEAR\\)" , replacement="year", colnames(D))
chow <- cbind(chow,D)

## Correlation matrices for first and second generation
chow <- mutate(chow,LNRENT = log(RENT), LNMULT=log(MULT), LNADD=log(ADD), LNACCESS=log(ACCESS), LNMEM=log(WORDS*BINARY*DIGITS))
as.dist(cor(subset(chow,YEAR<60,select=c(LNRENT,LNMULT,LNADD,LNACCESS,LNMEM))),upper=FALSE)
as.dist(cor(subset(chow,YEAR>=60,select=c(LNRENT,LNMULT,LNADD,LNACCESS,LNMEM))),upper=FALSE)

chow60 <- subset(chow,YEAR>=60)

## Replicates the regression in last line of Berndt, Table 4.1, p. 121
summary(chow60.lm <- lm( log(RENT) ~ log(MULT) + log(ACCESS) + log(WORDS*BINARY*DIGITS)
                      + year61 + year62 + year63 + year64 + year65, data=chow60))

## Same but with weighted least squares -- WRONG WEIGHTS!!
summary(chow60.lm.wls.wrong <- lm( log(RENT) ~ log(MULT) + log(ACCESS) + log(WORDS*BINARY*DIGITS)
                      + year61 + year62 + year63 + year64 + year65, weights=sqrt(VOLUME), data=chow60))

## Same but with weighted least squares (square root of VOLUME)
summary(chow60.lm.wls <- lm( log(RENT) ~ log(MULT) + log(ACCESS) + log(WORDS*BINARY*DIGITS)
                      + year61 + year62 + year63 + year64 + year65, weights=VOLUME, data=chow60))

cat(index <- exp(c(0,coef(chow60.lm)[5:9])),sep="\n")
cat(index.wt <- exp(c(0,coef(chow60.lm.wls)[5:9])),sep="\n")
plot(60:65,index,type="l",ann=FALSE,xaxt="n",las=1)
axis(side=1, at=60:65)
mtext(side=1, text="Year",line=3)
mtext(side=3, line=3, text="Price Indexes: quality-adjusted unit of computing")
mtext(side=3, line=2, text="Alternative Weights")
mtext(text="1960=1.000", at=c(1.1), side=2, adj=0.7, las=1)
points(60:65,index.wt,type="b")
text(63,0.85,"WLS")
text(63,0.5,"OLS")


n  <- ggplot(data=melt(data.frame(Year=60:65,ols=index,wls=index.wt),id="Year"), aes(x=Year,y=value,group=variable,shape=variable)) +
          geom_line(size=1.2) + geom_point(size=4) + theme(legend.position = "none") +
              ggtitle(expression(atop("Price Indexes: quality-adjusted unit of computing", italic("WLS vs. OLS")))) +
                  ylab("") +
                      annotate(geom="text", label="WLS", x=63,y=0.85) + annotate(geom="text", label="OLS", x=63,y=0.5)
print(n)
grid.text("Index (1960=1.00)", x=unit(0,"npc"), y=unit(0.95,"npc"), hjust=-0.1)


## Same as previous "by hand": intercept suppressed and sqrt(VOLUME)
## used to weight each variable (including constant).  I() syntax
## means treat "*" literally instead of interacting variables
summary(chow60.lm.wls.alt <- lm( I(sqrt(VOLUME)*log(RENT)) ~
                           0 +
                           I(sqrt(VOLUME)) + I(sqrt(VOLUME)*log(MULT)) +
                           I(sqrt(VOLUME)*log(ACCESS)) +
                           I(sqrt(VOLUME)*log(WORDS*BINARY*DIGITS)) +
                           I(sqrt(VOLUME)*year61) +
                           I(sqrt(VOLUME)*year62) +
                           I(sqrt(VOLUME)*year63) +
                           I(sqrt(VOLUME)*year64) +
                           I(sqrt(VOLUME)*year65),data=chow60))


names(chow60.lm.wls.alt$coefficients)  <- gsub("I\\(sqrt\\(VOLUME\\)[ *]+","", names(chow60.lm.wls.alt$coefficients))
names(chow60.lm.wls.alt$coefficients)  <- gsub("I\\(sqrt\\(VOLUME\\)\\)","(Intercept)))", names(chow60.lm.wls.alt$coefficients))
names(chow60.lm.wls.alt$coefficients)  <- gsub("\\)$","", names(chow60.lm.wls.alt$coefficients))
names(chow60.lm.wls.alt$coefficients)  <- gsub("\\)\\)",")", names(chow60.lm.wls.alt$coefficients))


library(stargazer)
stargazer(chow60.lm, chow60.lm.wls,chow60.lm.wls.alt, type="text")





## Run the full pooled hedonic model
summary(chow5465.lm <- lm( log(RENT) ~ log(MULT) + log(ACCESS) + log(WORDS*BINARY*DIGITS) + factor(YEAR), data=chow))
(index <- exp(c(0,coef(chow5465.lm)[5:15])))

## Run the hedonic for pairs of adjacent years to get the chain index
## Check the first two chain index values by hand
chow5455 <- subset(chow, YEAR==54 | YEAR==55)
summary(chow5455.lm <- lm( log(RENT) ~ log(MULT) + log(ACCESS) + log(WORDS*BINARY*DIGITS) + year55, data=chow5455))
chow5556 <- subset(chow, YEAR==55 | YEAR==56)
summary(chow5556.lm <- lm( log(RENT) ~ log(MULT) + log(ACCESS) + log(WORDS*BINARY*DIGITS) + year56, data=chow5556))

beta <- vector()
beta[1] = 0
for(year in 54:64) { 
    ## print(year)
    print(summary(assign(paste("chow",year,year+1,".lm",sep=""),
           lm( paste("log(RENT) ~ log(MULT) + log(ACCESS) + log(WORDS*BINARY*DIGITS) + year", year+1,sep="") ,
              data=subset(chow,YEAR==year | YEAR==year+1)))))

    beta[year-52] <- print(coef(get(paste("chow",year,year+1,".lm",sep="")))[5])
}

cat(index <- exp(c(0,coef(chow5465.lm)[5:15])),sep="\n")
cat(beta, sep='\n')
cat(cumsum(beta), sep='\n')
cat(index.chain <- exp(cumsum(beta)),sep="\n")

plot(54:65,index,type="l",ann=FALSE,xaxt="n",las=1)
axis(side=1, at=54:65)
mtext(side=1, text="Year",line=3)
mtext(side=3, line=3, text="Price Indexes: quality-adjusted unit of computing")
mtext(text="1954=1.000", at=c(1.1), side=2, adj=0.7, las=1)
points(54:65,index.chain,type="b")
text(58,0.5,"Chained")
text(58,0.7,"Traditional")

n  <- ggplot(data=melt(data.frame(Year=54:65,traditional=exp(c(0,coef(chow5465.lm)[5:15])),chained=exp(cumsum(beta))),id="Year"),
             aes(x=Year,y=value,group=variable,shape=variable)) +
          geom_line(size=1.2) + geom_point(size=4) + theme(legend.position = "none") +
              ggtitle(expression(atop("Price Indexes: quality-adjusted unit of computing", italic("Traditional vs. Chained")))) +
                  ylab("") + scale_x_continuous(breaks=54:65) +
                      annotate(geom="text", label="Traditional", x=58,y=0.7) + annotate(geom="text", label="Chained", x=58,y=0.5)
print(n)
grid.text("Index (1954=1.00)", x=unit(0,"npc"), y=unit(0.95,"npc"), hjust=-0.1)
