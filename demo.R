## Here is the path to the extra R libraries in Machmer W-13 only
.libPaths(c("/home/r-data/Rlibs",.libPaths()))

## Load the libraries that you will need.
library(foreign) ## to read Stata and other data formats
library(lmtest) ## For robust standard errors
library(sandwich) ## For robust standard errors
library(Hmisc) ## For some nicely formatted summary stats
library(pastecs) ## For some nicely formatted summary stats
library(zoo) ## For time series objects
library(dynlm) ## For dynamic linear model

## Set some options
## scipen controls scientific notation versus decimal places
## width controls the column width of output
options(scipen=1000,width=200)




## Some work with the Normalton Curfew Evaluation Berman, Chapter 5

curfew <- read.dta("http://courses.umass.edu/econ452-maash/data/curfew.dta")
summary(curfew)
curfew.ym <- as.yearmon(paste(substr(curfew$MONTH,1,3),curfew$YEAR), "%b %Y")
curfew <- subset(curfew,select=-MONTH)
curfew.z <- zoo(curfew,order.by=curfew.ym)

summary(dynlm(JUVARSTS ~ CURFEW,data=curfew.z))
summary(dynlm(JUVARSTS ~ CURFEW + season(JUVARSTS),data=curfew.z))


plot(curfew.z$CURFEW,curfew.z$JUVARSTS)
plot(subset(curfew.z,select=c(CURFEW,JUVARSTS)))



## Some work with the 1978 and 1985 CPS (Berndt, Chapter 5)
library(foreign)
cps <- read.dta("http://courses.umass.edu/econ753/berndt/stata/chap5-cps.dta")
tapply(exp(cps$lnwage),list(cps$year,cps$nonwh),mean)
tapply(exp(cps$lnwage),list(cps$year,cps$nonwh,cps$fe),mean)

barplot(tapply(exp(cps$lnwage),cps$fe,mean))

barplot(tapply(exp(cps$lnwage),cps$occup,mean))

barplot(table(cps$ind,cps$fe),legend=TRUE,beside=TRUE)

barplot(prop.table(table(cps$ind,cps$fe),2),legend=TRUE,beside=TRUE)




cps85 <- subset(cps,year==1985)

cps78 <- subset(cps,year==1978)


## Heteroskedasticity-consistent (robust) standard errors
cps85.lm <- lm(lnwage ~ fe + marr + nonwh + ed + ex + exsq, data=cps85)
summary(cps85.lm)
coeftest(cps85.lm,vcov=vcovHC(cps85.lm,type="HC1"))

## Pooled and unpooled regressions (with interaction terms)

cps.pooled.lm <- lm(lnwage ~ factor(fe) + marr + nonwh + ed + ex + exsq, data=cps85)
summary(cps.pooled.lm)
cps.separated.lm <- lm(lnwage ~ factor(fe)*( marr + nonwh + ed + ex + exsq ), data=cps85)
summary(cps.separated.lm)



## Oaxaca-Blinder decomposition
## Run the regression separately by race
cps.lm.race <- by(cps78,cps78$nonwh,
                   function(x) lm(lnwage ~ fe + marr + ed + ex + exsq, data=x))

## Create the constant term (for convenience)
cps78$one <- 1

## Compute the average attributes by race
(cps.mean.race <-
aggregate(subset(cps78,select=c(lnwage,one,fe,marr,ed,ex,exsq)),
          list(cps78$nonwh), mean))


## Extract average attributes by race
(attr.w <- as.matrix(cps.mean.race)[1,3:8])
(attr.b <- as.matrix(cps.mean.race)[2,3:8])

## Extract coefficients by race
(coef.w <- as.matrix(sapply(cps.lm.race,coef))[1:6,1])
(coef.b <- as.matrix(sapply(cps.lm.race,coef))[1:6,2])

## Difference in mean outcome
as.matrix(cps.mean.race[2,2]) - as.matrix(cps.mean.race[1,2])
attr.b %*% coef.b - attr.w %*% coef.w

## Difference attributable to attributes
(attr.b - attr.w) %*% coef.b
## Difference attributable to coefficients
(coef.b - coef.w) %*% attr.w

## As shares of total
(attr.b - attr.w) %*% coef.b / (attr.b %*% coef.b - attr.w %*% coef.w)
(coef.b - coef.w) %*% attr.w / (attr.b %*% coef.b - attr.w %*% coef.w)



## Some work with the Home Mortgage Disclosure Act Data (Stock and Watson, Chapter 11)


## Read HMDA data from website.
hmda <- read.dta("http://wps.aw.com/wps/media/objects/3254/3332253/datasets2e/datasets/hmda_aer.dta")

## Exclude applications for multifamily 
hmda <- subset(hmda,s51!=3)
## Exclude applications withdrawn or incomplete
hmda <- subset(hmda,s7!=4 & s7!=5)
## Only home-purchase loans (not home improvement, refinance, etc.)
hmda <- subset(hmda,s4==1)
## Code deny
hmda$deny.f <- factor(hmda$s7==3)
hmda$deny <- 1*(hmda$s7==3)

## Limit sample to whites and blacks
hmda <- subset(hmda,(s13==3)|(s13==5))
## Make race a "factor"
hmda$race <- relevel(factor(hmda$s13, label=c("black","nh.white")),ref="nh.white")

## Code some additional variables
## All of these are explained in hmda.pdf 
hmda$pi_rat <- hmda$s46/100
hmda$black <- 1*(hmda$s13==3)
hmda$hse_inc <- hmda$s45/100
hmda$loan_val <- hmda$s6/hmda$s50
hmda$ccred <- hmda$s43
hmda$mcred <- hmda$s42
hmda$pubrec <- 1*(hmda$s44>0)
hmda$denpmi <- 1*(hmda$s53==1)
hmda$selfemp <- 1*(hmda$s27a==1)
hmda$married <- 1*(hmda$s23a=="M")
hmda$single <- 1*(hmda$married==0)
hmda$hischl <- 1*(hmda$school>=12)
hmda$probunmp <- hmda$uria
hmda$condo <- 1*(hmda$s51 == 1)
hmda$ltv_med <- 1*(hmda$loan_val>=0.80)*(hmda$loan_val<=.95)
hmda$ltv_high <- 1*(hmda$loan_val>0.95)
hmda$blk_pi <- hmda$black*hmda$pi_rat
hmda$blk_hse <- hmda$black*hmda$hse_inc
hmda$ccred3 <- 1*(hmda$ccred==3) 
hmda$ccred4 <- 1*(hmda$ccred==4)
hmda$ccred5 <- 1*(hmda$ccred==5)
hmda$ccred6 <- 1*(hmda$ccred==6)
hmda$mcred3 <- 1*(hmda$mcred==3)
hmda$mcred4 <- 1*(hmda$mcred==4)


summary( subset(hmda,select=c(deny,black,pi_rat,hse_inc,ltv_med,
ltv_high,ccred,mcred,pubrec,denpmi,selfemp,single,hischl,probunmp,
mcred3,mcred4,ccred3,ccred4,ccred5,ccred6,condo)))

by(subset(hmda,select=c(deny,pi_rat,hse_inc,ltv_med,
ltv_high,ccred,mcred,pubrec,denpmi,selfemp,single,hischl,probunmp,
mcred3,mcred4,ccred3,ccred4,ccred5,ccred6,condo))  , hmda$black, summary)



describe( subset(hmda,select=c(deny,black,pi_rat,hse_inc,ltv_med,
ltv_high,ccred,mcred,pubrec,denpmi,selfemp,single,hischl,probunmp,
mcred3,mcred4,ccred3,ccred4,ccred5,ccred6,condo)))

by(subset(hmda,select=c(deny,pi_rat,hse_inc,ltv_med,
ltv_high,ccred,mcred,pubrec,denpmi,selfemp,single,hischl,probunmp,
mcred3,mcred4,ccred3,ccred4,ccred5,ccred6,condo))  , hmda$black, describe)

stat.desc( subset(hmda,select=c(deny,black,pi_rat,hse_inc,ltv_med,
ltv_high,ccred,mcred,pubrec,denpmi,selfemp,single,hischl,probunmp,
mcred3,mcred4,ccred3,ccred4,ccred5,ccred6,condo)))

by(subset(hmda,select=c(deny,pi_rat,hse_inc,ltv_med,
ltv_high,ccred,mcred,pubrec,denpmi,selfemp,single,hischl,probunmp,
mcred3,mcred4,ccred3,ccred4,ccred5,ccred6,condo))  , hmda$black, stat.desc)


## Contingency table
table(hmda$race,hmda$deny.f)

## What is the difference between row and column percents?
prop.table(table(hmda$race,hmda$deny.f),1)
prop.table(table(hmda$race,hmda$deny.f),2)


## Use a t-test to check if the denial rates are the same for
## white and black
t.test(hmda$deny ~ hmda$race )

## Same test with LPM regression
hmda.1 <- lm(deny ~ race,data=hmda)
coeftest(hmda.1,vcov=vcovHC(hmda.1,type="HC1"))

hmda.2 <- lm(deny ~ pi_rat,data=hmda)
coeftest(hmda.2,vcov=vcovHC(hmda.2,type="HC1"))

hmda.3 <- lm(deny ~ race + pi_rat,data=hmda)
coeftest(hmda.3,vcov=vcovHC(hmda.3,type="HC1"))

hmda.4 <- lm(deny ~ race + pi_rat + hse_inc + ltv_med + ltv_high
             + ccred + mcred + pubrec + denpmi + selfemp, data=hmda)
coeftest(hmda.4,vcov=vcovHC(hmda.4,type="HC1"))


hmda.p <- glm(deny ~ race + pi_rat + hse_inc + ltv_med + ltv_high
             + ccred + mcred + pubrec + denpmi + selfemp, data=hmda, family=binomial(link=probit))
summary(hmda.p)
coeftest(hmda.p,vcov=vcovHC(hmda.p,type="HC1"))



## Oaxaca-Blinder decomposition

## Run the regression separately by race
hmda.lm.race <- by(hmda,hmda$black,
                   function(x) lm(deny ~ pi_rat + hse_inc + ltv_med +
                   ltv_high + ccred + mcred + pubrec + denpmi +
                   selfemp, data=x))

## Create the constant term (for convenience)
hmda$one <- 1

## Compute the average attributes by race
(hmda.mean.race <-
aggregate(subset(hmda,select=c(deny,one,pi_rat,hse_inc,ltv_med,ltv_high,ccred,mcred,pubrec,denpmi,selfemp)),
          list(hmda$black), mean))

## Extract average attributes by race
(attr.w <- as.matrix(hmda.mean.race)[1,3:12])
(attr.b <- as.matrix(hmda.mean.race)[2,3:12])

## Extract coefficients by race
(coef.w <- as.matrix(sapply(hmda.lm.race,coef))[1:10,1])
(coef.b <- as.matrix(sapply(hmda.lm.race,coef))[1:10,2])

## Difference in mean outcome
as.matrix(hmda.mean.race[2,2]) - as.matrix(hmda.mean.race[1,2])
attr.b %*% coef.b - attr.w %*% coef.w

## Difference attributable to attributes
(attr.b - attr.w) %*% coef.b
## Difference attributable to coefficients
(coef.b - coef.w) %*% attr.w

## As shares of total
(attr.b - attr.w) %*% coef.b / (attr.b %*% coef.b - attr.w %*% coef.w)
(coef.b - coef.w) %*% attr.w / (attr.b %*% coef.b - attr.w %*% coef.w)
