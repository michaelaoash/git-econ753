## Load the needed libraries
## Do these need (one-time) installation?
library(tidyverse) ## Includes advanced data management and graphics
library(haven) ## to read Stata and other data formats
library(lmtest) ## For robust standard errors
library(sandwich) ## For robust standard errors
library(Hmisc) ## For some nicely formatted summary stats

## Set some options scipen controls scientific notation versus
## decimals (higher number = favor decimals over scientific notation)
## width controls the column width of output
options(scipen=1000,width=200)

## Some work with the 1978 and 1985 CPS (Berndt, Chapter 5)
cps <- read_dta("http://courses.umass.edu/econ753/berndt/stata/chap5-cps.dta")

## list variables
names(cps)

## Basic summary statistics
summary(cps)
describe(cps)

cps$ed
mean(cps$ed)
sd(cps$ed)

stem(cps$ed)
stem(cps$lnwage)

stem(exp(cps$lnwage))
mean(exp(cps$lnwage))
sd(exp(cps$lnwage))


## Some tabulation commands
## and "with"

with(cps, table(as_factor(occupation),fe))
xtabs(~ as_factor(occupation) + fe, data=cps)
summary(xtabs(~ as_factor(occupation) + fe, data=cps))

xtabs(~ as_factor(occupation) + fe + year, data=cps)

xtabs(~ as_factor(occupation) + fe + year, data=cps)

prop.table(xtabs(~ as_factor(occupation) + fe, data=cps))
prop.table(xtabs(~ as_factor(occupation) + fe, data=cps), margin=2)
prop.table(xtabs(~ as_factor(occupation) + fe, data=cps), margin=1)

with(cps,tapply(lnwage,list(year,as_factor(ethnic)),mean))

with(cps,tapply(exp(lnwage),list(year,as_factor(ethnic)),mean))




cps <- mutate(cps,
              wage = exp(lnwage),
              college = (ed >= 16)
              )


ggplot(cps, aes(x=factor(fe), y=lnwage)) + geom_bar(stat="summary", fun.y="mean")
ggplot(cps, aes(x=factor(fe), y=exp(lnwage), fill=factor(fe))) + geom_bar(stat="summary", fun.y="mean")

ggplot(cps, aes(x=ed, y=exp(lnwage), color=fe)) + geom_point() + geom_smooth(method="lm")


## Create datasets limited to 1985 and 1978
cps85 <- filter(cps,year==1985)
cps78 <- filter(cps,year==1978)

## List the objects in R (should be three)
ls()




cps85.lm <- lm(lnwage ~ fe + marr + nonwh + ed + ex + exsq, data=cps85)
## Basic regression results
summary(cps85.lm)
## Heteroskedasticity-consistent (robust) standard errors (equivalent to ", robust" in Stata)
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
cps.mean.race <- summarize_all(group_by(cps78,nonwh),mean)
cps.mean.race <- select(cps.mean.race, lnwage,one,fe,marr,ed,ex,exsq)


## Extract average attributes by race
(attr.w <- as.matrix(cps.mean.race)[1,2:7])
(attr.b <- as.matrix(cps.mean.race)[2,2:7])

## Extract coefficients by race
(coef.w <- as.matrix(sapply(cps.lm.race,coef))[1:6,1])
(coef.b <- as.matrix(sapply(cps.lm.race,coef))[1:6,2])

## Difference in mean outcome
as.matrix(cps.mean.race[2,1]) - as.matrix(cps.mean.race[1,1])
attr.b %*% coef.b - attr.w %*% coef.w

## Difference attributable to attributes
(attr.b - attr.w) %*% coef.b
## Difference attributable to coefficients
(coef.b - coef.w) %*% attr.w

## As shares of total
(attr.b - attr.w) %*% coef.b / (attr.b %*% coef.b - attr.w %*% coef.w)
(coef.b - coef.w) %*% attr.w / (attr.b %*% coef.b - attr.w %*% coef.w)

