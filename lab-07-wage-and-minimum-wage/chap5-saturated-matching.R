library(foreign) ## To read Stata datasets
library(car) ## for testing linear combinations with linearHypothesis()
library(lmtest) ## For robust standard errors
library(sandwich) ## For robust standard errors
library(tidyverse)
library(reshape2)

options(scipen=10000)
options(width=200)

chap5.cps <- read.dta("http://courses.umass.edu/econ753/berndt/stata/chap5-cps.dta")
chap5.cps$year.f <- factor(chap5.cps$year)

## Matching with full saturation
## See also full-saturation-matching.R
cps85 <- subset(chap5.cps,year==1985)
cps85 <- chap5.cps %>% filter(year==1985)

## This creates the demographic groups by education, experience, nonwhite, and hispanicity
## The formatC business adds leading zeros to the single-digit values so they sort properly
cps85 <- mutate(cps85,
                dem = paste(formatC(ed, width = 2, format = "d", flag = "0"),
                             formatC(ex, width = 2, format = "d", flag = "0"),
                             nonwh,hisp))

cps85 <- mutate(cps85,
                dem = paste(formatC(ed, width = 2, format = "d", flag = "0"),
                            formatC(ex, width = 2, format = "d", flag = "0"))
                )



## Here is a straightforward way to get the needed elements
dem.fe.lnwage <- with(cps85, tapply(lnwage,list(dem,fe),mean))
dem.fe.counts <- with(cps85, tapply(lnwage,list(dem,fe),length))

## Improved with tidyverse
## This is equivalent to the Stata collapse command
cps85.sum <- cps85 %>% group_by(dem, fe) %>% summarize(lnwage=mean(lnwage), N = n())


## Note that the demographic grouping could all be handled here
## Aside: try this for occpation by year
chap5.cps %>% group_by(occupation, year) %>% summarize(lnwage=mean(lnwage), ed = mean(ed), N = n())

## This reshapes the data from long dem,fe to wide dem x fe
## http://www.jstatsoft.org/v21/i12/paper
## Old melt/cast formulation
cps85.melt <- melt(cps85.sum, id=c("dem","fe"), measured=c("lnwage","N")  )
cps85.cast <- dcast(cps85.melt, dem ~ fe + variable)

## New pivot_wider formulation
cps85.diff <- cps85.sum %>%
    pivot_wider(id_cols = "dem", names_from = fe, values_from = c(lnwage, N)) %>%
    mutate(
        diff_lnwage = lnwage_1 - lnwage_0
    )

## This differences mean wage by sex


## Mean difference weighted by number of women in the demographic cell
with(cps85.diff, weighted.mean(diff_lnwage, `1_N`,na.rm=TRUE))

## To get the difference the old-fashioned way, need to be careful about the denominator
## I.e., only include cells that have a difference.
with(cps85.diff, sum(  diff_lnwage * `1_N` , na.rm=TRUE ) / sum( ifelse(is.na(diff_lnwage),NA,1)*`1_N`,na.rm=TRUE))

## Or delete the rows with empty difference (i.e., no men or no women)
cps85.diff2 <- subset(cps85.diff, !is.na(diff_lnwage))
with(cps85.diff2, sum(  diff_lnwage * `1_N`  ) / sum( `1_N`))

## Alternative weightings
with(cps85.diff2, sum(  diff_lnwage * `0_N`  ) / sum( `0_N`))
with(cps85.diff2, sum(  diff_lnwage * (`0_N`+`1_N`  ) / sum( `0_N` + `1_N` )))




## Assignment: repeat for 1978
cps78 <- subset(chap5.cps,year==1978)








## Different LM structure for 1978 and 1985

summary(interacted.year.lm <- lm(lnwage ~ year.f*(1 + ed + ex + exsq + union + fe + hisp + nonwh), data=chap5.cps))

summary(pooled.year.lm <- lm(lnwage ~ (year.f + ed + ex + exsq + union + fe + hisp + nonwh), data=chap5.cps))

summary(separated.78.lm <- lm(lnwage ~ (ed + ex + exsq + union + fe + hisp + nonwh), data=subset(chap5.cps,year==1978)))

summary(separated.85.lm <- lm(lnwage ~ (ed + ex + exsq + union + fe + hisp + nonwh), data=subset(chap5.cps,year==1985)))






F.chow.year <-   ( ( sum(pooled.year.lm$residuals^2) - (sum(separated.78.lm$residuals^2) + sum(separated.85.lm$residuals^2)) ) / 7 ) / 
                  ( ( sum(separated.78.lm$residuals^2) + sum(separated.85.lm$residuals^2) ) /
                   (separated.85.lm$df.residual + separated.78.lm$df.residual ) )
F.chow.year

linearHypothesis(interacted.year.lm,c("year.f1985:ed","year.f1985:ex","year.f1985:exsq","year.f1985:union","year.f1985:fe","year.f1985:hisp","year.f1985:nonwh" ))

(coef <- names(coefficients(interacted.year.lm)))
grep(":", coef,value=TRUE)
linearHypothesis(interacted.year.lm,grep(":", coef,value=TRUE))




## Different LM structure for men and women in 1978

chap5.cps <- read.dta("http://courses.umass.edu/econ753/berndt/stata/chap5-cps.dta ")

summary(interacted.sex.lm <- lm(lnwage ~ fe*(1 + ed + ex + exsq + union + hisp + nonwh), data=subset(chap5.cps,year==1978)))
summary(pooled.sex.lm <- lm(lnwage ~ (ed + ex + exsq + union + hisp + nonwh), data=subset(chap5.cps,year==1978)))
summary(separated.f.lm <- lm(lnwage ~ (ed + ex + exsq + union + hisp + nonwh), data=subset(chap5.cps,year==1978 & fe==1)))
summary(separated.m.lm <- lm(lnwage ~ (ed + ex + exsq + union + hisp + nonwh), data=subset(chap5.cps,year==1978 & fe==0)))

## Chow test for complete equality (guess the answer!) with linearHypothesis or F-stat "by hand"
linearHypothesis(interacted.sex.lm,c("fe","fe:ed","fe:ex","fe:exsq","fe:union","fe:hisp","fe:nonwh" ))
(F.chow.sex.numerator <- ( sum(pooled.sex.lm$residuals^2) - ( sum(separated.f.lm$residuals^2) + sum(separated.m.lm$residuals^2) ) )  / pooled.sex.lm$rank )
(F.chow.sex.denominator <-  ( sum(separated.f.lm$residuals^2) + sum(separated.m.lm$residuals^2) ) / (separated.m.lm$df.residual + separated.f.lm$df.residual))
(F.chow.sex <- F.chow.sex.numerator / F.chow.sex.denominator)


## Chow test for differential-returns discrimination (versus simple-penalty discrimination)

chap5.cps <- read.dta("http://courses.umass.edu/econ753/berndt/stata/chap5-cps.dta")

summary(indicator.sex.lm <- lm(lnwage ~ (fe + ed + ex + exsq + union + hisp + nonwh), data=subset(chap5.cps,year==1978)))
summary(interacted.sex.lm <- lm(lnwage ~ fe*(1 + ed + ex + exsq + union + hisp + nonwh), data=subset(chap5.cps,year==1978)))

## Run linear hypothesis beta=0 for female interaction variables named explicitly
linearHypothesis(interacted.sex.lm,c("fe:ed","fe:ex","fe:exsq","fe:union","fe:hisp","fe:nonwh" ))
## Run linear hypothesis beta=0 for all coefficients containing "fe:" (female interaction variables)
linearHypothesis(interacted.sex.lm,grep("fe:", names(coef(interacted.sex.lm)),value=TRUE) )  

## Compute Chow test F-stat with standard formula
(F.chow.sex.numerator <-  (sum(indicator.sex.lm$residuals^2) -  sum(interacted.sex.lm$residuals^2)) /  (interacted.sex.lm$rank - indicator.sex.lm$rank) )
(F.chow.sex.denominator <-  sum(interacted.sex.lm$residuals^2)  / interacted.sex.lm$df.residual )
(F.chow.sex <- F.chow.sex.numerator / F.chow.sex.denominator)
1-pf(F.chow.sex, interacted.sex.lm$rank - indicator.sex.lm$rank , interacted.sex.lm$df.residual)


## Different LM structure for union and nonunion
## Experience grid
x <- 0:40

## Different LM structure for union and nonunion in 1978
summary(interacted.union.lm <- lm(lnwage ~ union*(1 + ed + ex + exsq + fe + hisp + nonwh), data=subset(chap5.cps,year==1978)))
summary(pooled.union.lm <- lm(lnwage ~ (ed + ex + exsq + fe + hisp + nonwh), data=subset(chap5.cps,year==1978)))
summary(separated.u.lm <- lm(lnwage ~ (ed + ex + exsq + fe + hisp + nonwh), data=subset(chap5.cps,year==1978 & union==1)))
summary(separated.n.lm <- lm(lnwage ~ (ed + ex + exsq + fe + hisp + nonwh), data=subset(chap5.cps,year==1978 & union==0)))

A <- pooled.union.lm$coefficients[c("(Intercept)", "ed", "fe", "hisp", "nonwh")]  %*%
    with(subset(chap5.cps,year==1978), c(1, mean(ed), mean(fe), mean(hisp), mean(nonwh)  ))
age.earnings.union <- as.vector(A) + separated.u.lm$coefficients[c("ex")] * x + separated.u.lm$coefficients[c("exsq")] * x^2
age.earnings.nonunion <- as.vector(A) + separated.n.lm$coefficients[c("ex")] * x + separated.n.lm$coefficients[c("exsq")] * x^2
plot(x,age.earnings.union)
lines(x,age.earnings.union)
lines(x,age.earnings.nonunion)

(F.chow.union.numerator <- ( sum(pooled.union.lm$residuals^2) - ( sum(separated.u.lm$residuals^2) + sum(separated.n.lm$residuals^2) ) )  / pooled.union.lm$rank)
(F.chow.union.denominator <-  ( sum(separated.u.lm$residuals^2) + sum(separated.n.lm$residuals^2) ) / (separated.n.lm$df.residual + separated.u.lm$df.residual))
(F.chow.union <- F.chow.union.numerator / F.chow.union.denominator)



## Different LM structure for union and nonunion in 1985
summary(interacted.union.lm <- lm(lnwage ~ union*(1 + ed + ex + exsq + fe + hisp + nonwh), data=subset(chap5.cps,year==1985)))
summary(pooled.union.lm <- lm(lnwage ~ (ed + ex + exsq + fe + hisp + nonwh), data=subset(chap5.cps,year==1985)))
summary(separated.u.lm <- lm(lnwage ~ (ed + ex + exsq + fe + hisp + nonwh), data=subset(chap5.cps,year==1985 & union==1)))
summary(separated.n.lm <- lm(lnwage ~ (ed + ex + exsq + fe + hisp + nonwh), data=subset(chap5.cps,year==1985 & union==0)))

A <- pooled.union.lm$coefficients[c("(Intercept)", "ed", "fe", "hisp", "nonwh")]  %*%
    with(subset(chap5.cps,year==1985), c(1, mean(ed), mean(fe), mean(hisp), mean(nonwh)  ))
age.earnings.union <- as.vector(A) + separated.u.lm$coefficients[c("ex")] * x + separated.u.lm$coefficients[c("exsq")] * x^2
age.earnings.nonunion <- as.vector(A) + separated.n.lm$coefficients[c("ex")] * x + separated.n.lm$coefficients[c("exsq")] * x^2
plot(x,age.earnings.union)
lines(x,age.earnings.union)
lines(x,age.earnings.nonunion)

(F.chow.union.numerator <- ( sum(pooled.union.lm$residuals^2) - ( sum(separated.u.lm$residuals^2) + sum(separated.n.lm$residuals^2) ) )  / pooled.union.lm$rank)
(F.chow.union.denominator <-  ( sum(separated.u.lm$residuals^2) + sum(separated.n.lm$residuals^2) ) / (separated.n.lm$df.residual + separated.u.lm$df.residual))
(F.chow.union <- F.chow.union.numerator / F.chow.union.denominator)



cps <- read.dta("http://courses.umass.edu/econ753/berndt/stata/chap5-cps.dta")
tapply(exp(cps$lnwage),list(cps$year,cps$nonwh),mean)
tapply(exp(cps$lnwage),list(cps$year,cps$nonwh,cps$fe),mean)

barplot(tapply(exp(cps$lnwage),cps$fe,mean))

barplot(tapply(exp(cps$lnwage),cps$occup,mean))

barplot(table(cps$ind,cps$fe),legend=TRUE,beside=TRUE)

barplot(prop.table(table(cps$ind,cps$fe),2),legend=TRUE,beside=TRUE)




cps85 <- subset(cps,year==1985)

cps78 <- subset(cps,year==1978)

save.image(file="chap5-cps.Rdata")

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

## Decomposition 1
## Difference attributable to attributes scored by black coefficients
(attr.b - attr.w) %*% coef.b
## Difference attributable to coefficients scored at white attributes
(coef.b - coef.w) %*% attr.w
## As shares of total
(attr.b - attr.w) %*% coef.b / (attr.b %*% coef.b - attr.w %*% coef.w)
(coef.b - coef.w) %*% attr.w / (attr.b %*% coef.b - attr.w %*% coef.w)

## Decomposition 2
## Difference attributable to attributes scored by white coefficients
(attr.b - attr.w) %*% coef.w
## Difference attributable to coefficients scored at black attributes
(coef.b - coef.w) %*% attr.b
## As shares of total
(attr.b - attr.w) %*% coef.w / (attr.b %*% coef.b - attr.w %*% coef.w)
(coef.b - coef.w) %*% attr.b / (attr.b %*% coef.b - attr.w %*% coef.w)




## Oaxaca-Blinder decomposition
## Run the regression separately by sex
cps.lm.sex <- by(cps85,cps85$fe,
                   function(x) lm(lnwage ~ nonwh + marr + ed + ex + exsq, data=x))
## Create the constant term (for convenience)
cps85$one <- 1
## Compute the average attributes by sex
(cps.mean.sex <-
aggregate(subset(cps85,select=c(lnwage,one,nonwh,marr,ed,ex,exsq)),
          list(cps85$fe), mean))
## Extract average attributes by sex
(attr.m <- as.matrix(cps.mean.sex)[1,3:8])
(attr.f <- as.matrix(cps.mean.sex)[2,3:8])
## Extract coefficients by sex
(coef.m <- as.matrix(sapply(cps.lm.sex,coef))[1:6,1])
(coef.f <- as.matrix(sapply(cps.lm.sex,coef))[1:6,2])
## Difference in mean outcome
as.matrix(cps.mean.sex[2,2]) - as.matrix(cps.mean.sex[1,2])
attr.f %*% coef.f - attr.m %*% coef.m

## Decomposition 1
## Difference attributable to attributes scored by female coefficients
(attr.f - attr.m) %*% coef.f
## Difference attributable to coefficients scored at male attributes
(coef.f - coef.m) %*% attr.m
## As shares of total
(attr.f - attr.m) %*% coef.f / (attr.f %*% coef.f - attr.m %*% coef.m)
(coef.f - coef.m) %*% attr.m / (attr.f %*% coef.f - attr.m %*% coef.m)

## Decomposition 2
## Difference attributable to attributes scored by male coefficients
(attr.f - attr.m) %*% coef.m
## Difference attributable to coefficients scored at female attributes
(coef.f - coef.m) %*% attr.f
## As shares of total
(attr.f - attr.m) %*% coef.m / (attr.f %*% coef.f - attr.m %*% coef.m)
(coef.f - coef.m) %*% attr.f / (attr.f %*% coef.f - attr.m %*% coef.m)


