## Some work with the Home Mortgage Disclosure Act Data (Stock and Watson, Chapter 11)

## Read HMDA data from Stata file.
hmda <- read_dta("hmda_sw.dta")

## Exclude applications for multifamily 
hmda <- filter(hmda,s51!=3)
## Exclude applications withdrawn or incomplete
hmda <- filter(hmda,s7!=4 & s7!=5)
## Only home-purchase loans (not home improvement, refinance, etc.)
hmda <- filter(hmda,s4==1)
## Code deny
hmda$deny.f <- factor(hmda$s7==3)
hmda$deny <- 1*(hmda$s7==3)

## Limit sample to whites and blacks
hmda <- filter(hmda,(s13==3)|(s13==5))
## Make race a "factor"
hmda$race <- relevel(factor(hmda$s13, label=c("black","nh.white")),ref="nh.white")

## Code some additional variables
## All of these are explained in hmda.pdf
hmda  <- mutate(hmda, 
                pi_rat   = s46/100,
                black    = 1*(s13==3),
                hse_inc  = s45/100,
                loan_val = s6/s50,
                ccred    = s43,
                mcred    = s42,
                pubrec   = 1*(s44>0),
                denpmi   = 1*(s53==1),
                selfemp  = 1*(s27a==1),
                married  = 1*(s23a=="M"),
                single   = 1*(married==0),
                hischl   = 1*(school>=12),
                probunmp = uria,
                condo    = 1*(s51 == 1),
                ltv_med  = 1*(loan_val>=0.80)*(loan_val<=.95),
                ltv_high = 1*(loan_val>0.95),
                blk_pi   = black*pi_rat,
                blk_hse  = black*hse_inc,
                ccred3   = 1*(ccred==3) ,
                ccred4   = 1*(ccred==4),
                ccred5   = 1*(ccred==5),
                ccred6   = 1*(ccred==6),
                mcred3   = 1*(mcred==3),
                mcred4   = 1*(mcred==4)
                )

summary( select(hmda,deny,black,pi_rat,hse_inc,ltv_med, ltv_high,ccred,mcred,pubrec,denpmi,selfemp,single,hischl,probunmp, mcred3,mcred4,ccred3,ccred4,ccred5,ccred6,condo))



by(subset(hmda,select=c(deny,pi_rat,hse_inc,ltv_med,ltv_high,ccred,mcred,pubrec,denpmi,selfemp,single,hischl,probunmp,mcred3,mcred4,ccred3,ccred4,ccred5,ccred6,condo))  , hmda$black, summary)


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
