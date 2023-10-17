## Load the needed libraries
## Do these need (one-time) installation?
library(tidyverse) ## Includes advanced data management and graphics
library(haven) ## to read Stata and other data formats
library(labelled)
library(broom) ## Processing output
library(janitor) ## Advanced tabling commands
library(lmtest) ## For robust standard errors
library(sandwich) ## For robust standard errors
library(Hmisc) ## For some nicely formatted summary stats

## Set some options scipen controls scientific notation versus
## decimals (higher number = favor decimals over scientific notation)
## width controls the column width of output
options(scipen=1000,width=200)



## Get CPI for 1978, 1985, and 2022
fredr::fredr_set_key("2c43987fff98daa62e436c00e39f99ff")
cpi1978  <- as.numeric(fredr::fredr_series_observations("CPIAUCNS", observation_start=as.Date("1978-01-01"),
             observation_end=as.Date("1978-12-31"),
             frequency="a",aggregation_method="avg")[,"value"])
cpi1985  <- as.numeric(fredr::fredr_series_observations("CPIAUCNS", observation_start=as.Date("1985-01-01"),
             observation_end=as.Date("1985-12-31"),
             frequency="a",aggregation_method="avg")[,"value"])
cpi2022  <- as.numeric(fredr::fredr_series_observations("CPIAUCNS", observation_start=as.Date("2022-01-01"),
             observation_end=as.Date("2022-12-31"),
             frequency="a",aggregation_method="avg")[,"value"])
cpi <- tibble( year=c(1978,1985),  cpi=c(cpi1978,cpi1985)   )



## Some work with the 1978 and 1985 CPS (Berndt, Chapter 5)
cps <- read_stata("http://courses.umass.edu/econ753/berndt/stata/chap5-cps.dta")

## list variables
names(cps)


## Basic summary statistics
summary(cps)

cps$ed
mean(cps$ed)
sd(cps$ed)

table(cps$ed)
cps %>% tabyl(ed)
stem(cps$ed)

stem(cps$lnwage)

## Tukey stem-and-leaf plot
stem(cps$lnwage, scale=2, width=125)

stem(exp(cps$lnwage), scale=2, width=125)
mean(exp(cps$lnwage))
sd(exp(cps$lnwage))

dplyr::summarize(group_by(cps,year), mean(lnwage), sd(lnwage), mean(exp(lnwage)), sd(exp(lnwage)), exp(mean(lnwage))  )


## Some tabulation commands
## table uses "with" to specify the dataset and xtab uses use data= to specify the dataset
with(cps, table(as_factor(occupation),fe))

xtabs(~ occupation + fe, data=cps)
xtabs(~ as_factor(occupation) + fe, data=cps)


## Occupation comes in from Stata as labelled but the label is not
## used by default
cps %>% tabyl(occupation,fe) %>% adorn_title()

cps %>% as_factor() %>% tabyl(occupation,fe) %>% adorn_title()

cps %>% as_factor() %>% tabyl(occupation,industry) %>% adorn_title()

cps %>% as_factor() %>% tabyl(occupation,industry, fe) %>% adorn_title()

cps %>% as_factor() %>% tabyl(occupation,industry, fe) %>% adorn_title("combined")


summary(xtabs(~ as_factor(occupation) + fe, data=cps))

xtabs(~ as_factor(occupation) + fe + year, data=cps)

prop.table(xtabs(~ as_factor(occupation) + fe, data=cps))
prop.table(xtabs(~ as_factor(occupation) + fe, data=cps), margin=2)
prop.table(xtabs(~ as_factor(occupation) + fe, data=cps), margin=1)

with(cps,tapply(lnwage,list(year,as_factor(ethnic)),mean))

with(cps,tapply(exp(lnwage),list(year,as_factor(ethnic)),mean))



cps <- left_join(cps, cpi)


## The "mutate" function (in the tidyverse package) takes a dataset,
## adds new variables, in this case "wage" and "college", and returns
## a new dataset, in this case written on top of the existing dataset
## with "->"
cps <- mutate(cps,
              wage = exp(lnwage),
              rwage2022 = wage * cpi2022 / cpi,
              rwage1985 = wage * cpi1985 / cpi,
              lnrwage2022 = log(rwage2022),
              lnrwage1985 = log(rwage1985),
              college = (ed >= 16)
              )

dplyr::summarize(group_by(cps,year), mean(lnwage), sd(lnwage), mean(exp(lnwage)), sd(exp(lnwage)), exp(mean(lnwage))  )
dplyr::summarize(group_by(cps,year), mean(lnrwage2022), sd(lnrwage2022), mean(exp(lnrwage2022)), sd(exp(lnrwage2022)), exp(mean(lnrwage2022))  )

dplyr::summarize(group_by(cps,year), mean(lnrwage1985), sd(lnrwage1985), mean(exp(lnrwage1985)), sd(exp(lnrwage1985)), exp(mean(lnrwage1985))  )




## Introducing ggplot(), a sophisticated graphics library included in tidyverse
ggplot(cps, aes(x=factor(fe), y=lnwage)) + geom_bar(stat="summary", fun="mean")

ggplot(cps %>% as_factor(), aes(x=occupation, y=lnwage)) + geom_bar(stat="summary", fun="mean")

ggplot(cps %>% as_factor(), aes(x=occupation, y=rwage2022)) + geom_bar(stat="summary", fun="mean")

ggplot(cps %>% as_factor(), aes(x=reorder(occupation, desc(rwage2022)), y=rwage2022)) +
    geom_bar(stat="summary", fun="mean") 

ggplot(cps %>% as_factor(), aes(x=reorder(occupation, desc(rwage2022)), y=rwage2022)) +
    geom_bar(stat="summary", fun="mean") +
    facet_grid(rows=vars(year))

ggplot(cps %>% as_factor(), aes(x=reorder(occupation, desc(rwage2022)), y=rwage2022, fill=factor(year))) +
    geom_bar(stat="summary", fun="mean", position="dodge") 


ggplot(cps %>% as_factor(), aes(x=reorder(occupation, desc(rwage2022)), y=rwage2022, fill=factor(year))) +
    geom_bar(stat="summary", fun="mean", position="dodge", width=0.5) 

ggplot(cps %>% as_factor(), aes(x=college, y=rwage2022, fill=factor(year))) +
    geom_bar(stat="summary", fun="mean", position="dodge", width=0.5) 




ggplot(cps, aes(x=factor(fe), y=exp(lnwage), fill=factor(fe))) + geom_bar(stat="summary", fun="mean")



## Note that %>% is the "pipe" symbol in tidyverse.  It feeds the
## dataset (on the left) into the next function on the right Many but
## not all functions are equipped to receive "piped" dataset

cps %>% ggplot(aes(x=ed, y=exp(lnwage))) + geom_point()

cps %>% ggplot(aes(x=ed, y=exp(lnwage), color=factor(fe))) + geom_point()

cps %>% ggplot(aes(x=ed, y=exp(lnwage), color=factor(fe))) + geom_point() + geom_smooth(method="lm")

cps %>% ggplot(aes(x=ed, y=exp(lnwage), color=factor(fe), size=ex)) + geom_point()


## Create datasets limited to 1985 and 1978 Note that this uses the
## filter function in the tidyverse package.  This will NOT work if
## you have not loaded tidyverse.
cps85 <- filter(cps,year==1985)
cps78 <- filter(cps,year==1978)

## List the objects in R (there should be three)
ls()


## Basic OLS regrssion with the lm() (linear model) function
help.search("regression")
help.search("regression", package="stats")
help.search("regression", package=c("stats","lmtest"))
help(lm)


cps85.lm <- lm(lnwage ~ fe + marr + nonwh + ed + ex + exsq, data=cps85)
## Basic regression results
summary(cps85.lm)
coeftest(cps85.lm)
## Heteroskedasticity-consistent (robust) standard errors (identical to ", robust" in Stata)
coeftest(cps85.lm, vcov=vcovHC(cps85.lm,type="HC1"))


## Pooled and unpooled regressions (with interaction terms)
cps.pooled.lm <- lm(lnwage ~ fe + marr + nonwh + ed + ex + exsq, data=cps85)
summary(cps.pooled.lm)
cps.separated.lm <- lm(lnwage ~ fe*( marr + nonwh + ed + ex + exsq ), data=cps85)
summary(cps.separated.lm)

cps.fem.lm <- lm(lnwage ~ marr + nonwh + ed + ex + exsq, data=filter(cps85,fe==1))


## Oaxaca-Blinder decomposition
## Run the regression separately by race
cps.lm.race <- by(cps78,cps78$nonwh,
                  function(x) lm(lnwage ~ fe + marr + ed + ex + exsq, data=x))

cps.lm.race  <- group_by(cps78,nonwh) %>% do(cps.lm  = lm(lnwage ~ fe + marr + ed + ex + exsq, data=.))
tidy(cps.lm.race, cps.lm)

## Create the constant term (for convenience)
cps78$one <- 1

## Compute the average attributes by race
cps.mean.race <- summarize_all(group_by(cps78,nonwh),mean)
cps.mean.race <- select(cps.mean.race,lnwage,one,fe,marr,ed,ex,exsq)

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


(attr.b - attr.w) %*% coef.w / (attr.b %*% coef.b - attr.w %*% coef.w)
(coef.b - coef.w) %*% attr.b / (attr.b %*% coef.b - attr.w %*% coef.w)

