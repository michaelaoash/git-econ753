library(tidyverse)
library(haven)
library(reshape2)
library(plm)
library(lmtest)

## Warning: sheet 407 is used twice (once in NJ (1), once in PA (0) )

fastfood.df  <- read_dta("lab-07-wage-and-minimum-wage/ucla/fastfood.dta")
## fastfood.df  <- subset(fastfood.df, status2 == 1 | status2==3)
fastfood.df  <- mutate(fastfood.df,
                       sheet = ifelse(sheet==407 & state==1,408,sheet)  )

fastfood1.df  <- mutate(fastfood.df, fte=empft + emppt/2 + nmgrs, fte2=empft2 + emppt2/2 + nmgrs2, dfte=fte2-fte)

fastfood1.df %>% select(sheet, state, status2, fte, fte2, dfte) %>% arrange(state,sheet)

fastfood2.df  <- mutate(fastfood1.df,
                        fte2 = ifelse(!(status2 %in% c(0,1,3)),0, fte2 ),
                        dfte = ifelse(!(status2 %in% c(0,1,3)),fte2-fte , dfte)
                        )

## 1. FTE employment before, all available observation
with(fastfood1.df, tapply(fte, state, mean, na.rm=TRUE))
with(fastfood1.df, tapply(fte, state, sd, na.rm=TRUE)) / sqrt(with(fastfood1.df, tapply(!is.na(fte), state, sum, na.rm=TRUE)))

fastfood1.df %>% group_by(state) %>%
    summarize(mean(fte, na.rm=TRUE),
              sd(fte, na.rm=TRUE) / sqrt(sum(!is.na(fte), na.rm=TRUE )),
              sd(fte, na.rm=TRUE) / sqrt(sum(!is.na(fte)))
              )


## 2. FTE employment after, all available observation
with(fastfood1.df, tapply(fte2, state, mean, na.rm=TRUE))
with(fastfood1.df, tapply(fte2, state, sd, na.rm=TRUE)) / sqrt(with(fastfood1.df, tapply(!is.na(fte2), state, sum, na.rm=TRUE)))


fastfood1.df %>% group_by(state) %>%
    summarize(mean(fte2, na.rm=TRUE),
              sd(fte2, na.rm=TRUE) / sqrt(sum(!is.na(fte2), na.rm=TRUE ))
              )



## 3. Change in mean FTE employment
## Standard error of the difference in means
## Having trouble generating the published standard errors
## Issue is correlated error within store (sheet)
## See below for clustered standard error approach
with(fastfood1.df, tapply(fte2, state, mean, na.rm=TRUE)) - with(fastfood1.df, tapply(fte, state, mean, na.rm=TRUE))
sqrt( (with(fastfood1.df, tapply(fte2, state, sd, na.rm=TRUE)) /
         sqrt(with(fastfood1.df, tapply(!is.na(fte2) , state, sum) )))^2 +
      (with(fastfood1.df, tapply(fte, state, sd, na.rm=TRUE)) /
         sqrt(with(fastfood1.df, tapply(!is.na(fte) , state, sum) )))^2 )




## Standard error with unequal variance formula
## Having trouble generating the published standard errors
sqrt (((with(fastfood1.df, tapply(!is.na(fte2), state, sum )) - 1 ) * with(fastfood1.df, tapply(fte2, state, sd, na.rm=TRUE)) ^2 +
(with(fastfood1.df, tapply(!is.na(fte), state, sum )) - 1 ) * with(fastfood1.df, tapply(fte, state, sd, na.rm=TRUE)) ^2) /
( (with(fastfood1.df, tapply(!is.na(fte2), state, sum )) - 1 ) + (with(fastfood1.df, tapply(!is.na(fte), state, sum )) - 1 ) )) *
   sqrt(with(fastfood1.df, (  1/tapply( !is.na(fte2 ), state, sum  ) )) + with(fastfood1.df, (  1/tapply( !is.na(fte ), state, sum  ) )))




## 4. Change in mean FTE employment, balanced sample of stores
## Standard error of the change
with(fastfood1.df, tapply(fte2 - fte , state, mean, na.rm=TRUE))
with(fastfood1.df, tapply(fte2 - fte , state, sd,   na.rm=TRUE)) /
    sqrt(with(fastfood1.df, tapply(!is.na(fte2 - fte), state, sum) ))

## 5. Change in mean FTE employment, setting FTE at temporarily closed stores to 0
## Standard error of the change
with(fastfood2.df, tapply(fte2 - fte , state, mean, na.rm=TRUE))
with(fastfood2.df, tapply(fte2 - fte , state, sd,   na.rm=TRUE)) /
    sqrt(with(fastfood2.df, tapply(!is.na(fte2 - fte), state, sum) ))


## 4a. Change in mean FTE employment, balanced sample of stores
## Standard error of the change
fastfood3.df  <- subset(fastfood1.df, (status2==1 | status2==3) & !is.na(fte) & !is.na(fte2))
with(fastfood3.df, tapply(fte2, state, mean, na.rm=TRUE)) - with(fastfood3.df, tapply(fte, state, mean, na.rm=TRUE))
sqrt( (with(fastfood3.df, tapply(fte2, state, sd, na.rm=TRUE)) /
         sqrt(with(fastfood3.df, tapply(!is.na(fte2) , state, sum) )))^2 +
      (with(fastfood3.df, tapply(fte, state, sd, na.rm=TRUE)) /
         sqrt(with(fastfood3.df, tapply(!is.na(fte) , state, sum) )))^2 )
with(fastfood3.df, tapply(fte2 - fte , state, sd,   na.rm=TRUE)) /
    sqrt(with(fastfood3.df, tapply(!is.na(fte2 - fte), state, sum) ))



fastfood.long.df  <- melt(fastfood1.df, id.vars=c("sheet","chain","co_owned","state","southj","centralj","northj","pa1","pa2","shore","status2"))
fastfood.long.df  <- subset(fastfood.long.df, variable=="fte" | variable=="fte2")

fastfood.long.df  <- mutate(fastfood.long.df,
                            sheet.f  = factor(sheet),
                            variable = factor(variable)
                            )

did.plm  <- plm(value ~ state*variable, data=fastfood.long.df , model='pooling', index=c("sheet","variable"))
G <- length(unique(fastfood.long.df$sheet))
N <- length(fastfood.long.df$sheet)
dfa <- (G/(G - 1)) * (N - 1)/did.plm$df.residual
coeftest(did.plm, vcov=dfa*vcovHC(did.plm, type = 'HC1', cluster = 'group'))

did.plm  <- plm(value ~ variable, data=subset(fastfood.long.df,state==1), model='pooling', index=c('sheet'))
G  <- length(unique(subset(fastfood.long.df,state==1)$sheet))
N <- length(subset(fastfood.long.df, state==1)$sheet)
dfa <- (G/(G - 1)) * (N - 1)/did.plm$df.residual
coeftest(did.plm, dfa*vcovHC(did.plm, type = 'HC1', cluster = 'group'))

did.plm  <- plm(value ~ variable, data=subset(fastfood.long.df,state==0), model='pooling', index=c('sheet'))
G  <- length(unique(subset(fastfood.long.df,state==0)$sheet))
N <- length(subset(fastfood.long.df, state==0)$sheet)
dfa <- (G/(G - 1)) * (N - 1)/did.plm$df.residual
coeftest(did.plm, dfa*vcovHC(did.plm, type = 'HC1', cluster = 'group'))


summary(didregression  <- lm(value ~ state*variable, data=subset(fastfood.long.df)))


## Try Random Effects Moel
library(lme4)
summary(didregression  <- lmer(value ~ state*variable  + (1 | sheet ), data=subset(fastfood.long.df, variable=="fte" | (variable=="fte2" & status2 %in% c(1,3) )  )))


## This replicates the mean change result!
final_data=subset(fastfood.long.df, (variable=="fte" & !is.na(value)) | (variable=="fte2" & status2 %in% c(1,3) ))
summary(didregression  <- lm(value ~ factor(sheet) + state*variable, data=final_data))
coeftest(didregression, vcovHC(didregression, type = 'HC1', cluster = 'group'))

