## Working with the HMDA data
library(tidyverse)
library(haven)
library(janitor)
library(lmtest)
library(here)


hmda <- read_dta(here("lab-08-hmda/hmda_sw.dta"))

## Exclude multifamily dwellings
hmda <- hmda %>% filter(s51!=3)

## Exclude incomplete or withdrawn
hmda <- hmda %>% filter(s7!=4 & s7!=5)

hmda$deny.f <- factor(hmda$s7==3)

hmda$deny <- 1*(hmda$s7==3)

hmda <- hmda %>% filter(s13==3 | s13==5)

hmda$race <- relevel(factor(hmda$s13, label=c("black","nh.white")), ref="nh.white")

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




summary(lm( deny ~ black, data=hmda))

summary(lm( deny ~ black + pi_rat, data=hmda))


hmda %>% group_by(race) %>%
    summarize(mean(deny), median(pi_rat))



