library(tidyverse)  ## include dplyr
library(haven)      ## read dta files, etc.

cps  <- read_dta("chap5-cps.dta")

with(cps, table(year))

cps78  <- filter(cps,  year==1978)
cps85  <- filter(cps,  year==1985)

## Berndt Chapter 5
## https://courses.umass.edu/econ753/berndt.poe/Ch05.pdf

mean(cps78$lnwage)

cps78  <- mutate(cps78,
                 wage = exp(lnwage)
                 )


cps85  <- mutate(cps85,
                 wage = exp(lnwage)
                 )

with(cps78, mean(lnwage))
with(cps78, mean(wage))
with(cps78, median(wage))
with(cps78, exp(mean(lnwage)))

with(cps78, sd(wage))
with(cps78, sd(lnwage))
with(cps85, sd(lnwage))



