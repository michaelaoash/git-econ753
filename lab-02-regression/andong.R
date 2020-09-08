## ZAP
## Based on Zhu, Ash, and Pollin (2006) https://doi.org/10.1080/0269217032000148645
## Frisch Waugh Lovell partial regression model
library(tidyverse)
library(haven)
andong <- read_dta("sbegnew.dta")

## Clean by deleting observations with any missing data
andong <-subset(andong, !is.na(gyp) & !is.na(tori))

## attach() is bad form but makes it easier to access variables.
## Challenge: rewrite with good tidyverse syntax
attach(andong)


plot(tori,gyp)
text(tori,gyp,name)
ggplot(andong, aes(y=gyp, x=tori)) + geom_point() + geom_text(label=name) + geom_smooth(method="lm")

## Frisch Waugh Lovell (partial regression)
fwl <- function(frame,gypu,toriu) {
    name  <<- frame$name
    lm.gypu <- lm(gyp ~ lrgdp + lsec + revcoup + govi + pii + bmpi + bpyi,frame)
    gypu <<- lm.gypu$residual
    lm.toriu <- lm(tori ~ lrgdp + lsec + revcoup + govi + pii + bmpi + bpyi,frame)
    toriu <<- lm.toriu$residual
    lm.u  <- lm(gypu ~ toriu)
    summary(lm.u)
}

fwl(andong,gypu,toriu)
plot(toriu,gypu)
text(toriu,gypu,name)


ggplot(data.frame(toriu,gypu,name), aes(x=toriu, y=gypu) ) + geom_point() + geom_text(label=name) + geom_smooth(method="lm")




## Not implemented: test omitting outliers
plot(toriu,gypu)
text(toriu,gypu,name)
print("Left-click each country to omit; right-click when done.")
omit <- identify(toriu,gypu,plot=FALSE)
name[omit]
except <- andong[-omit]

