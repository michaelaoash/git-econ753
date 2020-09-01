## source("andong.R",print.eval=TRUE)
## Send output to log (messages stay on console)
library(tidyverse)
library(haven)
andong <- read_dta("sbegnew.dta")
andong <-subset(andong, !is.na(gyp) & !is.na(tori))
attach(andong)


plot(tori,gyp)
text(tori,gyp,name)

ggplot(andong, aes(y=gyp, x=tori)) + geom_point() + geom_text(label=name) + geom_smooth(method="lm")



fwl <- function(frame,gypu,toriu) {
  lm.gypu <- lm(gyp ~ lrgdp + lsec + revcoup + govi + pii + bmpi + bpyi,frame)
  gypu <- lm.gypu$residual
  lm.toriu <- lm(tori ~ lrgdp + lsec + revcoup + govi + pii + bmpi + bpyi,frame)
  toriu <- lm.toriu$residual
  lm.u <- lm(gypu ~ toriu)
  summary(lm.u)
}


fwl(andong,gypu,toriu)

plot(toriu,gypu)
text(toriu,gypu,name)

print("Left-click each country to omit; right-click when done.")
omit <- identify(toriu,gypu,plot=FALSE)
name[omit]

except <- andong[-omit]

