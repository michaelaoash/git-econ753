pdf(file = "hmda-roc.pdf", width = 0, height = 0, paper = "USr")
## Some work with the Home Mortgage Disclosure Act Data (Munnell et al. 1992 via Stock and Watson, Chapter 11)
library(tidyverse)
library(haven)
library(janitor)
library(lmtest)
library(party)
library(here)

## Read HMDA data from Stata file.
hmda <- read_dta(here("lab-08-hmda", "hmda_sw.dta"))
## hmda <- read_dta("hmda_sw.dta")

## Exclude applications for multifamily
hmda <- filter(hmda, s51!=3)
## Exclude applications withdrawn or incomplete
hmda <- filter(hmda, s7!=4 & s7!=5)
## Only home-purchase loans (not home improvement, refinance, etc.)
hmda <- filter(hmda, s4==1)
## Code deny
hmda$deny.f <- factor(hmda$s7==3)
hmda$deny <- 1*(hmda$s7==3)

## Limit sample to whites and blacks
hmda <- filter(hmda, (s13==3)|(s13==5))
## Make race a "factor"
hmda$race <- relevel(factor(hmda$s13, label=c("black", "nh.white")), ref="nh.white")

## Code some additional variables
## All of these are explained in hmda.pdf
hmda  <- mutate(hmda,
                pi_rat   = s46 / 100,
                income   = s31a,
                black    = 1 * (s13 == 3),
                hse_inc  = s45 / 100,
                loan_val = s6 / s50,
                ccred    = s43,
                mcred    = s42,
                pubrec   = 1 * (s44 > 0),
                denpmi   = 1 * (s53 == 1),
                selfemp  = 1 * (s27a == 1),
                married  = 1 * (s23a == "M"),
                single   = 1 * (married == 0),
                hischl   = 1 * (school >= 12),
                probunmp = uria,
                condo    = 1 * (s51 == 1),
                ltv_med  = 1 * (loan_val >= 0.80) * (loan_val <= 0.95),
                ltv_high = 1 * (loan_val > 0.95),
                blk_pi   = black * pi_rat,
                blk_hse  = black * hse_inc,
                ccred3   = 1 * (ccred == 3),
                ccred4   = 1 * (ccred == 4),
                ccred5   = 1 * (ccred == 5),
                ccred6   = 1 * (ccred == 6),
                mcred3   = 1 * (mcred == 3),
                mcred4   = 1 * (mcred == 4)
                )

hmda %>%
    select(deny, black, pi_rat, income, hse_inc, ltv_med, ltv_high, ccred, mcred, pubrec, denpmi, selfemp, single, hischl, probunmp, mcred3, mcred4, ccred3, ccred4, ccred5, ccred6, condo) %>%
    summary()

by(subset(hmda,
          select =
              c(deny, pi_rat, hse_inc, ltv_med, ltv_high, ccred, mcred, pubrec, denpmi, selfemp, single, hischl, probunmp, mcred3, mcred4, ccred3, ccred4, ccred5, ccred6, condo)),
   hmda$black,
   summary)

hmda %>%
    group_by(black) %>%
    select(deny, pi_rat, hse_inc, ltv_med, ltv_high, ccred, mcred, pubrec, denpmi, selfemp, single, hischl, probunmp, mcred3, mcred4, ccred3, ccred4, ccred5, ccred6, condo) %>%
    dplyr::summarize(mean(deny), median(pi_rat), mean(pi_rat), mean(ltv_med), mean(ltv_high))


Hmisc::describe( subset(hmda,
                        select=c(deny, black, pi_rat, hse_inc, ltv_med,
                                 ltv_high, ccred, mcred, pubrec, denpmi, selfemp, single, hischl, probunmp,
                                 mcred3, mcred4, ccred3, ccred4, ccred5, ccred6, condo)))


## Contingency table
table(hmda$race, hmda$deny.f)

with(hmda, table(race, deny.f))

## What is the difference between row and column percents?
prop.table(table(hmda$race, hmda$deny.f), 1)
prop.table(table(hmda$race, hmda$deny.f), 2)
prop.table(table(hmda$race, hmda$deny.f))


hmda %>% tabyl(race, deny.f)

hmda %>% tabyl(race, deny.f) %>% adorn_title()

hmda %>% tabyl(race, deny.f) %>% adorn_totals("both")

hmda %>% tabyl(race, deny.f) %>% adorn_totals("both") %>% adorn_percentages() %>% adorn_pct_formatting() %>% adorn_title()

hmda %>% tabyl(race, deny.f) %>% adorn_totals("both") %>% adorn_percentages(denominator = "col") %>% adorn_pct_formatting() %>% adorn_title()

hmda %>% tabyl(race, deny.f) %>% chisq.test()


## Use a t-test to check if the denial rates are the same for
## white and black
t.test(hmda$deny ~ hmda$race)

with(hmda, t.test(deny ~ race))

t.test(hmda$pi_rat ~ hmda$race)

## Same test with LPM regression
hmda.1 <- lm(deny ~ race, data=hmda)
coeftest(hmda.1, vcov = vcovHC(hmda.1, type = "HC1"))

hmda.2 <- lm(deny ~ pi_rat, data=hmda)
coeftest(hmda.2, vcov = vcovHC(hmda.2, type = "HC1"))

hmda.3 <- lm(deny ~ race + pi_rat, data=hmda)
coeftest(hmda.3, vcov = vcovHC(hmda.3, type = "HC1"))

hmda_lpm <- lm(deny ~ race + pi_rat + hse_inc + ltv_med + ltv_high
             + ccred + mcred + pubrec + denpmi + selfemp, data=hmda)
coeftest(hmda_lpm, vcov = vcovHC(hmda_lpm, type = "HC1"))


hmda_probit <- glm(deny ~ race + pi_rat + hse_inc + ltv_med + ltv_high
             + ccred + mcred + pubrec + denpmi + selfemp, data=hmda, family=binomial(link = probit))
summary(hmda_probit)
coeftest(hmda_probit, vcov = vcovHC(hmda_probit, type = "HC1"))

hmda_logit <- glm(deny ~ race + pi_rat + hse_inc + ltv_med + ltv_high
             + ccred + mcred + pubrec + denpmi + selfemp, data=hmda, family=binomial(link = logit))
summary(hmda_logit)
coeftest(hmda_logit, vcov = vcovHC(hmda_logit, type = "HC1"))



## Assessing model performance: Receiver Operating Characteristic (ROC) curve
hmda_predicted <- hmda %>% mutate(
             probhat = predict(hmda_logit, type = "response")
         )
roc <- NULL
for (c in seq(from = 0, to = 1, by = 0.01)) {
    hmda_predicted <- hmda_predicted %>% mutate(
                           sensitive = ifelse(deny == 1, probhat > c, NA), ## Model catches true positives at cutoff c
                           specific = ifelse(deny == 0, probhat < c, NA) ## Model avoids false positives at cutoff c
                       )
    performance_c <- c(cutoff = c,
                       sensitivity = mean(hmda_predicted$sensitive, na.rm = TRUE),
                       specificity = mean(hmda_predicted$specific, na.rm = TRUE),
                       `1 - specificity` = 1 - mean(hmda_predicted$specific, na.rm = TRUE)
                       )
    roc <- bind_rows(roc, performance_c)
}

roc <- roc %>% mutate(clabel = ifelse(cutoff %in% seq(from = 0, to = 1, by = 0.1), cutoff, NA))

my_colors <- c("sensitivity" = "blue", "specificity" = "red", "1 - specificity" = "red")

roc %>% pivot_longer(cols = -c(cutoff, clabel)) %>% filter(name == "sensitivity") %>%
    ggplot(aes(x = cutoff, y = value, color = name)) + geom_line() + scale_color_manual(values = my_colors)
roc %>% pivot_longer(cols = -c(cutoff, clabel)) %>% filter(name == "specificity") %>%
    ggplot(aes(x = cutoff, y = value, color = name)) + geom_line() + scale_color_manual(values = my_colors)
roc %>% pivot_longer(cols = -c(cutoff, clabel)) %>% filter(name == "1 - specificity") %>%
    ggplot(aes(x = cutoff, y = value, color = name)) + geom_line() + scale_color_manual(values = my_colors)
roc %>% pivot_longer(cols = -c(cutoff, clabel)) %>% filter(!name %in% c("specificity")) %>%
    ggplot(aes(x = cutoff, y = value, color = name)) + geom_line() + scale_color_manual(values = my_colors)

roc %>% ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_line(linewidth = 2) +
    geom_label(aes(label = clabel)) +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, linetype = "dotted") +
    annotate(geom = "text", x = 0.01, y = 0.99, label = "perfect\nmodel") +
    coord_equal() +
    labs(title = "Boston HMDA Denial Model: Receiver Operating Characterisic (ROC) Curve")


## Exercise: plot the ROC for a model that does not include race



## Oaxaca-Blinder decomposition
## Run the regression separately by race
hmda.lm.race <- by(hmda, hmda$black,
                   function(x) lm(deny ~ pi_rat + hse_inc + ltv_med +
                   ltv_high + ccred + mcred + pubrec + denpmi +
                   selfemp, data=x))

## Create the constant term (for convenience)
hmda$one <- 1

## Compute the average attributes by race
(hmda.mean.race <-
aggregate(subset(hmda, select=c(deny, one, pi_rat, hse_inc, ltv_med, ltv_high, ccred, mcred, pubrec, denpmi, selfemp)),
          list(hmda$black), mean))

## Extract average attributes by race
(attr.w <- as.matrix(hmda.mean.race)[1, 3:12])
(attr.b <- as.matrix(hmda.mean.race)[2, 3:12])

## Extract coefficients by race
(coef.w <- as.matrix(sapply(hmda.lm.race, coef))[1:10, 1])
(coef.b <- as.matrix(sapply(hmda.lm.race, coef))[1:10, 2])

## Difference in mean outcome
as.matrix(hmda.mean.race[2, 2]) - as.matrix(hmda.mean.race[1, 2])
attr.b %*% coef.b - attr.w %*% coef.w

## Difference attributable to attributes
(attr.b - attr.w) %*% coef.b
## Difference attributable to coefficients
(coef.b - coef.w) %*% attr.w

## As shares of total
(attr.b - attr.w) %*% coef.b / (attr.b %*% coef.b - attr.w %*% coef.w)
(coef.b - coef.w) %*% attr.w / (attr.b %*% coef.b - attr.w %*% coef.w)



## Some work with classification trees
## This matches the Varian tree
Hmda  <- hmda %>% select(deny, black, pi_rat, hse_inc, loan_val, ccred, mcred, pubrec, denpmi)

plot(ctree(deny ~ ., data = Hmda[complete.cases(Hmda), ]), type = "simple")
