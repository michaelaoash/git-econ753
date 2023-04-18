
rm(list=ls())

# Program Details: Overview -----------------------------------------------

# *************************************************************************
# Programmer:	Hien Nguyen
# Case:	Econ 753 - Replication
# Program:  clean_NHIS_earnings_dif_v1.R
# Version: v1
# Date Created: Sep 20, 2020
# Last Updated: Sep 20, 2020
# Description:
# Notes:
# *************************************************************************

## Load the needed libraries
library(tidyverse) ## Includes advanced data management and graphics
library(haven) ## to read Stata and other data formats
library(lmtest) ## For robust standard errors
library(sandwich) ## For robust standard errors
library(Hmisc) ## For some nicely formatted summary stats
library(viridis) ## color scheme viridis, which is consistent color or b/w scale
library(pastecs) ## For some nicely formatted summary stats
library(tibble)
library(openxlsx)
library(xtable)
library(readstata13)
library(reshape2) #for melt function
library(fastDummies) #for dummies creation
library(stargazer)
library(car) ## for testing linear combinations with linearHypothesis()
library(ipumsr) ## for ACS data handling from IPUMS
library(Hmisc) #for weighted std
library(survey)
library(grid)
library(gridExtra)
library(data.table) # for enhance casting and melting functionality


## Set some options scipen controls scientific notation versus
## decimals (higher number = favor decimals over scientific notation)
## width controls the column width of output
options(scipen=1000,width=200)

# I - Import data and set up -------------------

     ## Directory
     path <- "/Users/Admin_2/Documents/UMass PhD Folder/Carpenter Eppink replication project/Econ 753 Lab presentation/"
     input <- file.path(path,"Data")
     output <- file.path(path,"Output")
     chart <- file.path(path,"Charts")

     ## Import data
      nhis <- read.dta13(file.path(input, "nhis_newvar.dta"))
      # CE sample
        CE <- nhis %>%
        filter(age_p>=25 & age_p<=64) %>%
        filter(is.na(ernyr_i2)=="FALSE") %>%
        filter(ernyr_f=="0 Reported")

## II - Descriptive statistics

    # Survey design set
    nhissvy <- svydesign(id=~psu_p, strata=~strat_p,
                      nest = TRUE,
                      weights=~wtfa_new,
                      data=nhis)

    CEsample <- subset(nhissvy,age_p>24 & age_p<65 & is.na(ernyr_i2)=="FALSE" & ernyr_f=="0 Reported")
    summary(CEsample)

    #Weighted mean
    sample_mean <- svyby(~age_p + edu_ba + edu_aso + edu_col + edu_hs + edu_less + edu_less2 +
                          white + partner + children + reg_ne + reg_midwe +  reg_so + reg_we +
                          ernyr_i2 + emp_full,~sogi, design = CEsample,na.rm=TRUE,svymean)

    table1a <- sample_mean %>%
    select(age_p, edu_ba, edu_aso, edu_col, edu_hs,edu_less, edu_less2,
                          white , partner , children , reg_ne,reg_midwe, reg_so, reg_we,
                          ernyr_i2, emp_full,sogi)%>%
     mutate(stat="mean")

    # Weighted variance

      var.age_p <- svyby(~age_p, ~sogi, design = CEsample, na.rm=TRUE,svyvar) %>%
            select(-se)
      var.edu_ba <- svyby(~edu_ba, ~sogi, design = CEsample, na.rm=TRUE,svyvar)%>%
            select(-se)
      var.edu_aso <- svyby(~edu_aso, ~sogi, design = CEsample, na.rm=TRUE,svyvar)%>%
            select(-se)
      var.edu_col <- svyby(~edu_col, ~sogi, design = CEsample, na.rm=TRUE,svyvar)%>%
            select(-se)
      var.edu_hs <- svyby(~edu_hs, ~sogi, design = CEsample,na.rm=TRUE, svyvar)%>%
            select(-se)
      var.edu_less <- svyby(~edu_less, ~sogi, design = CEsample,na.rm=TRUE, svyvar)%>%
            select(-se)
      var.edu_less2 <- svyby(~edu_less2, ~sogi, design = CEsample,na.rm=TRUE, svyvar)%>%
            select(-se)
      var.white <- svyby(~white, ~sogi, design = CEsample,na.rm=TRUE, svyvar)%>%
            select(-se)
      var.partner <- svyby(~partner, ~sogi, design = CEsample,na.rm=TRUE, svyvar)%>%
            select(-se)
      var.children <- svyby(~children, ~sogi, design = CEsample,na.rm=TRUE, svyvar)%>%
            select(-se)
      var.reg_ne <- svyby(~reg_ne, ~sogi, design = CEsample, na.rm=TRUE,svyvar)%>%
            select(-se)
      var.reg_midwe <- svyby(~reg_midwe, ~sogi, design = CEsample, na.rm=TRUE,svyvar)%>%
            select(-se)
      var.reg_so <- svyby(~reg_so, ~sogi, design = CEsample,na.rm=TRUE, svyvar)%>%
            select(-se)
      var.reg_we <- svyby(~reg_we, ~sogi, design = CEsample,na.rm=TRUE, svyvar)%>%
            select(-se)
      var.ernyr_i2 <- svyby(~ernyr_i2, ~sogi, design = CEsample, na.rm=TRUE,svyvar)%>%
            select(-se)
      var.emp_full <- svyby(~emp_full, ~sogi, design = CEsample,na.rm=TRUE, svyvar)%>%
            select(-se)

      sample_var <-  var.age_p %>%
        left_join(var.edu_ba, by = "sogi") %>%
        left_join(var.edu_aso, by = "sogi") %>%
        left_join(var.edu_col, by = "sogi") %>%
        left_join(var.edu_hs, by = "sogi") %>%
        left_join(var.edu_less, by = "sogi") %>%
        left_join(var.edu_less2, by = "sogi") %>%
        left_join(var.white, by = "sogi") %>%
        left_join(var.partner, by = "sogi") %>%
        left_join(var.children, by = "sogi") %>%
        left_join(var.reg_ne, by = "sogi") %>%
        left_join(var.reg_midwe, by = "sogi") %>%
        left_join(var.reg_so, by = "sogi") %>%
        left_join(var.reg_we, by = "sogi") %>%
        left_join(var.ernyr_i2, by = "sogi") %>%
        left_join(var.emp_full, by = "sogi")

      table1b <-  sample_var
      table1b[2:17] <- sqrt(table1b[2:17]) # calculate std dev
      table1b <-  table1b %>%
        mutate(stat="sd")

      table1 <- rbind(table1a, table1b)
      table1 <- select(table1, sogi, stat, everything())

      table1[4:16] <- round(table1[4:16],3)
      table1[3] <- round(table1[3],1)
      table1[17] <- round(table1[17],2)
      table1[18] <- round(table1[18],3)

      table1 <-melt(table1, id=c("stat","sogi"))
      table1 <- dcast(table1, stat + variable ~ sogi)
      table1 <- arrange(table1, variable)

##  Two-sample t-test for sogi: earnings and full-time employment only
      svyttest(ernyr_i2 ~ sogi, design=subset(CEsample, sogi=="male, straight" |sogi=="male, gay"))
      # t = -0.92032, df = 299, p-value = 0.3581
      # alternative hypothesis: true difference in mean is not equal to 0
      # difference in mean  -2582.006 (straight men earns 2,600 less)
      # Fail to reject H0 so dif is not statistically significant

      svyttest(emp_full ~ sogi, design=subset(CEsample, sogi=="male, straight" |sogi=="male, gay"))
      #t = 1.4156, df = 299, p-value = 0.1579
      # alternative hypothesis: true difference in mean is not equal to 0
      # difference in mean 0.02820251 (straight men 3% more likely)
      # Fail to reject H0, so no statistically significant difference

      ## So two-sample t-test shows that mean earnings and full-time employment are not statistically different between gay and straight, but statistically different betwee lesbian and straight

## III - Diagnostic plots

      ## Earnings distribution by gender
        fem.main <- subset(CE, sogi=="fem, straight" |sogi=="fem, lesbian"|sogi=="fem, bi" )
        male.main<- subset(CE, sogi=="male, straight" |sogi=="male, gay"|sogi=="male, bi" )

        fem.mean <- fem.main %>%
            group_by(sogi) %>%
            summarise(ernyr_i2 = mean(ernyr_i2, na.rm=TRUE))
        male.mean <- male.main %>%
            group_by(sogi) %>%
            summarise(ernyr_i2 = mean(ernyr_i2, na.rm=TRUE))

        # Density histogram
        labfem <- c("fem, straight" = "Women, heterosexual", "fem, lesbian" = "Women, lesbian","fem, bi" = "Women, bisexual" )
        labmale <- c("male, straight" = "Men, heterosexual", "male, gay" = "Men, gay","male, bi" = "Men, bisexual" )

        (densityplot.fe <- ggplot(fem.main, aes(x=ernyr_i2)) + geom_histogram(aes(y = ..density.., alpha=0.5))+
        facet_wrap ( ~ sogi  , ncol=1, labeller=labeller(sogi = labfem)) +
        geom_vline(data=fem.mean, aes(xintercept=ernyr_i2), color="blue", linetype="dashed", size=1) +
        labs(y="Density", x="Annual earning", caption="NB:The top 5% earnings are topcoded. Vertical lines correpond to mean earnings") + theme(legend.position="none", plot.caption = element_text(hjust = 0)) +
        ggtitle("Figure XX - Density distribution of annual earnings, \n female workers by SOGI, NHIS 2013-2015"))

        (densityplot.ma <- ggplot(male.main, aes(x=ernyr_i2)) + geom_histogram(aes(y = ..density.., alpha=0.5))+
        facet_wrap ( ~ sogi  , ncol=1, labeller=labeller(sogi = labmale)) +
        geom_vline(data=male.mean, aes(xintercept=ernyr_i2), color="blue", linetype="dashed", size=1) +
        labs(y="Density", x="Annual earning", caption="NB:The top 5% earnings are topcoded. Vertical lines correpond to mean earnings") +
        theme(legend.position="none", plot.caption = element_text(hjust = 0)) +
        ggtitle("Figure XX - Density distribution of annual earnings, \n male workers by SOGI, NHIS 2013-2015"))

      ## Distribution of homo workers by earnings percentile
        CE <-  CE %>%
        mutate(i.femles = ifelse(sogi=="fem, lesbian",1,0),
              i.femhet = ifelse(sogi=="fem, straight", 1,0),
              i.fembi = ifelse(sogi=="fem, bi",1,0),
              i.malegay = ifelse(sogi=="male, gay", 1,0),
              i.malehet = ifelse(sogi=="male, straight", 1,0),
              i.malebi = ifelse(sogi=="male, bi", 1,0)
            )

      tab.pct <- tibble(x = NA, q = NA, i.femles=NA,i.femhet=NA,
                            i.fembi=NA, i.malegay=NA, i.malehet= NA,i.malebi=NA  )

      for (x in seq(0, 1, by = 0.01)) {
      q <-  quantile(CE$ernyr_i2, x, na.rm=TRUE)
      count <- CE %>%
      filter(ernyr_i2<q)%>%
      summarise(i.femles = sum(i.femles),
                i.femhet = sum(i.femhet ),
                i.fembi =sum(i.fembi ),
                i.malegay =sum(i.malegay),
                i.malehet =sum(i.malehet),
                i.malebi =sum(i.malebi )
              )
      row <- cbind(x, q, count)
      tab.pct  <-rbind(tab.pct, row)
     }

    tab.pct  <-  tab.pct %>%
        filter(is.na(x)==FALSE & x!=0.0)%>%
          mutate(ratio_gay_het = (i.femles + i.malegay)/(i.femhet + i.malehet),
          fem_gay_het = i.femles /i.femhet,
          male_gay_het = i.malegay/i.malehet
        )
    # Ratio look pretty stable, both total and by gender

       plot.gaypct1 <- ggplot(tab.pct)+
       geom_point(mapping =aes(y=ratio_gay_het, x=q))+
       geom_line(mapping =aes(y=ratio_gay_het, x=q))+
       scale_y_continuous(name="", breaks=seq(0, 0.05, 0.01), limits=c(0, 0.05))+
       scale_x_continuous(name="(a) Both male and female", breaks=seq(0, 200000, 20000))

       plot.gaypct2 <-  ggplot(tab.pct)+
       geom_point(mapping =aes(y=fem_gay_het, x=q))+
       geom_line(mapping =aes(y=fem_gay_het, x=q))+
       scale_y_continuous(name="", breaks=seq(0, 0.05, 0.01), limits=c(0, 0.05))+
       scale_x_continuous(name="(b) Female only", breaks=seq(0, 200000, 20000))

       plot.gaypct3 <-  ggplot(tab.pct)+
       geom_point(mapping =aes(y=male_gay_het, x=q))+
       geom_line(mapping =aes(y=male_gay_het, x=q))+
       scale_y_continuous(name="", breaks=seq(0, 0.05, 0.01), limits=c(0, 0.05))+
       scale_x_continuous(name="(c) Male only", breaks=seq(0, 200000, 20000))

       (fig.gaypct <- grid.arrange(plot.gaypct1, plot.gaypct2, plot.gaypct3, ncol = 1, top="Figure XX - Ratio of homosexual to heterosexual workers by earning percentile"))

       # ggsave(file.path(chart,paste("ratio_by_earning_pctile_gayonly_", ver, ".png", sep="")), fig.gaypct, width=8, height=6, unit=c("in"))

    ## Earnings by education
      # Sample mean earning by edu level
      edu <- CE %>%
      filter(educ1<=21)%>%
      group_by(educ1) %>%
      summarise(group_mean=weighted.mean(ernyr_i2, wtfa_new)
      )

      # Mean earning by edu for gay and straight
      edu.sogi <- CE %>%
      filter(educ1<=21)%>%
      filter(sogi=="fem, straight" |sogi=="fem, lesbian"|sogi=="male, straight" |sogi=="male, gay") %>%
      mutate(sogi=recode_factor(sogi, "fem, straight"="fem_het", "fem, lesbian"="fem_les", "male, straight"="male_het", "male, gay"="male_gay"))%>%
      group_by(educ1,sogi) %>%
      summarise(mean=weighted.mean(ernyr_i2, wtfa_new),
                    )

      edu.sogi <- melt(edu.sogi, id=c("educ1", "sogi"))
      edu.sogi <- dcast(edu.sogi, educ1 ~sogi+variable)

      tab.edu <- edu %>%
          left_join(edu.sogi, by="educ1")%>%
          mutate(edu_level = case_when(
                  educ1 ==0 ~ "never attended",
                  educ1 ==1 ~ "1st grade",
                  educ1 ==2 ~ "2nd grade",
                  educ1 ==3 ~ "3rd grade",
                  educ1 ==4 ~ "4th grade",
                  educ1 ==5 ~ "5th grade",
                  educ1 ==6 ~ "6th grade",
                  educ1 ==7 ~ "7th grade",
                  educ1 ==8 ~ "8th grade",
                  educ1 ==9 ~ "9th grade",
                  educ1 ==10 ~"10th grade",
                  educ1 ==11 ~"11th grade",
                  educ1 ==12 ~"12th grade",
                  educ1 ==13 ~ "GED",
                  educ1 ==14 ~ "High school grad",
                  educ1 ==15 ~ "Some college, no degree",
                  educ1 ==16 ~ "Associate degree, technical",
                  educ1 ==17 ~ "Associate degree, academic",
                  educ1 ==18 ~ "Bachelor's",
                  educ1 ==19 ~ "Master's",
                  educ1 ==20 ~ "Professional degree",
                  educ1 ==21 ~ "PhD"
                    ))

    # Plot of mean earnings by edu
      (  plot_edu_fem <-  ggplot(tab.edu) +
        geom_line(mapping =aes(y=group_mean, x=reorder(edu_level, educ1), group=1)) +
        geom_point(mapping =aes(y=fem_het_mean, x=reorder(edu_level, educ1), color="Heterosexual")) +
        geom_point(mapping =aes(y=fem_les_mean, x=reorder(edu_level, educ1), color="Lesbian"))+
        labs(y="Annual earnings", x="", color="", caption="Solid line shows sample's weighted average earning (all SOGI groups)") + theme(legend.position="right", plot.caption = element_text(hjust = 0), axis.text.x = element_text(size =8, angle = 60, hjust = 1)) +
        ggtitle("Figure XX - Annual earnings and educational attainment, \n Heterosexual v. Lesbian women") +
        scale_color_viridis(option="inferno",end=0.6, discrete=TRUE)
      )
      (  plot_edu_male <-  ggplot(tab.edu) +
        geom_line(mapping =aes(y=group_mean, x=reorder(edu_level, educ1), group=1)) +
        geom_point(mapping =aes(y=male_het_mean, x=reorder(edu_level, educ1), color="Heterosexual")) +
        geom_point(mapping =aes(y=male_gay_mean, x=reorder(edu_level, educ1), color="Gay"))+
        labs(y="Annual earnings", x="", color="", caption="Solid line shows sample's weighted average earning (all SOGI groups)") + theme(legend.position="right", plot.caption = element_text(hjust = 0), axis.text.x = element_text(size =8, angle = 60, hjust = 1)) +
        ggtitle("Figure XX - Annual earnings and educational attainment, \n Heterosexual v. Gay men") +
        scale_color_viridis(option="inferno",end=0.6, direction = -1,discrete=TRUE)
      )

## IV - OLS regression
    CE <- CE %>%
          mutate(so.homo=ifelse(sogi=="fem, lesbian" | sogi=="male, gay",1,0),
                so.bi=ifelse(sogi=="fem, bi" | sogi=="male, bi",1,0),
                so.other=ifelse(sogi=="fem, other" | sogi=="male, other",1,0),
                so.dontknow=ifelse(sogi=="fem, don't know" | sogi=="male, don't know",1,0),
                so.refuse=ifelse(sogi=="fem, refuse" | sogi=="male, refuse",1,0),
                so.missing=ifelse(sogi=="fem, missing" | sogi=="male, missing",1,0),
      )

    # Log earnings
    CE <- CE %>%
          mutate(ln_earn=log(ernyr_i2))

    #Demographic control
    CE <- CE %>%
          mutate(age2=age_p^2,
                edu_level = case_when(
                  educ1==13 | educ1==14 ~ "HS", #high school or GED
                  educ1>=18 & educ1<=21 ~ "BA", #bachelor degree or more
                  educ1==16 | educ1==17 ~ "asso", #associate degree
                  educ1==15 ~ "some_col", #some college, no degree
                  educ1<=12 ~ "less_HS", #less than HS
                  educ1==97 ~ "refused",
                  educ1==99 ~ "unknown"),
              edu_level = relevel(as.factor(edu_level), ref="HS"), #reference cat is white
              edu_level2 = case_when( # This is CE definition
                educ1==13 | educ1==14 ~ "HS", #high school or GED
                educ1>=18 & educ1<=21 ~ "BA", #bachelor degree or more
                educ1==16 | educ1==17 ~ "asso", #associate degree
                educ1==15 ~ "some_col", #some college, no degree
                educ1<12 ~ "less_HS", #less than HS
                educ1==97 ~ "refused",
                educ1==99 ~ "unknnow"),
            edu_level2 = relevel(as.factor(edu_level2), ref="HS"),
              race = relevel(as.factor(racerpi2), ref="1"), #reference cat is white
              hisp=ifelse(hispan_i==12,0,1),
              rel_stat=case_when (
                r_maritl== "1 Married - spouse in household" ~ "partnered",
                r_maritl== "2 Married - spouse not in household" ~ "partnered",
                r_maritl== "4 Widowed" ~ "widowed",
                r_maritl== "5 Divorced" ~ "divorced",
                r_maritl== "6 Separated" ~ "separated",
                r_maritl== "7 Never married" ~ "single",
                r_maritl== "8 Living with partner" ~ "partnered",
                r_maritl== "9 Unknown marital status" ~ "unknown"
              ),
              rel_stat = factor(rel_stat, levels = c("single", "widowed", "divorced", "separated", "partnered", "unknown")), # single is reference cat
              d_kid5=ifelse(kid_05==0,0,1),
              d_kid17=ifelse(kid_617==0,0,1)
                        )

  # Labor characteristics : tenure, firm size, sectors
    CE <- CE %>%
    mutate(tenure=ifelse(yrswrkpa<=35, yrswrkpa, NA),
          tenure2 = tenure^2,
          tenure_topcoded=ifelse(yrswrkpa==35, 1, 0),
          tenure_refused = ifelse(yrswrkpa==97,1,0),
          tenure_missing = ifelse(yrswrkpa==98 | is.na(yrswrkpa)==TRUE, 1, 0),
          tenure_dontknow = ifelse(yrswrkpa==99,1,0),
          firm_size=relevel(as.factor(locall1a), ref="1"),
          sector=case_when(
          as.integer(wrkcata)%in% c(1,5,6) ~ "private",
          as.integer(wrkcata)%in% c(2,3,4) ~ "public",
          as.integer(wrkcata) ==7 ~ "refused",
          as.integer(wrkcata) ==8 ~ "missing",
          as.integer(wrkcata) ==9 ~ "unknown"
          ),
          sector=factor(sector, levels=c("private", "public","refused","missing","unknown")),
          occ=factor(occupn2),
          occ=relevel(occ,ref = 13), # ref cat is 13 which is food service
          ind=factor(indstrn2),
          ind=relevel(ind, ref=18) # ref cat is 18 which is food and accomodation industry
        )

    # Baseline: years and month dummies only
    earn.fem.base <- lm(ln_earn ~ so.homo + so.bi + so.other+
              so.dontknow + so.refuse +  so.missing +
              factor(srvy_yr) + factor(intv_mon),
              data=subset(CE, sex=="2 Female" & age_p>=25 & age_p<=64 & emp_full==1 & is.na(ernyr_i2)=="FALSE" & ernyr_f=="0 Reported"),
              weights=wtfa_new)
    coeftest(earn.fem.base,vcov=vcovHC(earn.fem.base,type="HC1"))

    earn.male.base <- lm(ln_earn~ so.homo + so.bi + so.other+
              so.dontknow + so.refuse +  so.missing +
              factor(srvy_yr) + factor(intv_mon),
              data=subset(CE, sex=="1 Male" & age_p>=25 & age_p<=64 & emp_full==1 & is.na(ernyr_i2)=="FALSE" & ernyr_f=="0 Reported"),
              weights=wtfa_new)
    coeftest(earn.male.base,vcov=vcovHC(earn.male.base,type="HC1"))

    # Full model
    earn.fem.full <- update(earn.fem.base, formula= . ~ . +
                age_p + age2 +
                factor(edu_level) + factor(race) + hisp +
                factor(rel_stat) + d_kid5 + d_kid17 + reg_ne + reg_midwe + reg_so+
                tenure + tenure2 + tenure_refused +  tenure_missing + tenure_dontknow +
                 factor(firm_size) + factor(sector)+
                factor(occupn2) + factor(indstrn2)+
                as.integer(tcearn_f) + tenure_topcoded)
    coeftest(earn.fem.full,vcov=vcovHC(earn.fem.full,type="HC1"))

    earn.male.full <- update(earn.male.base, formula= . ~ . +
                  age_p + age2 +
                  factor(edu_level) + factor(race) + hisp +
                  factor(rel_stat) + d_kid5 + d_kid17 + reg_ne + reg_midwe + reg_so+
                  tenure + tenure2 + tenure_refused +  tenure_missing + tenure_dontknow +
                   factor(firm_size) + factor(sector)+
                  factor(occupn2) + factor(indstrn2)+
                  as.integer(tcearn_f) + tenure_topcoded)
    coeftest(earn.male.full,vcov=vcovHC(earn.male.full,type="HC1"))

## V - Robustness check

  ## remove uninformative observation
  CE <- dummy_cols(CE, select_columns =c("srvy_yr","intv_mon", "edu_level", "race", "rel_stat","firm_size", "sector","occ","ind"))

  regdata <- subset(CE,edu_level_refused==0 & edu_level_unknown==0 &
              tenure_refused==0 & tenure_missing==0 & tenure_dontknow==0 & is.na(tenure)==FALSE &
              rel_stat_unknown==0 &
              firm_size_97==0 & firm_size_99==0 &
              sector_refused==0 & sector_missing==0 & sector_unknown==0 &
              occ_23==0 & occ_97==0 & occ_98==0 & occ_99==0 & # 23 is army
              ind_21==0 & ind_97==0 & ind_98==0 & ind_99==0 & # 21 is army
              occ_18==0 & ind_1==0 & # these two doesn't have any gay men in them
              age_p>=25 & age_p<=64 & emp_full==1 & is.na(ernyr_i2)=="FALSE" & ernyr_f=="0 Reported"
              )

  earn.male.base <- lm(ln_earn~ so.homo + so.bi + so.other+
          so.dontknow + so.refuse +  so.missing +
          factor(srvy_yr) + factor(intv_mon),
          data=subset(regdata,sex=="1 Male"),
          weights=wtfa_new)
  coeftest(earn.male.base,vcov=vcovHC(earn.male.base,type="HC1"))
  # notice this changes a litle bit from the replicated result in section IV

  earn.male.demo <- update(earn.male.base, formula= . ~ . +
                          age_p + age2 +
              factor(edu_level) + factor(race) + hisp +
              factor(rel_stat) + d_kid5 + d_kid17 + reg_ne + reg_midwe + reg_so
              )
  coeftest(earn.male.demo,vcov=vcovHC(earn.male.demo,type="HC1"))

  earn.male.full <- update(earn.male.demo, formula= . ~ . +
                        tenure + tenure2 +
                        factor(firm_size) + factor(sector)+
                        occ + ind+
                        as.integer(tcearn_f) + tenure_topcoded
              )
  coeftest(earn.male.full,vcov=vcovHC(earn.male.full,type="HC1"))

  earn.male.young <- update(earn.male.full, formula= . ~ .,
              data=subset(regdata,sex=="1 Male" & age_p>=25 & age_p<=44),
              weights=wtfa_new)
  coeftest(earn.male.young,vcov=vcovHC(earn.male.young,type="HC1"))

  earn.male.old <- update(earn.male.full, formula= . ~ .,
                data=subset(regdata,sex=="1 Male" & age_p>=45 & age_p<=64),
                weights=wtfa_new)
  coeftest(earn.male.old,vcov=vcovHC(earn.male.old,type="HC1"))

  earn.male.single <- lm(ln_earn~ so.homo + so.bi + so.other+
              so.dontknow + so.refuse +  so.missing +
              factor(srvy_yr) + factor(intv_mon)+
              age_p + age2 +
              factor(edu_level) + factor(race) + hisp +
              #factor(rel_stat) +
              d_kid5 + d_kid17 + reg_ne + reg_midwe + reg_so+
              tenure + tenure2 +
              factor(firm_size) + factor(sector)+
              occ + ind+
              as.integer(tcearn_f) + tenure_topcoded,
              data=subset(regdata,sex=="1 Male" & rel_stat=="single"),
              weights=wtfa_new)
    coeftest(earn.male.single,vcov=vcovHC(earn.male.single,type="HC1"))

  earn.male.partner <- update(earn.male.single, formula= . ~ .,
              data=subset(regdata,sex=="1 Male" & rel_stat=="partnered"),
                  weights=wtfa_new)
  coeftest(earn.male.partner,vcov=vcovHC(earn.male.partner,type="HC1"))

  earn.male.lowed <- update(earn.male.full, formula= . ~ .,
              data=subset(regdata, sex=="1 Male" & edu_level%in% c("HS","asso", "less_HS", "some_col")),
              weights=wtfa_new)
  coeftest(earn.male.lowed,vcov=vcovHC(earn.male.lowed,type="HC1"))

  earn.male.highed <- lm(ln_earn~ so.homo + so.bi + so.other+
            so.dontknow + so.refuse +  so.missing +
            factor(srvy_yr) + factor(intv_mon)+
            age_p + age2 +
            #factor(edu_level)
            + factor(race) + hisp +
            factor(rel_stat) + d_kid5 + d_kid17 + reg_ne + reg_midwe + reg_so+
            tenure + tenure2 +
            factor(firm_size) + factor(sector)+
            occ + ind +
            as.integer(tcearn_f) + tenure_topcoded,
            data=subset(regdata, sex=="1 Male" & edu_level=="BA"),
         weights=wtfa_new)
     coeftest(earn.male.highed,vcov=vcovHC(earn.male.highed,type="HC1"))

  earn.male.public <- lm(ln_earn~ so.homo + so.bi + so.other+
            so.dontknow + so.refuse +  so.missing +
            factor(srvy_yr) + factor(intv_mon)+
            age_p + age2 +
            factor(edu_level)
            + factor(race) + hisp +
            factor(rel_stat) + d_kid5 + d_kid17 +
            reg_ne + reg_midwe + reg_so+
            tenure + tenure2 +
            factor(firm_size) +
            # factor(sector)+
            occ + ind +
            as.integer(tcearn_f) + tenure_topcoded,
            data=subset(regdata, sex=="1 Male" & sector=="public"),
         weights=wtfa_new)
  coeftest(earn.male.public,vcov=vcovHC(earn.male.public,type="HC1"))

  earn.male.private <- update(earn.male.public, formula = . ~ . ,
            data=subset(regdata, sex=="1 Male" & sector=="private"),
         weights=wtfa_new)
  coeftest(earn.male.private,vcov=vcovHC(earn.male.private,type="HC1"))

  earn.male.large <- update(earn.male.full, formula= .~.,
            data=subset(regdata,sex=="1 Male" & (firm_size=="7" | firm_size=="8")),
            weights=wtfa_new)
  coeftest(earn.male.large,vcov=vcovHC(earn.male.large,type="HC1"))

  earn.male.small <- update(earn.male.full, formula= .~.,
            data=subset(regdata, sex=="1 Male" & (firm_size!="7" & firm_size!="8")),
            weights=wtfa_new)
  coeftest(earn.male.small,vcov=vcovHC(earn.male.small,type="HC1"))

  ## Excluding occ or ind

    earn.male.noocc <-  lm(ln_earn~ so.homo + so.bi + so.other+
              so.dontknow + so.refuse +  so.missing +
              factor(srvy_yr) + factor(intv_mon)+
              age_p + age2 +
              factor(edu_level)
              + factor(race) + hisp +
              factor(rel_stat) + d_kid5 + d_kid17 +
              reg_ne + reg_midwe + reg_so+
              tenure + tenure2 +
              factor(firm_size) +
              factor(sector)+
              #occ +
              ind +
              as.integer(tcearn_f) + tenure_topcoded,
              data=subset(regdata, sex=="1 Male"),
           weights=wtfa_new)
    coeftest(earn.male.noocc,vcov=vcovHC(earn.male.noocc,type="HC1"))

     earn.male.noind <- lm(ln_earn~ so.homo + so.bi + so.other+
               so.dontknow + so.refuse +  so.missing +
               factor(srvy_yr) + factor(intv_mon)+
               age_p + age2 +
               factor(edu_level)
               + factor(race) + hisp +
               factor(rel_stat) + d_kid5 + d_kid17 +
               reg_ne + reg_midwe + reg_so+
               tenure + tenure2 +
               factor(firm_size) +
               factor(sector)+
               occ +
               #ind +
               as.integer(tcearn_f) + tenure_topcoded,
               data=subset(regdata, sex=="2 Female"),
            weights=wtfa_new)
     coeftest(earn.male.noind,vcov=vcovHC(earn.male.noind,type="HC1"))

  earn.male.noocc.noind <- lm(ln_earn~ so.homo + so.bi + so.other+
            so.dontknow + so.refuse +  so.missing +
            factor(srvy_yr) + factor(intv_mon)+
            age_p + age2 +
            factor(edu_level)
            + factor(race) + hisp +
            factor(rel_stat) + d_kid5 + d_kid17 +
            reg_ne + reg_midwe + reg_so+
            tenure + tenure2 +
            factor(firm_size) +
            factor(sector)+
            #occ +
            #ind +
            as.integer(tcearn_f) + tenure_topcoded,
            data=subset(regdata, sex=="1 Male"),
         weights=wtfa_new)
  coeftest(earn.male.noocc.noind,vcov=vcovHC(earn.male.noocc.noind,type="HC1"))

  ## Model with health variables
  earn.male.health <- lm(ln_earn ~ so.homo + so.bi + so.other+
          so.dontknow + so.refuse +  so.missing +
         factor(srvy_yr) + factor(intv_mon)+
         age_p + age2 +
         factor(edu_level2) + factor(race) + hisp +
         factor(rel_stat) + d_kid5 + d_kid17 + reg_ne + reg_midwe + reg_so+
         tenure + tenure2 + tenure_refused +  tenure_missing + tenure_dontknow +
          factor(firm_size) + factor(sector)+
         factor(occupn2) + factor(indstrn2)+
         as.integer(tcearn_f) + tenure_topcoded +
          factor(chdev) + factor(cnkind7) + factor(dibev) +
         factor(aflhca10) + factor(aflhca17) + factor(aflhca18) + factor(smkev)+
      bmi + factor(aprvtryr) + factor(ahcdlyr5) +
         factor(adnlong2) + factor(ahcsyr1) + factor(ahcsyr8) +
            factor(private) + factor(plnwrkr1),
         data=subset(CE, sex=="1 Male" & age_p>=25 & age_p<=64 & emp_full==1 & is.na(ernyr_i2)=="FALSE" & ernyr_f=="0 Reported"),
         weights=wtfa_new)

    coeftest(earn.male.health,vcov=vcovHC(earn.male.health,type="HC1"))


    earn.fem.health <- update(earn.male.health, formula = . ~ .,
     data=subset(CE, sex=="2 Female" & age_p>=25 & age_p<=64 & emp_full==1 & is.na(ernyr_i2)=="FALSE" & ernyr_f=="0 Reported"),
     weights=wtfa_new)
    coeftest(earn.fem.health,vcov=vcovHC(earn.fem.health,type="HC1"))

## VI - Oaxaca decomposition

  OB <- regdata %>%
      subset(sogi=="male, gay" | sogi=="male, straight")

  OB_mean <- OB %>%
  group_by(so.homo) %>%
  summarise(ln_earn = weighted.mean(ln_earn, wtfa_new), # 10.5 percent gap
            srvy_yr_2014=weighted.mean(srvy_yr_2014, wtfa_new),  #excluded cat is 2013
            srvy_yr_2015=weighted.mean(srvy_yr_2015, wtfa_new),
            intv_mon_2=weighted.mean(intv_mon_2, wtfa_new),  #excluded cat is january
            intv_mon_3=weighted.mean(intv_mon_3, wtfa_new),
            intv_mon_4=weighted.mean(intv_mon_4, wtfa_new),
            intv_mon_5=weighted.mean(intv_mon_5, wtfa_new),
            intv_mon_6=weighted.mean(intv_mon_6, wtfa_new),
            intv_mon_7=weighted.mean(intv_mon_7, wtfa_new),
            intv_mon_8=weighted.mean(intv_mon_8, wtfa_new),
            intv_mon_9=weighted.mean(intv_mon_9, wtfa_new),
            intv_mon_10=weighted.mean(intv_mon_10, wtfa_new),
            intv_mon_11=weighted.mean(intv_mon_11, wtfa_new),
            intv_mon_12=weighted.mean(intv_mon_12, wtfa_new),
            age_p=weighted.mean(age_p, wtfa_new),
            age2=weighted.mean(age2, wtfa_new),
            edu_level_asso=weighted.mean(edu_level_asso, wtfa_new), # exl cat is HS
            edu_level_BA=weighted.mean(edu_level_BA, wtfa_new),
            edu_level_some_col=weighted.mean(edu_level_some_col, wtfa_new),
            edu_level_less_HS=weighted.mean(edu_level_less_HS, wtfa_new),
            race_2=weighted.mean(race_2, wtfa_new), #exc cat is race_1=WHITE
            race_3=weighted.mean(race_3, wtfa_new),
            race_4=weighted.mean(race_4, wtfa_new),
            race_5=weighted.mean(race_5, wtfa_new),
            race_6=weighted.mean(race_6, wtfa_new),
            hisp=weighted.mean(hisp, wtfa_new),
            rel_stat_widowed=weighted.mean(rel_stat_widowed, wtfa_new), #exl cat is single
            rel_stat_divorced=weighted.mean(rel_stat_divorced, wtfa_new),
            rel_stat_separated=weighted.mean(rel_stat_separated, wtfa_new),
            rel_stat_partnered=weighted.mean(rel_stat_partnered, wtfa_new),
            d_kid5=weighted.mean(d_kid5, wtfa_new), # exc no kids
            d_kid17=weighted.mean(d_kid17, wtfa_new),
            reg_ne=weighted.mean(reg_ne, wtfa_new), # exc the West
            reg_midwe=weighted.mean(reg_midwe, wtfa_new),
            reg_so=weighted.mean(reg_so, wtfa_new),
            tenure =weighted.mean(tenure, wtfa_new),
            tenure2=weighted.mean(tenure2, wtfa_new),
            firm_size_2=weighted.mean(firm_size_2, wtfa_new), # exc cat is firm_size_1 which is 1 employee
            firm_size_3=weighted.mean(firm_size_3, wtfa_new),
            firm_size_4=weighted.mean(firm_size_4, wtfa_new),
            firm_size_5=weighted.mean(firm_size_5, wtfa_new),
            firm_size_6=weighted.mean(firm_size_6, wtfa_new),
            firm_size_7=weighted.mean(firm_size_7, wtfa_new),
            firm_size_8=weighted.mean(firm_size_8, wtfa_new),
            sector_public=weighted.mean(sector_public, wtfa_new), #exc cat is private sector
            occ_1=weighted.mean(occ_1, wtfa_new), # exc cat is 13 which is food serving. occ_18 is excluded because no gays
            occ_2=weighted.mean(occ_2, wtfa_new),
            occ_3=weighted.mean(occ_3, wtfa_new),
            occ_4=weighted.mean(occ_4, wtfa_new),
            occ_5=weighted.mean(occ_5, wtfa_new),
            occ_6=weighted.mean(occ_6, wtfa_new),
            occ_7=weighted.mean(occ_7, wtfa_new),
            occ_8=weighted.mean(occ_8, wtfa_new),
            occ_9=weighted.mean(occ_9, wtfa_new),
            occ_10=weighted.mean(occ_10, wtfa_new),
            occ_11=weighted.mean(occ_11, wtfa_new),
            occ_12=weighted.mean(occ_12, wtfa_new),
            #occ_13=weighted.mean(occ_13, wtfa_new),
            occ_14=weighted.mean(occ_14, wtfa_new),
            occ_15=weighted.mean(occ_15, wtfa_new),
            occ_16=weighted.mean(occ_16, wtfa_new),
            occ_17=weighted.mean(occ_17, wtfa_new),
            occ_19=weighted.mean(occ_19, wtfa_new),
            occ_20=weighted.mean(occ_20, wtfa_new),
            occ_21=weighted.mean(occ_21, wtfa_new),
            occ_22=weighted.mean(occ_22, wtfa_new),
            ind_2=weighted.mean(ind_2, wtfa_new), # ref cat is ind_18. Ind_1 is excluded because of no gays
            ind_3=weighted.mean(ind_3, wtfa_new),
            ind_4=weighted.mean(ind_4, wtfa_new),
            ind_5=weighted.mean(ind_5, wtfa_new),
            ind_6=weighted.mean(ind_6, wtfa_new),
            ind_7=weighted.mean(ind_7, wtfa_new),
            ind_8=weighted.mean(ind_8, wtfa_new),
            ind_9=weighted.mean(ind_9, wtfa_new),
            ind_10=weighted.mean(ind_10, wtfa_new),
            ind_11=weighted.mean(ind_11, wtfa_new),
            ind_12=weighted.mean(ind_12, wtfa_new),
            ind_13=weighted.mean(ind_13, wtfa_new),
            ind_14=weighted.mean(ind_14, wtfa_new),
            ind_15=weighted.mean(ind_15, wtfa_new),
            ind_16=weighted.mean(ind_16, wtfa_new),
            ind_17=weighted.mean(ind_17, wtfa_new),
            #ind_18=weighted.mean(ind_18, wtfa_new),
            ind_19=weighted.mean(ind_19, wtfa_new),
            ind_20=weighted.mean(ind_20, wtfa_new),
          )

    table_OBmean <- melt(OB_mean, id="so.homo")

    table_OBmean <- dcast(table_OBmean, variable ~ so.homo, value.var="value")

    table_OBmean <- table_OBmean %>%
        transmute(variable=variable,
                  gay=`1`,
                  straight=`0`,
                  dif=gay-straight)


    ## Run separate regression to obtain coefficients

    # Gay
    earn.gay.demo <- lm(ln_earn ~ srvy_yr_2014 + srvy_yr_2015 +
                                  intv_mon_2  +
                                  intv_mon_3 +
                                  intv_mon_4 +
                                  intv_mon_5 +
                                  intv_mon_6 +
                                  intv_mon_7 +
                                  intv_mon_8 +
                                  intv_mon_9 +
                                  intv_mon_10 +
                                  intv_mon_11 +
                                  intv_mon_12 +
                                  age_p +
                                  age2 +
                                  edu_level_asso +
                                  edu_level_BA +
                                  edu_level_some_col +
                                  edu_level_less_HS +
                                  race_2 +
                                  race_3 +
                                  race_4 +
                                  race_5 +
                                  race_6 +
                                  hisp +
                                  rel_stat_widowed +
                                  rel_stat_divorced +
                                  rel_stat_separated +
                                  rel_stat_partnered +
                                  d_kid5 +
                                  d_kid17 +
                                  reg_ne +
                                  reg_midwe +
                                  reg_so+
                                  tenure + tenure2+
                                  firm_size_2+
                                  firm_size_3+
                                  firm_size_4+
                                  firm_size_5+
                                  firm_size_6+
                                  firm_size_7+
                                  firm_size_8+
                                  sector_public+
                                  occ_1+
                                  occ_2+
                                  occ_3+
                                  occ_4+
                                  occ_5+
                                  occ_6+
                                  occ_7+
                                  occ_8+
                                  occ_9+
                                  occ_10+
                                  occ_11+
                                  occ_12+
                                  occ_14+
                                  occ_15+
                                  occ_16+
                                  occ_17+
                                  occ_19+
                                  occ_20+
                                  occ_21+
                                  occ_22+
                                  ind_2+
                                  ind_3+
                                  ind_4+
                                  ind_5+
                                  ind_6+
                                  ind_7+
                                  ind_8+
                                  ind_9+
                                  ind_10+
                                  ind_11+
                                  ind_12+
                                  ind_13+
                                  ind_14+
                                  ind_15+
                                  ind_16+
                                  ind_17+
                                  ind_19+
                                  ind_20,
                        data=subset(OB, so.homo==1),
                        weights=wtfa_new)
  summary(earn.gay.demo)
  coeftest(earn.gay.demo,vcov=vcovHC(earn.gay.demo,type="HC1"))

  coef_gay.demo<- as_tibble(coef(summary(earn.gay.demo))[,1], rownames = NA)
  coef_gay.demo <- coef_gay.demo %>%
      rownames_to_column(var="variable") %>%
      rename("beta_gay" = "value")

  earn.straight.demo <- update(earn.gay.demo, formula = . ~ .,
  data=subset(OB, so.homo==0),  weights=wtfa_new)
  summary(earn.straight.demo)
  coeftest(earn.straight.demo,vcov=vcovHC(earn.straight.demo,type="HC1"))

  coef_straight.demo<- as_tibble(coef(summary(earn.straight.demo))[,1], rownames = NA)
  coef_straight.demo <- coef_straight.demo %>%
      rownames_to_column(var="variable") %>%
      rename("beta_straight" = "value")

  ## Putting table together
    table_OB <- table_OBmean %>%
      full_join(coef_gay.demo, by="variable") %>%
      left_join(coef_straight.demo, by="variable") %>%
      mutate(dif_beta = beta_gay - beta_straight,
            delta_char = beta_gay * dif , # evaluated at the gay returns, this component show difference in characteristic between gay and straight
            delta_return = straight * dif_beta) %>% # if gay men has the same characteristic as straight men, so this component speaks to difference in return. Positive value means gay premium
    mutate(delta_return = ifelse(variable=="(Intercept)", dif_beta, delta_return))

    sum <- table_OB %>%
        summarise(delta_char = sum(delta_char, na.rm=TRUE),
                  delta_return = sum(delta_return, na.rm=TRUE),
                  variable = "Total",
                  gay = NA,
                  straight = NA,
                  dif=NA,
                  beta_gay=NA,
                  beta_straight=NA,
                  dif_beta = NA
                )
    table_OB <- rbind(table_OB, sum)

    check <- table_OB %>%
            mutate(sum=delta_char + delta_return)

    ## Subsumed OB table
    table_OB_subsume <- table_OB %>%
      subset(variable!="ln_earn") %>%
      mutate( category =case_when(
              # str_detect(variable, "ln_earn") ~ "ln_earn",
              str_detect(variable, "srvy") ~ "survey",
              str_detect(variable, "intv") ~ "survey",
              str_detect(variable, "age") ~ "age",
              str_detect(variable, "edu_") ~ "education",
              str_detect(variable, "race_") ~ "race_ethnic",
              str_detect(variable, "hisp") ~ "race_ethnic",
              str_detect(variable, "rel_stat") ~ "relationship",
              str_detect(variable, "d_kid") ~ "children",
              str_detect(variable, "reg") ~ "geo_region",
              str_detect(variable, "tenure") ~ "tenure",
              str_detect(variable, "firm_size") ~ "firm_size",
              str_detect(variable, "sector") ~ "sector",
              str_detect(variable, "occ_") ~ "occupation",
              str_detect(variable, "ind_") ~ "industry",
              str_detect(variable, "Inter") ~ "intercept",
              str_detect(variable, "Total") ~ "Total",
              ),
            category = factor(category, levels=c("age", "race_ethnic", "education",
              "relationship","children","geo_region","tenure","sector", "firm_size",
              "industry", "occupation","survey","intercept", "Total"))
          )  %>%
        group_by(category) %>%
        summarise(delta_char=sum(delta_char, na.rm=TRUE),
                  delta_return=sum(delta_return, na.rm=TRUE))%>%
        mutate(pc_char = delta_char/0.0666 *100,
              pc_return = delta_return/0.0666*100)

## VII -  Analysis of occupation and industry

    xtabs(~so.homo, data = subset (regdata, (sogi=="male, gay" | sogi=="male, straight")))
    N_gay <- 430
    N_het <- 17357

    ind.gay <- regdata %>%
      subset(sogi=="male, gay" | sogi=="male, straight")%>%
      mutate(ind_name = case_when(
                indstrn2 ==1 ~ "Agriculture, Forestry, Fishing, Hunting",
                indstrn2 ==2 ~ "Mining",
                indstrn2 ==3 ~ "Utilities",
                indstrn2 ==4 ~ "Construction",
                indstrn2 ==5 ~ "Manufacturing",
                indstrn2 ==6 ~ "Wholesale trade",
                indstrn2 ==7 ~ "Retail trade",
                indstrn2 ==8 ~ "Transportation & warehousing",
                indstrn2 ==9 ~ "Information",
                indstrn2 ==10 ~"Finance & insurance",
                indstrn2 ==11 ~"Real estate",
                indstrn2 ==12 ~"Professional & technical services",
                indstrn2 ==13 ~ "Management of enterprises",
                indstrn2 ==14 ~ "Administrative & waste management",
                indstrn2 ==15 ~ "Educational services",
                indstrn2 ==16 ~ "Healthcare & social assistance",
                indstrn2 ==17 ~ "Art, entertainment, recreation",
                indstrn2 ==18 ~ "Accommodation & food services",
                indstrn2 ==19 ~ "Other services",
                indstrn2 ==20 ~ "Public administration",
                indstrn2 ==21 ~ "Military",
                indstrn2 ==97 ~ "Refused, classified",
                indstrn2 ==98 ~ "Not ascertained",
                indstrn2 ==99 ~ "Don't know"
                            ))

  ind.gay.distr <- ind.gay %>%
        group_by(so.homo, indstrn2, ind_name) %>%
        summarise(count = n(),
                  earn_avg = weighted.mean(ernyr_i2, wtfa_new))

    tab.ind <- dcast(setDT(ind.gay.distr), indstrn2 + ind_name ~ so.homo, value.var = c("count", "earn_avg"))
    tab.ind <- tab.ind %>%
          mutate(share_0 = count_0/N_het*100,
                  share_1 = count_1/N_gay*100,
                  earn_gap = earn_avg_1 - earn_avg_0)%>%
          arrange(-earn_gap)%>%
          select(indstrn2, ind_name, count_1, share_1, count_0, share_0, earn_avg_1, earn_avg_0, earn_gap)

    ks.test(tab.ind$count_1,tab.ind$count_0)
    # REJECTING THE NULL THAT gay and straights are drawn from the same continuous distribution, i.e. that gay and straight industrial sorting are not identical

    (  plot_ind <- ggplot(tab.ind) +
    geom_line(mapping =aes(y=share_1, x=reorder(ind_name, earn_gap), group=1, color="Gay men"),size=1) +
    geom_line(mapping =aes(y=share_0, x=reorder(ind_name, earn_gap), group=1, color="Straight men"), size=1) +
    geom_vline(xintercept = 11, linetype="dotted", color = "red", size=1.5) +
    coord_flip() +
    labs(y="% of workers", x="", color="",caption="Industry ordered by increasing gay earnings premium") +
    theme(legend.position="right", plot.caption = element_text(hjust = 0)) +
    ggtitle("Figure XX - Distribution of workers by industry")+
    scale_color_viridis(option="viridis",begin=0.1, end=0.85, direction = -1, discrete=TRUE)
    )

    occ.gay <- regdata %>%
        subset(sogi=="male, gay" | sogi=="male, straight")%>%
        mutate(occ_name = case_when(
                occupn2 ==1 ~ "Management",
                occupn2 ==2 ~ "Business & finance",
                occupn2 ==3 ~ "Computer & math",
                occupn2 ==4 ~ "Architecture & engineering",
                occupn2 ==5 ~ "Life, physical, social sciences",
                occupn2 ==6 ~ "Community & social services",
                occupn2 ==7 ~ "Legal",
                occupn2 ==8 ~ "Education, training, library",
                occupn2 ==9 ~ "Art,design, entertainment, sport, media",
                occupn2 ==10 ~"Healthcare",
                occupn2 ==11 ~"Healthcare support",
                occupn2 ==12 ~"Protection services",
                occupn2 ==13 ~ "Food prep & serving",
                occupn2 ==14 ~ "Building cleaning & maintenance",
                occupn2 ==15 ~ "Personal care & service",
                occupn2 ==16 ~ "Sales & related",
                occupn2 ==17 ~ "Office & adminstrative support",
                occupn2 ==18 ~ "Farming, fishing, forestry",
                occupn2 ==19 ~ "Construction & extraction",
                occupn2 ==20 ~ "Installation, maintenance, repair",
                occupn2 ==21 ~ "Production",
                occupn2 ==22 ~ "Transportation & material moving"
                  ))

    occ.gay.distr <- occ.gay %>%
          group_by(so.homo, occupn2, occ_name) %>%
          summarise(count = n(),
                    earn_avg = weighted.mean(ernyr_i2, wtfa_new))

      tab.occ <- dcast(setDT(occ.gay.distr), occupn2 + occ_name ~ so.homo, value.var = c("count", "earn_avg"))
      tab.occ <- tab.occ %>%
            mutate(share_0 = count_0/N_het*100,
                    share_1 = count_1/N_gay*100,
                    earn_gap = earn_avg_1 - earn_avg_0)%>%
            arrange(-earn_gap)%>%
            select(occupn2, occ_name, count_1, share_1, count_0, share_0, earn_avg_1, earn_avg_0, earn_gap)

      ks.test(tab.occ$count_1,tab.occ$count_0)

      (  plot_occ <- ggplot(tab.occ) +
          geom_line(mapping =aes(y=share_1, x=reorder(occ_name, earn_gap), group=1, color="Gay men"), size=1) +
          geom_line(mapping =aes(y=share_0, x=reorder(occ_name, earn_gap), group=1, color="Straight men"), size=1) +
          geom_vline(xintercept = 12, linetype="dotted", color = "red", size=1.5) +
          coord_flip() +
          labs(y="% of workers", x="", color="",size="",caption="Occupations ordered by increasing gay earnings premium") +
          theme(legend.position="right", plot.caption = element_text(hjust = 0)) +
          ggtitle("Figure 2 - Distribution of workers by occupation")+
          scale_color_viridis(option="viridis",begin=0.1, end=0.85, direction = -1, discrete=TRUE)
      )

  # Regression, with Restrict to industry and occupation for which more than 30 gay men
  regdata2 <- subset(regdata, (indstrn2 %in% c("18","5","15","16","12","7") |
                                occupn2  %in% c("2","17","16","1")))
  xtabs(~sogi, data=regdata2) # gay = 341
  summarise(subset(regdata2, sex=="1 Male"), n()) #11894

  # baseline model
  earn.male.base <- lm(ln_earn~ so.homo + so.bi + so.other+
            so.dontknow + so.refuse +  so.missing +
            factor(srvy_yr) + factor(intv_mon),
            data=subset(regdata2, sex=="1 Male"),
            weights=wtfa_new)
  coeftest(earn.male.base,vcov=vcovHC(earn.male.base,type="HC1"))

  # demographics
  earn.male.demo <- update(earn.male.base, formula= . ~ . +
            age_p + age2 +
            factor(edu_level) + factor(race) + hisp +
            factor(rel_stat) + d_kid5 + d_kid17 + reg_ne + reg_midwe + reg_so
          )

  coeftest(earn.male.demo,vcov=vcovHC(earn.male.demo,type="HC1"))

  # No ind, no occ
  earn.male.nooccind <- update(earn.male.demo, formula= . ~ . +
            tenure + tenure2 +
            factor(firm_size) + factor(sector)+
            as.integer(tcearn_f) + tenure_topcoded
            )

  coeftest(earn.male.nooccind,vcov=vcovHC(earn.male.nooccind,type="HC1"))

  ## CE Full model
  earn.male.full <- update(earn.male.nooccind, formula= . ~ . +
            occ + ind
            )
  coeftest(earn.male.full,vcov=vcovHC(earn.male.full,type="HC1"))
