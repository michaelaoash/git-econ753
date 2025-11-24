library(here)
library(tidyverse)
library(janitor)
options(dplyr.width=Inf, dplyr.print_max=Inf, scipen=10000)
library(readxl)
options(width=200)


## APEEP marginal damage values ($/ton)  [Note 1.10231 tons/mt not needed here]
M2014  <- read_csv(here("egrid2020", "md_M_2014_DR-Krewski_VRMR-9186210.csv"), 
                   col_names = c("NH3_M_2014", "NOx_M_2014", "PM25_M_2014", "SO2_M_2014", "VOC_M_2014"))
md2011  <- read_xlsx(here("egrid2020", "md_2011.xlsx"),sheet=2, col_types=c("text","guess","guess","guess","guess","guess"))
apeep  <- bind_cols(md2011,M2014)
apeep <- apeep %>% mutate(
                       FIPS = str_pad(as.character(fips),width=5,pad="0")
                       )

excel_sheets(here("egrid2020", "egrid2020_data.xlsx"))

(nms <- names(read_excel(here("egrid2020", "egrid2020_data.xlsx"), sheet=4, skip=1, n_max = 0)))
(ct <- ifelse(grepl("^ORISPL", nms), "text", "guess"))

plnt20  <- read_xlsx(here("egrid2020","egrid2020_data.xlsx"), sheet=4, skip=1, col_types = ct )

dictionary.plnt20  <- read_xlsx(here("egrid2020","egrid2020_data.xlsx"), sheet=4, n_max=1)

(dictionary.plnt20  <- data.frame(variable=colnames(plnt20),
                                  description=colnames(dictionary.plnt20)))

plnt20 <- plnt20 %>%
    mutate(FIPS = paste0(FIPSST, FIPSCNTY))

summary(plnt20$NAMEPCAP)
summary(plnt20$PLNGENAN)
summary(plnt20$PLCO2EQA)
summary(plnt20$PLSO2AN)

## (nms <- names(read_excel("egrid2020_data.xlsx", sheet=2, skip=1, n_max = 0)))
## (ct <- ifelse(grepl("^ORISPL", nms), "text", "guess"))
## unt20  <- read_xlsx("egrid2020_data.xlsx", sheet=2, skip=1, col_types = ct )
## dictionary.unt20  <- read_xlsx("egrid2020_data.xlsx", sheet=2, n_max=1)
## (dictionary.unt20  <- data.frame(variable=colnames(unt20),
##                                         description=colnames(dictionary.unt20)))

(nms <- names(read_excel(here("egrid2020","eGRID2020 DRAFT PM Emissions.xlsx"), sheet="2020 PM Plant-level Data", skip=1, n_max = 0)))
(ct <- ifelse(grepl("^ORISPL", nms), "text", "guess"))
egrid_pm25_2020  <- read_xlsx(here("egrid2020", "eGRID2020 DRAFT PM Emissions.xlsx"), sheet="2020 PM Plant-level Data", skip=1, col_types = ct )
egrid_pm25_2020  <- egrid_pm25_2020 %>% transmute(ORISPL, PLPM25AN_tons = ifelse(is.na(PLPM25AN), 0, PLPM25AN))
plnt20 <- left_join(plnt20, egrid_pm25_2020, by=c("ORISPL"="ORISPL"))


plnt20 <- left_join(plnt20, apeep, by=c("FIPS" = "FIPS")) %>%
    mutate(
        PM2.5_apeep = (PLPM25AN_tons * PM25_M_2014),
        SOX_apeep   = (PLSO2AN * SO2_M_2014),
        NOx_apeep   = (PLNOXAN * NOx_M_2014),
        apeep = PM2.5_apeep + SOX_apeep + NOx_apeep
    )




plnt20 %>%
    filter(PLNGENAN>0) %>%
    group_by(PLFUELCT) %>%
    summarize(
        NAMEPCAP = sum(NAMEPCAP, na.rm=TRUE),
        PLNGENAN = sum(PLNGENAN, na.rm=TRUE),
        PLSO2AN = sum(PLSO2AN, na.rm=TRUE),
        PLNOXAN = sum(PLNOXAN, na.rm=TRUE),
        PLPM25AN = sum(PLPM25AN_tons, na.rm=TRUE),
        PLCO2EQA = sum(PLCO2EQA, na.rm=TRUE),
        apeep = sum(apeep, na.rm=TRUE)) %>%
    transmute(PLFUELCT,
              NAMEPCAP / sum(NAMEPCAP),
              PLNGENAN / sum(PLNGENAN),
              PLSO2AN / sum(PLSO2AN),
              PLNOXAN / sum(PLNOXAN),
              PLPM25AN / sum(PLPM25AN),
              PLCO2EQA / sum(PLCO2EQA),
              apeep / sum(apeep)
              )


## Look for university generation
## Per Reuters inquiry, Michael Pell and Tim McLaughlin

plnt20 %>%
    filter(grepl("univ|college|institute|polytech",PNAME,ignore.case=TRUE) |
           grepl("univ|college|institute|polytech",UTLSRVNM,ignore.case=TRUE)) %>%
    select(PNAME,UTLSRVNM,OPRNAME,SECTOR,PSTATABB,PLFUELCT,NAMEPCAP,PLCO2EQA,PLCLPR,PLOLPR,PLGSPR)





## Now focus on Massachusetts facilities

ma <- plnt20 %>%
    filter(PSTATABB=="MA")

ma %>%
    filter(PLNGENAN>0) %>%
    group_by(PLFUELCT) %>%
    summarize(
        NAMEPCAP = sum(NAMEPCAP, na.rm=TRUE),
        PLNGENAN = sum(PLNGENAN, na.rm=TRUE),
        PLSO2AN = sum(PLSO2AN, na.rm=TRUE),
        PLNOXAN = sum(PLNOXAN, na.rm=TRUE),
        PLPM25AN = sum(PLPM25AN_tons, na.rm=TRUE),
        PLCO2EQA = sum(PLCO2EQA, na.rm=TRUE)) %>%
    transmute(PLFUELCT,
              NAMEPCAP / sum(NAMEPCAP),
              PLNGENAN / sum(PLNGENAN),
              PLSO2AN / sum(PLSO2AN),
              PLNOXAN / sum(PLNOXAN),
              PLPM25AN / sum(PLPM25AN),
              PLCO2EQA / sum(PLCO2EQA)
              )

fossil_ma <- plnt20 %>%
    filter(PSTATABB=="MA", PLFUELCT %in% c("COAL","GAS","OIL")) %>%
    transmute(ORISPL, PNAME, LAT, LON, PLFUELCT, CNTYNAME, FIPS, FIPSST, FIPSCNTY, NAMEPCAP, CAPFAC, PLNGENAN, PLCO2EQA, PLNOXAN, PLSO2AN, PLPM25AN=PLPM25AN_tons, apeep)


saveRDS(fossil_ma, file=here("egrid2020","fossil-ma-egrid-2020.RDS"))
