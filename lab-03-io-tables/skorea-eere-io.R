library(here)
library(tidyverse)
library(readxl)
library(reshape2)
options(width=200,scipen=1000)

rm(list=ls())

## Use the domestic coefficients!!!
## Asheet  <-  read_xlsx('Input-Output tables_Producers price--Detailed Table--08202021.xlsx',sheet='Input coefficients', range="A6:FK171") %>% rename(Code="...1", Industry="...2")
Asheet  <-  read_xlsx(here('lab-03-io-tables','Input-Output tables_Producers price--Detailed Table--08202021.xlsx'),sheet='Input coefficients(dom)', range="A6:FK171") %>% rename(Code="...1", Industry="...2")
A  <- data.matrix(select(Asheet,-c(1,2)))
A  <- as.matrix(select(Asheet,-c(1,2)))
rownames(A)  <- unlist(select(Asheet, Industry))

leontief  <- solve(diag(dim(A)[1]) - A)

Esheet  <-  read_xlsx(here('lab-03-io-tables','I-O Analysis for Tutorial--02152023--SC.xlsx'), 'E-O Ratio(commodities)', range="A4:L169") %>% select(Code="Commodity...1", Industry, "E/O Ratio" )
EO  <- data.matrix(select(Esheet,3))

## Multiply leontief rowise by EO to leontief employment matrix
leontief.employment  <- sweep(leontief, 1, EO, "*")

## Table A1.1 is incomplete!  Use the code below instead
## Read Table A1.1 and make a concordance from EERE Sectors to the full list of Industries
## Note: I hand edited table A1.1 so that the Industry names match exactly
## a11  <- read_delim("table_a11.csv", col_types=c("ccn")) %>% mutate(Weights=Weights/100)
## a11  <- dcast(a11, Industry ~ Sector) %>% replace(is.na(.), 0)
## M <- full_join( select(Asheet,Industry), a11) %>% replace(is.na(.), 0)
Msheet  <-  read_xlsx(here('lab-03-io-tables','I-O Analysis for Tutorial--02152023--SC.xlsx'), 'New Program CC (USD)', range="B4:FK26") %>%
    rename(Sector="...1") %>%
    slice(5:21) %>%
    melt(variable.name="Industry") %>%
    mutate(
        Sector = ifelse(Sector %in% c("Weatherization", "Industrial Energy efficiency", "Smart grids", "Public transport", "Electric Vehicles", "Gasoline Motor Vehicles"), paste0("E: ",Sector),
                 ifelse(Sector %in% c("Sustainable Agriculture", "Afforestation"), paste0("Z: ", Sector),
                        paste0("R: ",Sector) ) )) %>%
    dcast( Industry ~ Sector) %>%
    replace(is.na(.), 0)
M <- data.matrix(Msheet[,-1])
rownames(M)  <- unlist(select(Asheet, Industry))

## Checks of industry-sector weights
colSums(M)
N  <- cbind(M, all=rowSums(M))
N[N[,"all"]!=0  ,]
as.matrix(N[N[,"E: Industrial Energy efficiency"]!=0 | N[,"E: Weatherization"]!=0, c("E: Weatherization","E: Industrial Energy efficiency") ])
as.matrix(N[N[,"E: Industrial Energy efficiency"]!=0, "E: Industrial Energy efficiency" ])


## y_eere is a generic EERE purchasing program in MILLION KRW
## Always reset it to zero when done
y_eere  <- matrix(0, nrow = ncol(M), ncol = 1)
rownames(y_eere)  <- colnames(M)

## KRW converted to USD at c. 1100 and back at c. 1200
## Then there is an additional difference between the worksheets and the report
## (KRW.adjustment  <- 1)
## (KRW.adjustment  <- 1200/1100 * 0.847)
(KRW.adjustment  <- 0.931)


employment_estimate  <- function() {
    y_eere  <- y_eere * KRW.adjustment
    x  <- (leontief %*% M %*% y_eere)
    emp_total  <- EO * x
    emp_direct <- EO * (M %*% y_eere)
    print(paste0("  Total employment:      ", sum(emp_total)))
    print(paste0("    Direct employment:   ", sum(emp_direct)))
    print(paste0("    Indirect employment: ", sum(emp_total) - sum(emp_direct)))
}


## Results are identical
## alt_employment_estimate  <- function() {
##     emp_total  <- (leontief.employment %*% M %*% y_eere)
##     emp_direct <- EO * (M %*% y_eere)
##     print(paste0("  Total employment:      ", sum(emp_total)))
##     print(paste0("    Direct employment:   ", sum(emp_direct)))
##     print(paste0("    Indirect employment: ", sum(emp_total) - sum(emp_direct)))
## }


## Tables 3.1 and 3.3 (1 Billion KRW)
for( eere_sector in colnames(M) ) {
    print(paste0(eere_sector, " Employment per 1B KRW"))
    y_eere[,]  <- 0
    y_eere[eere_sector,]  <- 1000
    employment_estimate()
    }


## South Korea EERE Plan (in trillion of KRW)
SK_eere_plan  <- matrix(0, nrow = ncol(M), ncol = 1)
rownames(SK_eere_plan)  <- colnames(M)
SK_eere_plan["E: Weatherization",]                               =  2.8
SK_eere_plan["E: Industrial Energy efficiency",]                 =  2.8
SK_eere_plan["E: Smart grids",]                                  =  2.8
SK_eere_plan["E: Public transport",]                             =  2.8
SK_eere_plan["E: Electric Vehicles",]                            =  2.8
SK_eere_plan["R: Onshore Community solar – distributed energy",] = 22.4
SK_eere_plan["R: Offshore utility-scale solar",]                 =  3.2
SK_eere_plan["R: Onshore Utility Scale Solar",]                  = 16.0
SK_eere_plan["R: Bioenergy",]                                    =  0.8
SK_eere_plan["R: Geothermal",]                                   =  0.8
SK_eere_plan["R: Hydro",]                                        =  0.8
SK_eere_plan["R: Wind-Offshore",]                                =  9.0
SK_eere_plan["R: Wind-Onshore",]                                 = 10.2
SK_eere_plan["R: Tidal Power",]                                  =  0.8


## Tables 3.2 and 3.4 (per program, by Sector)
for( eere_sector in colnames(M) ) {
    y_eere[,]  <- 0
    y_eere[eere_sector,]  <- SK_eere_plan[eere_sector,] * 1000000
    print(paste0("Employment from spending ", y_eere[eere_sector,], " million KRW on ", eere_sector))
    employment_estimate()
    }

## Energy Efficiency Total
y_eere[,]  <- 0
y_eere[substr(rownames(y_eere),1,2)=="E:",]  <- SK_eere_plan[substr(rownames(y_eere),1,2)=="E:",] * 1000000
y_eere
employment_estimate()
y_eere[,]  <- 0


## Renewable Total
y_eere[,]  <- 0
y_eere[substr(rownames(y_eere),1,2)=="R:",]  <- SK_eere_plan[substr(rownames(y_eere),1,2)=="R:",] * 1000000
y_eere
employment_estimate()
y_eere[,]  <- 0

