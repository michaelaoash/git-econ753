library(readxl)
library(tidyverse)
library(gridExtra)

## Importing and formatting 2009 IO Table 

IOTable2009 <- read_excel("C:/users/rchin/git-econ753/lab-03-io-tables/IND_NIOT_row_09132019.xlsx", 
                          sheet = "2009",
                          skip = 5)

## Need to include imports with domestic IO matrix (below is only domestic IOTable2009D)

IOTable2009B <- IOTable2009[ ,-(1:4)]

## domestic IO table

domesticIOTable2009C <- IOTable2009B[(1:35),(1:35)]

domesticIOTable2009 <- data.matrix(domesticIOTable2009C)


scale_vector <- IOTable2009[78,5:39] ## This is a row vector of final outputs for each industry, which I'll use to create the A-matrix


domesticAmatrix <- scale(domesticIOTable2009,  center = FALSE, scale=scale_vector)


## imports IO table

importIOTable2009C <- IOTable2009B[(36:70),(1:35)]

importIOTable2009 <- data.matrix(importIOTable2009C)

importAmatrix <- scale(importIOTable2009,  center = FALSE, scale=scale_vector)


## Construct Amatrix, IMINUSAmatrix

Amatrix <- (domesticAmatrix + importAmatrix)

## solve for inverse IMINUSAINVERSE

IMINUSAmatrix <- diag(nrow=35) - Amatrix
################
IMINUSAINVERSE <- solve(IMINUSAmatrix) ## error here ## 
################

## Importing sectoral weights breakdown from excel
GreenEnergyProgram <- read_excel("India-Input-Output Analysis--Employment Estimates--09132019.xlsx",
                                 sheet = "Green Energy Program")

## cutting up weights data-frame and turning it into a weights matrix
## Get the right rows and columns
weights2 <- GreenEnergyProgram[(10:20),(4:38)] 
## Convert strings to number
weightsmat <- data.matrix(weights2)
## Convert NA to 0
weightsmat[is.na(weightsmat)]=0



## Why this line?
## colnames(weightsmat) <- NULL

weightsmat_converted  <- matrix(nrow=nrow(weightsmat),ncol=0)

for(i in 1:35){
    ## Convert character strings to numeric
    this_column  <- as.numeric(weightsmat[,i])
     ## Convert NA to 0
    this_column[is.na(this_column)] <- 0
    print(this_column)
    weightsmat_converted  <- cbind(weightsmat_converted, this_column)
}

View(weightsmat_converted)

## renaming columns of GreenEnergyProgram table & using them to name the columns of the weights matrix

colnames(GreenEnergyProgram) <- GreenEnergyProgram[1,1:38]
colnames(GreenEnergyProgram)
colnames(weightsmat) <- colnames(GreenEnergyProgram)[4:38]
colnames(weightsmat)

## importing employment data from excel

rawEmploymentA <- read_excel("C:/users/rchin/git-econ753/lab-03-io-tables/India-Input-Output Analysis--Employment Estimates--09132019.xlsx",
                             sheet = "EO Matrix")

rawEmploymentB <- rawEmploymentA[,-(4:7)]
rawEmploymentC <- rawEmploymentB[,-2]
rawEmployment <- rawEmploymentC[,-(5)]

totalEmployment <- as.matrix(rawEmployment[c(2:36),2])

## output vector

output <- t(scale_vector)

## EO vector
EO <- matrix(nrow=35, ncol=1)
i <- 1
for(val in totalEmployment)
{
  EO[i] <- val / output[i]
  i <- i + 1
} 

## Employment matrix
  
employmentMatrix <- matrix(nrow=35, ncol=35)
i <- 1
for(val in EO)
{
  employmentMatrix[i,1:35] <- IMINUSAINVERSE[i,1:35] * val
  i <- i + 1
}

## Total employment numbers
employmentFigures <- matrix(nrow=3, ncol=35)
colnames(employmentFigures) <- colnames(IOTable2009)[2:36]
rownames(employmentFigures) <- c("Total Employment", "Direct Employment", "Indirect Employment")
employmentFigures[1,1:35] <- colSums(employmentMatrix)
employmentFigures[2,1:35] <- t(diag(employmentMatrix))
employmentFigures[3,1:35] <- employmentFigures[1,1:35]-employmentFigures[2,1:35]

employmentFigures <- as.matrix(employmentFigures)



employmentBySector <- employmentFigures %*% t(weightsmat)

view(employmentBySector)

