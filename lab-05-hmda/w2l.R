library(reshape2)
library(dplyr)
options(scipen=10000,width=120)
library(stringr)
library(tidyverse)


(firstex.0 <- read.table(head=TRUE,sep=",",text="
Country,inc04,inc05,inc06,cons04,cons05,cons06,inv04,inv05,inv06
US,95,100,100,95,100,100,60,80,75
FR,80,90,100,85,90,100,55,45,45"))

mutate(gather(firstex.0, variable, value,  -Country),
                    year = parse_number(variable),
                    var  = parse_character(variable)
       )

firstex.1 <- melt(firstex.0,id="Country")


firstex.2 <- mutate(firstex.1, 
                    year = str_extract(variable, "[0-9]+"),
                    var  = str_extract(variable, "[A-z]+")
                    )

(firstex.3  <- dcast(firstex.2, Country + year ~ var))





(secondex.0 <- read.table(head=TRUE,sep=",",text="
Subject,CircleBlue,CircleRed,CircleGreen,SquareBlue,SquareRed,SquareGreen,TriangleBlue,TriangleRed,TriangleGreen
101,95,100,100,95,100,100,60,80,75
102,80,90,100,85,90,100,55,45,45"))

(secondex.1 <- melt(secondex.0,id="Subject"))

## Insert a ":" in variable (values of form ShapeColor) so it's Shape:Color and then split into shape and color on the :
(secondex.2 <- mutate(secondex.1, 
  shape.color = gsub("(.*[a-z])([A-Z].*)","\\1:\\2", variable ),
  color = sapply (strsplit(shape.color, ":"), "[", 2),
  shape = sapply (strsplit(shape.color, ":"), "[", 1)
))

## Get rid of the intermediate variables for neatness
(secondex.3 <- subset(secondex.2,select=c(-variable,-shape.color) ))

secondex.3  <- secondex.2[ , c(1,3,5:6)  ]

## Melt again 
(secondex.4 <- melt(secondex.3,id=c("Subject","shape","color"),measurable="value"))

## dcast in alternative variables x 
dcast(secondex.4, Subject + shape ~ variable + color)
dcast(secondex.4, Subject + color ~ variable + shape)

## Run a regression with the fully melted data
summary(lm( value ~ factor(Subject) + factor(shape) + factor(color),   data=secondex.4))



## Long to wide
## Observations within groups
(dataset <- data.frame(x = c("a", "a", "a", "b", "b", "b", "b", "c", "c") ) ) 
dataset$random.num <- as.character(runif(length(dataset$x)))
dataset

(test  <- mutate( group_by(dataset,x) , obsnum = row_number()))
(test2 <- melt(test, id=c("x","obsnum"), measurable="random.num"))
dcast( test2, x ~  variable + obsnum)

