library(ggplot2)
library(sciplot)
library(mosaic)
library(tableone)
library(survival)
library(car)
library(gridExtra)
library(R.utils)
library(foreign)
library(lattice)
library(lme4)
library(MASS)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(rgl)
library(multcomp)
library(R.utils)
library(matlab)
install.packages('caret')
library(caret)

#This is the anaysis for the SAVE OR data 

#read in data 

save = read.csv('SaveTotal.csv')

head(save)

count(save)


#only include patients fROM the OR  

saveor = subset(save, Area == 'OR')

count(saveor)

#exclude patients with missing data 

saveor2 = subset(saveor, Total.Score != 'NA')

saveor3 = subset(saveor2, Grade != 'NA')

count(saveor3)

View(saveor3)

#subset patients into difficult and easy intubation groups 
difficult= subset(saveor3, Grade >="3")
difficult


difficult = subset(difficult, Total.Score != '0')


easy= subset(saveor3, Grade < "3")
easy



difficult$Grade
easy$Grade

count(difficult)
count(easy)

view(easy)
#get rid of useless columns 
saveor3$X = NULL
saveor3$X.1 = NULL
saveor3$X.2 = NULL

#Primary analysis of score 
count(easy, Total.Score<5)
count(difficult, Total.Score>=5)

difficult$Total.Score

count(easy)
count(difficult)

difficult$Total.Score

brave$Disease.associated.w..difficult.airway
easy$Disease.associated.w..difficult.airway

View(difficult)

amatrix <- matrix(c(319,5,17,4),ncol=2,byrow=TRUE,
                  dimnames = list(c("BRAVE Score < 5", "BRAVE Score >=5"), 
                                  c("Easy Intubation Group", "Difficult Intubation Group")))

fisher.test(amatrix)
chisq.test(amatrix)


#mutlivariable analysis 

HODIF <- matrix(c(69,10,0,0),ncol=2,byrow=TRUE,
                dimnames = list(c("BRAVE Score < 5", "BRAVE Score >=5"), 
                                c("Easy Intubation Group", "Difficult Intubation Group")))

fisher.test(HODIF)

count(easy$Disease.associated.w..difficult.airway)
count(difficult$Disease.associated.w..difficult.airway)


DISEASAS <- matrix(c(68,10,1,0),ncol=2,byrow=TRUE, 
                   dimnames = list(c("BRAVE Score < 5", "BRAVE Score >=5"),
                                   c("Easy Intubation Group", "Difficult Intubation Group")))

fisher.test(DISEASAS)



count(easy$Mallampati)
count(difficult$Mallampati)

MALLA <- matrix(c(57,4,12,5),ncol=2,byrow=TRUE,
                dimnames = list(c("BRAVE Score < 5", "BRAVE Score >=5"), 
                                c("Easy Intubation Group", "Difficult Intubation Group")))

fisher.test(MALLA)



count(easy$Thyro.mental.distance)
count(difficult$Thyro.mental.distance)

THYROMEN <- matrix(c(63,3,6,6),ncol=2,byrow=TRUE,
                   dimnames = list(c("BRAVE Score < 5", "BRAVE Score >=5"), 
                                   c("Easy Intubation Group", "Difficult Intubation Group")))

fisher.test(THYROMEN)



count(easy$Mouth.Opening)
count(difficult$Mouth.Opening)

MOUTHOPEN <- matrix(c(66,8,3,1),ncol=2,byrow=TRUE,
                    dimnames = list(c("BRAVE Score < 5", "BRAVE Score >=5"), 
                                    c("Easy Intubation Group", "Difficult Intubation Group")))

fisher.test(MOUTHOPEN)


count(easy$Neck.mobility)
count(difficult$Neck.mobility)

NECKMOB <- matrix(c(38,1,31,8),ncol=2,byrow=TRUE,
                  dimnames = list(c("BRAVE Score < 5", "BRAVE Score >=5"), 
                                  c("Easy Intubation Group", "Difficult Intubation Group")))

fisher.test(NECKMOB)



count(easy$Neck.Circumference)
count(difficult$Neck.Circumference)
NECKCIR <- matrix(c(40,7,29,2),ncol=2,byrow=TRUE,
                  dimnames = list(c("BRAVE Score < 5", "BRAVE Score >=5"), 
                                  c("Easy Intubation Group", "Difficult Intubation Group")))

fisher.test(NECKCIR)


count(easy$BMI..30)
count(difficult$BMI..30)

BMI30 <- matrix(c(48,8,21,1),ncol=2,byrow=TRUE,
                dimnames = list(c("BRAVE Score < 5", "BRAVE Score >=5"), 
                                c("Easy Intubation Group", "Difficult Intubation Group")))

fisher.test(BMI30)



count(easy$BMI..40)
count(difficult$BMI..40)

BMI40 <- matrix(c(62,7,7,2),ncol=2,byrow=TRUE,
                dimnames = list(c("BRAVE Score < 5", "BRAVE Score >=5"), 
                                c("Easy Intubation Group", "Difficult Intubation Group")))

fisher.test(BMI40)


count(easy$Thyro.sternal.distance)
count(difficult$Thyro.sternal.distance)

THYROSTERN <- matrix(c(65,9,4,0),ncol=2,byrow=TRUE,
                     dimnames = list(c("BRAVE Score < 5", "BRAVE Score >=5"), 
                                     c("Easy Intubation Group", "Difficult Intubation Group")))

fisher.test(THYROSTERN)


count(easy$Mandibular.angle)
count(difficult$Mandibular.angle)


MANDIBANGLE <- matrix(c(59,4,11,5),ncol=2,byrow=TRUE,
                      dimnames = list(c("BRAVE Score < 5", "BRAVE Score >=5"), 
                                      c("Easy Intubation Group", "Difficult Intubation Group")))

fisher.test(MANDIBANGLE)



count(easy$Overbite)
count(difficult$Overbite)

OVERBITE <- matrix(c(62,5,7,4),ncol=2,byrow=TRUE,
                   dimnames = list(c("BRAVE Score < 5", "BRAVE Score >=5"), 
                                   c("Easy Intubation Group", "Difficult Intubation Group")))

fisher.test(OVERBITE)



count(easy$Absence.of.upper.incisors)
count(difficult$Absence.of.upper.incisors)

ABSUPPINC <- matrix(c(43,0,26,9),ncol=2,byrow=TRUE,
                    dimnames = list(c("BRAVE Score < 5", "BRAVE Score >=5"), 
                                    c("Easy Intubation Group", "Difficult Intubation Group")))

fisher.test(ABSUPPINC)




#evaluate the affect of diplicate obesity documentation 

head(easy)
head(easy$Disease.associated.w..difficult.airway)
easy3 = subset(easy, Disease.associated.w..difficult.airway != '3' )

count(easy3)
count(easy)

library(utils)

view(easy3)

head(easy3)

head(easy)

