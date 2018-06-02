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


brave1=read.csv("BraveModWide (1).csv")
brave


head(brave1)

brave1$Last.Name

brave = subset(brave1, Last.Name != 'MARQUIS')
marquis = subset(brave1, Last.Name == 'MARQUIS')
marquis

count(brave)

head(brave)

difficult= subset(brave, Grade >="3")
difficult


easy= subset(brave, Grade < "3")
easy



difficult$Grade
easy$Grade

count(difficult)
count(easy)

View(brave)

brave$X = NULL
brave$X.1 = NULL
brave$X.2 = NULL

#Evaluate cutoff score
difficult$Total.Score
easy$Total.Score

count(easy$Absence.of.upper.incisors)
easy$Absence.of.upper.incisors

count(easy$Laryngoscope.not.used)
count(easy$Cricoid.Pressure)


easy$Extreme.hockey.stick
easy$Extreme.neck.position
easy$Bougie
easy$Multiple.Attempts
easy$H.O.Sifficult.Airway
easy$Disease.associated.with

count(easy$Mouth.Opening)
count(easy$Mallampati)
count(easy$Thyromental.distance)
count(easy$Neck.Mobility)
count(easy$Neck.Circumference)
count(easy$BMI.30)
count(easy$BMI.40)
count(easy$Thyro.sternal.distance)
count(easy$Mandibular.angle)
count(easy$Overbite)

count(easy$Absence.of.Upper.Incisors)



count(difficult$Laryngoscope.used)
count(difficult$Cricoid.Pressur)
count(difficult$Extreme.hockey.stick)
count(difficult$Extreme.neck.position)
count(difficult$Bougie)
count(difficult$Multiple.Attempts)
count(difficult$H.O.Sifficult.Airway)
count(difficult$Disease.associated.with)


count(difficult$Mallampati)
count(difficult$Thyromental.distance)
count(difficult$Mouth.Openingv$Neck.Mobility)
count(difficult$Neck.Circumference)
count(difficult$BMI.30)
count(difficult$BMI.40)
count(difficult$Thyro.sternal.distance)
count(difficult$Mandibular.angle)
count(difficult$Overbite)
count(difficult$Absence.of.Upper.Incisors)

count(easy, Total.Score<5)
count(difficult, Total.Score>=5)

marquis$Grade

#evaluate false positives
FalsePositive = subset(easy, Total.Score>=5) 
View(FalsePositive)

difficult$Total.Score

count(easy)
count(difficult)

difficult$Total.Score

brave$Disease.associated.w..difficult.airway
easy$Disease.associated.w..difficult.airway



amatrix <- matrix(c(66,1,3,8),ncol=2,byrow=TRUE,
                  dimnames = list(c("BRAVE Score < 5", "BRAVE Score >=5"), 
                                  c("Easy Intubation Group", "Difficult Intubation Group")))

fisher.test(amatrix)


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

