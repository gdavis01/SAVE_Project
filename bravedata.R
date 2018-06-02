library(ggplot2)
library(sciplot)
library(mosaic)
library(tableone)
library(survival)
library(car)
library(gridExtra)

# Grouped Bar Plot
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


brave=read.csv("BRAVE2.cSV")
brave
  

count(brave)

dmatrix <- matrix(c(25,1,0,6),ncol=2,byrow=TRUE,
             dimnames = list(c("BRAVE Score < 5", "BRAVE Score ???5"), 
                             c("Easy Intubation Group", "Difficult Intubation Group")))
brave$Absence.of.Upper.Incisors

count(brave$Absence.of.Upper.Incisors)


grid.table(dmatrix)

fisher.test(dmatrix)

brave=read.csv("brave.csv")
brave$

CCUData = subset(brave, Area=="CCU")
CCUData$


CCUData$Total.Score

brave$BMI..40

brave$Total.Score

mean(CCUData$Total.Score)

mean(brave$Total.Score)


brave$
difficult= subset(brave, Grade >="3")
difficult

  
easy= subset(brave, Grade < "3")
easy

difficult$Total.Score
easy$Total.Score

mean(difficult$Total.Score)

mean(easy$Total.Score)

median(difficult$Total.Score)

b1=edit(brave)

count(difficult$Total.Score >="4")

count(easy)

(count(easy$Total.Score <"4")

(count(difficult$Total.Score >="4")/count(difficult))



t.test(difficult$Total.Score, easy$Total.Score)

count(CCUData)
count(difficult)
count(easy)



count(easy$Grade1)/count(CCUData)

count(easy$Grade.2)

easy$Total.Score
count(difficult$Grade1)/count(CCUData)

count(difficult$Grade1)
easy
t.test(count(difficult$Grade1)/count(CCUData),count(easy$Grade1)/count(CCUData))

barplot(dmatrix, ylim=c(0,30),col=c("red","blue"), beside=TRUE, col="RED")

mmatrix <- matrix(c(6,1,0,6),ncol=2,byrow=TRUE)
grid.table(mmatrix)


mmatrix <- matrix(c(25,1,1,6),ncol=2,byrow=TRUE,
                  dimnames = list(c("BRAVE Score < 5", "BRAVE Score ???5"), 
                                  c("Easy Intubation Group", "Difficult Intubation Group")))
grid.table(mmatrix)
fisher.test(mmatrix)

count(difficult$Grade.4)

count(difficult$Disease.associated.w..difficult.airway)
count(CCUData$Disease.associated.w..difficult.airway)

count(difficult$Thyro.mental.distance)

easy$Mouth.Opening

easy$Absence.of.upper.incisors

