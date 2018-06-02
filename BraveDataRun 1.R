library(ggplot2)
library(sciplot)
library(mosaic)
library(tableone)
library(survival)
library(car)
library(gridExtra)



brave=read.csv("BraveModWide.csv")
brave


#I'm going to create a data matrix
##(easyAndShould,DifficultAndNot,EasyAndNot,DifficultShould))

dmatrix <- matrix(c(18,6,35,0),ncol=2,byrow=TRUE,
                  dimnames = list(c("BRAVE Score < 5", "BRAVE Score + or >5"), 
                                  c("Easy Intubation Group", "Difficult Intubation Group")))


bargraph.CI(dmatrix)

grid.table(dmatrix)

fisher.test(dmatrix)

difficult= subset(brave, Grade >="3")
difficult


easy= subset(brave, Grade < "3")
easy

difficult$Grade
easy$Grade

count(difficult)
count(easy)

difficult$Total.Score
easy$Total.Score

easy$Laryngoscope.used
easy$Cricoid.Pressur
easy$Extreme.hockey.stick
easy$Extreme.neck.position
easy$Bougie
easy$Multiple.Attempts
easy$H.O.Sifficult.Airway
easy$Disease.associated.with

easy$Mallampati
easy$Thyromental.distance
easy$Mouth.Opening
easy$Neck.Mobility
easy$Neck.Circumference
easy$BMI.30
easy$BMI.40
easy$Thyro.sternal.distance
easy$Mandibular.angle
easy$Overbite
easy$Absence.of.Upper.Incisors



difficult$Laryngoscope.used
difficult$Cricoid.Pressur
difficult$Extreme.hockey.stick
difficult$Extreme.neck.position
difficult$Bougie
difficult$Multiple.Attempts
difficult$H.O.Sifficult.Airway
difficult$Disease.associated.with

difficult$Mallampati
difficult$Thyromental.distance
difficult$Mouth.Openingv
difficult$Neck.Mobility
difficult$Neck.Circumference
difficult$BMI.30
difficult$BMI.40
difficult$Thyro.sternal.distance
difficult$Mandibular.angle
difficult$Overbite
difficult$Absence.of.Upper.Incisors


count(easy$Total.Score >="5")

count(easy)

count(easy$Total.Score<"5")

amatrix <- matrix(c(50,1,3,5),ncol=2,byrow=TRUE,
                  dimnames = list(c("BRAVE Score < 5", "BRAVE Score ???5"), 
                                  c("Easy Intubation Group", "Difficult Intubation Group")))
grid.table(amatrix)
fisher.test(amatrix)


bmatrix <- matrix(c(45,2,8,4),ncol=2,byrow=TRUE,
                  dimnames = list(c("BRAVE Score < 5", "BRAVE Score ???5"), 
                                  c("Easy Intubation Group", "Difficult Intubation Group")))
grid.table(bmatrix)
fisher.test(bmatrix)

cmatrix <- matrix(c(51,2,2,4),ncol=2,byrow=TRUE,
                  dimnames = list(c("BRAVE Score < 5", "BRAVE Score ???5"), 
                                  c("Easy Intubation Group", "Difficult Intubation Group")))
grid.table(cmatrix)
fisher.test(cmatrix)

dmatrix <- matrix(c(50,6,3,0),ncol=2,byrow=TRUE,
                  dimnames = list(c("BRAVE Score < 5", "BRAVE Score ???5"), 
                                  c("Easy Intubation Group", "Difficult Intubation Group")))
grid.table(dmatrix)
fisher.test(dmatrix)

dmatrix <- matrix(c(18,6,35,0),ncol=2,byrow=TRUE,
                  dimnames = list(c("BRAVE Score < 5", "BRAVE Score ???5"), 
                                  c("Easy Intubation Group", "Difficult Intubation Group")))
grid.table(dmatrix)
fisher.test(dmatrix)

dmatrix <- matrix(c(18,6,35,0),ncol=2,byrow=TRUE,
                  dimnames = list(c("BRAVE Score < 5", "BRAVE Score ???5"), 
                                  c("Easy Intubation Group", "Difficult Intubation Group")))
grid.table(dmatrix)
fisher.test(dmatrix)

dmatrix <- matrix(c(18,6,35,0),ncol=2,byrow=TRUE,
                  dimnames = list(c("BRAVE Score < 5", "BRAVE Score ???5"), 
                                  c("Easy Intubation Group", "Difficult Intubation Group")))
grid.table(dmatrix)
fisher.test(dmatrix)


easyint=c("50,3")
diffint=c("1,5")

table(easyint,diffint)




N=c(rep(50,3),rep(20,3))
Proport=c(20,15,15,5,10,5)/N

data <- data.frame(
  Category=rep(c('Smoker','Non-smoker'), each=3),
  Label=c('0<x<5','5<x<10','10<x<15','0<x<5','5<x<10','10<x<15'),
  Proportion=Proport,  
  SD=sqrt(Proport*(1-Proport)/N)
)

me1=qnorm(0.025,mean=data$Proportion,sd=data$SD)
me2=qnorm(0.975,mean=data$Proportion,sd=data$SD)

mat <- matrix(data$Proportion, nrow = 2, dimnames = list(levels(data$Category),levels(data$Label)))
barx=barplot(mat, beside=T, col = c("blue", "orange", "blue", "orange","blue", "orange"), names.arg = toupper(colnames(mat)), legend.text=TRUE,ylim=c(0,1),xlab="Income",ylab="Proportion",main="Bar plot with 95% confidence intervals")
box()
arrows(barx,me2, barx, me1, angle=90, code=1)
arrows(barx,me1, barx, me2, angle=90, code=1)


t.test(difficult$Total.Score,easy$Total.Score)

sample1=