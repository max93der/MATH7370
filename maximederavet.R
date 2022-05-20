data<-read.table("scout.txt", header=TRUE)
attach(data)
View(data)

######################
######QUESTION1#######
######################


#QUESTION 1.1
eff<-hist(data$Temps, breaks=c(3,4,4.5,5,5.5,6,6.5,7,7.5,8,10.5), plot=F)$counts
freq<-eff/sum(eff)
freq
effcum <- cumsum(eff)
effcum
freqcum <- cumsum(freq)
freqcum
borneInf <- c(3,4,4.5,5,5.5,6,6.5,7,7.5,8)
borneSup <- c(4,4.5,5,5.5,6,6.5,7,7.5,8,10.5)
cbind(borneInf, borneSup, eff, effcum, freq, freqcum)
stem(data$Temps,width=0)

#Question 1.2
histogram <- hist(data$Temps, c(3,4,4.5,5,5.5,6,6.5,7,7.5,8,10.5), col="gray", right=F,xlim=c(3,10.5), xlab="Classes", ylab="Fréquences", main="Histogramme de la variable temps")  

#Question 1.3
x <- seq(2.75,10.75, by=0.5)
y <- rep(c(0,histogram$density,0),c(1,2,1,1,1,1,1,1,1,1,5,1))
lines(x, y, type="o", pch=16, col=2)


#Question 1.4
1/100*((3*3.5)+(7*4.25)+(13*4.75)+(15*5.25)+(12*5.75)+(8*6.25)+(13*6.75)+(15*7.25)+(8*7.75)+(6*9.25))

mean(data$Temps)
median(data$Temps)

bornes <- c(4,4.5,5,5.5,6,6.5,7,7.5,8,10.5)
plot(ecdf(bornes),main="Distribution des effectifs cumulés", xlab="Classes", ylab="Frequ. cum.")

lines(bornes, c(freqcum), type="b", col="red", lty=2)


#Question 1.5
boiteMoustClas<-boxplot(data$Temps,range=0, main="Boite à moustache de Temps ")



######################
######QUESTION2#######
######################

#Question 2.1
age<-c(data$Age)
age<-factor(age)
boxplot(data$Temps~age, xlab="Âge", ylab="Temps", main="Temps passé dehors en fonction de l'âge")


#Question 2.2  
T1<-data$Temps[data$Age==1]
T1
T2<-data$Temps[data$Age==2]
T2
T3<-data$Temps[data$Age==3]
T3

quant1<-quantile(T1)
quant2<-quantile(T2)
quant3<-quantile(T3)

EIQ1<-quant1[3]-quant1[1]
EIQ2<-quant2[3]-quant2[1]
EIQ3<-quant3[3]-quant3[1]

