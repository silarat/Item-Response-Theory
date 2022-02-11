require(mirt)
require(psych)
library(dplyr)

rm(list=ls()) 

## Clean up global environment if R is running slowly ----
gc()

## Maximize memory if running low ----
### for 64-bit computers use this code:
# memory.size(8000)
### for older 32-bit computers use this code:
# memory.size(4000)

## Avoid getting results in scientific notation ----
options(scipen=100)
options(digits=2)

# SET UP YOUR WORKING DIRECTORY 'wd' for importing/saving files easily, update as needed ----
## I'm using the following pathway, change as needed for your computer ----
getwd()
currentwd <-"C:/Users/palmy/Documents/cambridge/gradschool/UW-Autumn21/HLM/R/" 
setwd(currentwd)
#------


##### Rules to decide the number of factors << This is where you assess UNIDIMENTIONALITY (is there one factor or more?)

###Eigenvalue >1 rule
eigen(cor(IRTdicho))$values
sum(eigen(cor(IRTdicho))$values>1) ##extracts the eigenvalues of the correlation matrix
##counts how many are >1 and sums them
###Scree plot

scree(IRTdicho)

##Parallel Analysis
RAWPAR(IRTdicho, factormodel='PCA', Ndatasets=100, percentile=95,corkind='pearson', verbose=TRUE)


#startover
IRTpoly <- read.delim("IRTpoly2.dat", header = TRUE)
str(IRTpoly)

IRTdicho <- read.delim("IRTdicho2.dat", header = TRUE)
str(IRTdicho)

IRTpoly$ï..SubjID <- NULL
str(IRTpoly)

IRTdicho$ï..SubjID <- NULL
str(IRTdicho)

#model fit

#ICC for dichotomous scoring
twoPL <- mirt(data  = IRTdicho,
              model = 1,
              itemtype="2PL",
              SE=TRUE)

coef(twoPL,IRTpars=TRUE,simplify=TRUE)
# Plot Item Characteristic Curves
args(itemplot)
?itemplot
itemplot(twoPL,item=1,type="trace")
itemplot(twoPL,item=2,type="trace")
itemplot(twoPL,item=3,type="trace")
itemplot(twoPL,item=4,type="trace")
itemplot(twoPL,item=5,type="trace")
itemplot(twoPL,item=6,type="trace")
itemplot(twoPL,item=7,type="trace")
itemplot(twoPL,item=8,type="trace")
itemplot(twoPL,item=9,type="trace")
itemplot(twoPL,item=10,type="trace")

exp.score <- expected.test(twoPL,as.matrix(Theta))
cbind(Theta,exp.score)
plot(Theta,exp.score,type="l",ylab="Expected Test Score", main="Expected Score Curve: Dichotomous Scoring")

#estimate person parameter
mle <- fscores(twoPL,method="ML")
describe(mle)

itemplot(twoPL,item=1,type="info")
itemplot(twoPL,item=2,type="info")
itemplot(twoPL,item=3,type="info")
itemplot(twoPL,item=4,type="info")
itemplot(twoPL,item=5,type="info")
itemplot(twoPL,item=6,type="info")
itemplot(twoPL,item=7,type="info")
itemplot(twoPL,item=8,type="info")
itemplot(twoPL,item=9,type="info")
itemplot(twoPL,item=10,type="info")



#testinfo
Theta = seq(-6,6,.01)
tinfo <- testinfo(twoPL, Theta)
describe(tinfo)
plot(Theta,testinfo(twoPL,Theta=Theta),type="l",
     ylab="Information", xlab="Endorsement of Autistic Tendencies", main = 'Information: Dichotomous Scoring', ylim=c(0,max(testinfo(twoPL,Theta=Theta))))

#plotting all the item information
extr.1 <- extract.item(twoPL,1)
extr.2 <- extract.item(twoPL,2)
extr.3 <- extract.item(twoPL,3)
extr.4 <- extract.item(twoPL,4)
extr.5 <- extract.item(twoPL,5)
extr.6 <- extract.item(twoPL,6)
extr.7 <- extract.item(twoPL,7)
extr.8 <- extract.item(twoPL,8)
extr.9 <- extract.item(twoPL,9)
extr.10 <- extract.item(twoPL,10)

points(Theta,iteminfo(extr.1,Theta,total.info=TRUE),
       type="l",lty=2,col="green",lwd=2)

points(Theta,iteminfo(extr.2,Theta,total.info=TRUE),
       type="l",lty=2,col="blue",lwd=2)

points(Theta,iteminfo(extr.3,Theta,total.info=TRUE),
       type="l",lty=2,col="orange",lwd=2)

points(Theta,iteminfo(extr.4,Theta,total.info=TRUE),
       type="l",lty=2,col="red",lwd=2)

points(Theta,iteminfo(extr.5,Theta,total.info=TRUE),
       type="l",lty=2,col="purple",lwd=2)

points(Theta,iteminfo(extr.6,Theta,total.info=TRUE),
       type="l",lty=2,col="dark green",lwd=2)

points(Theta,iteminfo(extr.7,Theta,total.info=TRUE),
       type="l",lty=2,col="pink",lwd=2)

points(Theta,iteminfo(extr.8,Theta,total.info=TRUE),
       type="l",lty=2,col="brown",lwd=2)

points(Theta,iteminfo(extr.9,Theta,total.info=TRUE),
       type="l",lty=2,col="light blue",lwd=2)

points(Theta,iteminfo(extr.10,Theta,total.info=TRUE),
       type="l",lty=2,col="gray",lwd=2)

legend("topleft",c("Item 1","Item 2","Item 3","Item 4","Item 5", "Item 6", "Item 7", "Item 8", "Item 9", "Item 10", "Total Info"),
       lty=c(2,2,2,2,2,2,2,2,2,2,1),
       col=c("green","blue","orange","red","purple", "dark green", "pink", "brown", "light blue", "gray", "black"),
       lwd=c(2,2,2,2,2,2,2,2,2,2,1))

#the legend will keep adding on
describe()
#I can think about how the test info reveals that only people with high endorsement of theta will benefit from this test?

#item fit
TwoPL <- mirt(IRTdicho, 1, itemtype='2PL')
fit <- itemfit(TwoPL) # the default method is "S_X2"
itemfit(TwoPL, 'S_X2') #at first it was X2


#item-total correlation
total.score <- rowSums(IRTdicho)
t.test(total.score ~ IRTdicho$BItem1)
###############################
#model fit
GPCM <- mirt(data  = IRTpoly,
            model = 1,
            itemtype="gpcm",
            SE=TRUE)

RSM <- mirt(data  = IRTpoly,
            model = 1,
            itemtype="rsm",
            SE=TRUE)
anova(GRM, GPCM)
anova(GRM, RSM)
#polytomous
?mirt
GRM <- mirt(data  = IRTpoly,
            model = 1,
            itemtype="graded",
            SE=TRUE)

coef(GRM,IRTpars=TRUE,simplify=TRUE)
itemplot(GRM,item=1,type="trace")
itemplot(GRM,item=2,type="trace")
itemplot(GRM,item=3,type="trace")
itemplot(GRM,item=4,type="trace")
itemplot(GRM,item=5,type="trace")
itemplot(GRM,item=6,type="trace")
itemplot(GRM,item=7,type="trace")
itemplot(GRM,item=8,type="trace")
itemplot(GRM,item=9,type="trace")
itemplot(GRM,item=10,type="trace")
exp.score <- expected.test(GRM,as.matrix(Theta))
cbind(Theta,exp.score)
plot(Theta,exp.score,type="l",ylab="Expected Test Score", main="Expected Score Curve: Polytomous Scoring")
mle2 <- fscores(GRM,method="ML")
describe(mle2)

#expected score


#GRM cat info

a_extr.2 <- extract.item(GRM, 2)
Theta2 <- matrix(seq(-6,6, by = .1))
info.2 <- iteminfo(a_extr.2, Theta2)

cat.info <- iteminfo(a_extr.2, Theta2, total.info = FALSE)
plot(Theta2, cat.info[,1], type = 'l', ylim = c(0, max(cat.info)),
     ylab = 'info', main = 'Category information')
for(i in 2:ncol(cat.info))
  lines(Theta2, cat.info[,i], col = i)
plot(Theta2, cat.info[,1], type = 'l', ylim = c(0, max(cat.info)),
     ylab = 'info', main = 'Category information')
for(i in 2:ncol(cat.info))
  lines(Theta2, cat.info[,i], col = i)

#GRM Test info
Theta = seq(-6,6,.01)
tinfo <- testinfo(GRM, Theta)
describe(tinfo)
plot(Theta,testinfo(GRM,Theta=Theta),type="l",
     ylab="Information",
     xlab="Endorsement of Autistic Tendencies", main = 'Information: Polytomous Scoring', ylim=c(0,max(testinfo(GRM,Theta=Theta))))

#plotting all the item information
extr.1 <- extract.item(GRM,1)
extr.2 <- extract.item(GRM,2)
extr.3 <- extract.item(GRM,3)
extr.4 <- extract.item(GRM,4)
extr.5 <- extract.item(GRM,5)
extr.6 <- extract.item(GRM,6)
extr.7 <- extract.item(GRM,7)
extr.8 <- extract.item(GRM,8)
extr.9 <- extract.item(GRM,9)
extr.10 <- extract.item(GRM,10)

points(Theta,iteminfo(extr.1,Theta,total.info=TRUE),
       type="l",lty=2,col="green",lwd=2)

points(Theta,iteminfo(extr.2,Theta,total.info=TRUE),
       type="l",lty=2,col="blue",lwd=2)

points(Theta,iteminfo(extr.3,Theta,total.info=TRUE),
       type="l",lty=2,col="orange",lwd=2)

points(Theta,iteminfo(extr.4,Theta,total.info=TRUE),
       type="l",lty=2,col="red",lwd=2)

points(Theta,iteminfo(extr.5,Theta,total.info=TRUE),
       type="l",lty=2,col="purple",lwd=2)

points(Theta,iteminfo(extr.6,Theta,total.info=TRUE),
       type="l",lty=2,col="dark green",lwd=2)

points(Theta,iteminfo(extr.7,Theta,total.info=TRUE),
       type="l",lty=2,col="pink",lwd=2)

points(Theta,iteminfo(extr.8,Theta,total.info=TRUE),
       type="l",lty=2,col="brown",lwd=2)

points(Theta,iteminfo(extr.9,Theta,total.info=TRUE),
       type="l",lty=2,col="light blue",lwd=2)

points(Theta,iteminfo(extr.10,Theta,total.info=TRUE),
       type="l",lty=2,col="gray",lwd=2)

legend("topleft",c("Item 1","Item 2","Item 3","Item 4","Item 5", "Item 6", "Item 7", "Item 8", "Item 9", "Item 10", "Total Info"),
       lty=c(2,2,2,2,2,2,2,2,2,2,1),
       col=c("green","blue","orange","red","purple", "dark green", "pink", "brown", "light blue", "gray", "black"),
       lwd=c(2,2,2,2,2,2,2,2,2,2,1))
?anova

GRM <- mirt(IRTpoly, 1, itemtype='graded')
fit <- itemfit(GRM) # the default method is "S_X2"
itemfit(GRM, 'X2')

#item-total score 
total.score2 <- rowSums(IRTpoly)
t.test(total.score2 ~ IRTpoly$Item1)



cohen.d(total.score2 ~ IRTpoly$Item1)

alpha(IRTdicho)
alpha(IRTpoly)
