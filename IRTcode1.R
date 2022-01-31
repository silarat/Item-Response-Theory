library(mirt)
library(lavaan)
library(psych)

#assess unidimensionality
eigen(cor(IRTdicho))$values
sum(eigen(cor(IRTdicho))$values>1) 
scree(IRTdicho)

##Parallel Analysis
RAWPAR(IRTdicho, factormodel='PCA', Ndatasets=100, percentile=95,corkind='pearson', verbose=TRUE)

#ICC for dichotomous scoring
twoPL <- mirt(data  = IRTdicho,
              model = 1,
              itemtype="2PL",
              SE=TRUE)

coef(twoPL,IRTpars=TRUE,simplify=TRUE)
# Plot Item Characteristic Curves
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

#estimate person parameter
mle <- fscores(twoPL,method="ML")
describe(mle)

#test information
Theta = seq(-6,6,.01)
tinfo <- testinfo(twoPL, Theta)
describe(tinfo)

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


TwoPL <- mirt(IRTdicho, 1, itemtype='2PL')
fit <- itemfit(TwoPL) 
itemfit(TwoPL, 'S_X2') #S_X2 or X2

#item-total correlation
total.score <- rowSums(IRTdicho)
t.test(total.score ~ IRTdicho$BItem1)

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

#Cronbach's alpha (internal consistency)
alpha(IRTdicho)
alpha(IRTpoly)