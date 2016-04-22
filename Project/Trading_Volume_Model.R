## Author: Wang, Enqun
## E-mail: wang.enqun@outlook.com
## Company: FirstP2P.com
## Date: 6/25/2015

#### Input Data

library(gdata)
mydata = read.csv(file.choose())

################## Exploratory Data Analysis ##########################

#### Summary

summary(mydata)
cor(mydata)
pairs(mydata)

#### Density plots

require(gridExtra)
require(ggplot2)

p1 <- ggplot(mydata, aes(x = X))+
  geom_density()
p2 <- ggplot(mydata, aes(x = C))+
  geom_density()
p3 <- ggplot(mydata, aes(x = D))+
  geom_density()
p4 <- ggplot(mydata, aes(x = E))+
  geom_density()

p5 <- ggplot(mydata, aes(x = F))+
  geom_density()
p6 <- ggplot(mydata, aes(x = G))+
  geom_density()
p7 <- ggplot(mydata, aes(x = H))+
  geom_density()
p8 <- ggplot(mydata, aes(x = I))+
  geom_density()

p9 <- ggplot(mydata, aes(x = J))+
  geom_density()
p10 <- ggplot(mydata, aes(x = K))+
  geom_density()
p11 <- ggplot(mydata, aes(x = L))+
  geom_density()
p12 <- ggplot(mydata, aes(x = M))+
  geom_density()

grid.arrange(p1, p2, p3, p4,
             p5, p6, p7, p8, 
             p9, p10, p11, p12, ncol=4)

## The density plots indicate that varible G, K, and L are bimodally distributed. Thus, we first leave out these variables to build the first model.
## Also, the density plots shows that varible C, D, F, H, and J skew left. Besides, I seems skew right. We might try to transform these variables.

#### Transformation

#### Density plots

p1 <- ggplot(mydata, aes(x = X))+
  geom_density()
p2 <- ggplot(mydata, aes(x = C))+
  geom_density()
p3 <- ggplot(mydata, aes(x = D^.5))+
  geom_density()

p4 <- ggplot(mydata, aes(x = E^.25))+
  geom_density()
p5 <- ggplot(mydata, aes(x = F^.5))+
  geom_density()
p7 <- ggplot(mydata, aes(x = H^.25))+
  geom_density()

p8 <- ggplot(mydata, aes(x = I))+
  geom_density()
p9 <- ggplot(mydata, aes(x = J))+
  geom_density()
p12 <- ggplot(mydata, aes(x = M))+
  geom_density()

grid.arrange(p1, p2, p3, 
             p4, p5, p7, 
             p8, p9, p12, ncol=3)
## After tranformating, variable C still skews a lot.???

#### Scatterplots

p1 <- ggplot(mydata, aes(X, X))+
  geom_point()
p2 <- ggplot(mydata, aes(C, X))+
  geom_point()
p3 <- ggplot(mydata, aes(D, X))+
  geom_point()

p4 <- ggplot(mydata, aes(E, X))+
  geom_point()
p5 <- ggplot(mydata, aes(F, X))+
  geom_point()
p7 <- ggplot(mydata, aes(H, X))+
  geom_point()

p8 <- ggplot(mydata, aes(I, X))+
  geom_point()
p9 <- ggplot(mydata, aes(J, X))+
  geom_point()
p12 <- ggplot(mydata, aes(M, X))+
  geom_point()

grid.arrange(p1, p2, p3, 
             p4, p5, p7, 
             p8, p9, p12, ncol=3)
## We leave out variable D, F, H, I, and J. Thus, we now have C, E, and M. An extreme outlier appears in E; and we consider if we can leave it out.

############################# Bimodal Variables ################################

require(gdata)
rdata<-read.csv(file.choose())

cor(rdata$K, rdata$L)

## Since K and L are highly coorelated, we can combine these two variables into one.

cor(rdata$X, rdata$L)
cor(rdata$X, rdata$G) 

#### Density & Histgram Plots

h<-ggplot(data=rdata, aes(G))+
  geom_histogram(aes(y=..density.., fill=..count..), color="darkgreen")+
  geom_density(color="gold") +
  scale_fill_gradient("Count", low = "dodgerblue4", high = "firebrick")+
  labs(title="Histgram & Density Plot of Value in Lucky Bags", 
       x="Value in Lucky Bags", y="Count")+
  theme_bw()
h

#### Scatter plots

require(ggplot2)
require(gridExtra)

p6 <- ggplot(rdata, aes(G, X))+
  geom_point()
p10 <- ggplot(rdata, aes(L, X))+
  geom_point()

grid.arrange(p6, p10, ncol=2)

#### Mixture Model

require(mixtools)

mixmdlL<-normalmixEM(rdata$L)
mixmdlG<-normalmixEM(rdata$G)

summary(mixmdlL)
summary(mixmdlG)

par(mfrow=c(1,2))                   

plot(mixmdlL, which=2)
lines(density(rdata$L), lty=2, lwd=2)

plot(mixmdlG, which=2)
lines(density(rdata$G), lty=2, lwd=2)

par(mfrow=c(1,1))


#### Analyze the relationship between X and L

## This part related to time series. As it relates to a long time period, we will not conclude it in our model.

model4<-lm(X ~ L, data=rdata)
summary(model4)


#### Turn G into Categorical Variable

require(gdata)

rdata<-read.csv(file.choose())

N<-as.factor(rdata$N)

plot(N)
b<-ggplot(data=rdata, aes(factor(N), X))+
  geom_boxplot(fill = "grey80", colour = "#3366FF")+
  labs(title="Boxplot of Lucky Bag vs. Trading Volume", 
       x="Lucky Bag", y="Trading Volume")

require(stats)
oneway.test(X ~ N, data=rdata) 

## Take only X and N into a model to do F test, we find that N does affect the value of X.
## However, when we put N into the whole model, we find that the influence of N is not significant.

#### Model Buiding

library(leaps)

model2 = regsubsets(X ~ C + D + E + F + J + H + I + M + N, 
                    data = rdata)

summary(model2)
summary(model2)$cp #Provides Mallow’s Cp for each p
summary(model2)$bic #Provides BIC for each p

model3<-glm(X ~ J + C + M , data=rdata)
summary(model3)

ggdiagnostic(model3)


####################### Revise Data ###################################

rdata<-read.csv(file.choose())

N<-as.factor(rdata$N)

#### Model Building & Selection

library(leaps)

model0 = regsubsets(X ~ C + D + E + F + H + I + J + M + N, 
                    data = rdata)

summary(model0)
summary(model0)$cp #Provides Mallow’s Cp for each p
summary(model0)$bic #Provides BIC for each p

## The Mallow's Cp suggests a model with variables C, J, M, D, and N. 
## The BIC suggests a model with variables C, J, and M. 
## Based on the parsimony principle, we choose the model with few variables.

#### Final Model

model<-lm(X ~ C + J + M, data = rdata)
summary(model)

confint(model)

#### Density plots

require(gridExtra)
require(ggplot2)

p1 <- ggplot(rdata, aes(x = X))+
  geom_density()
p2 <- ggplot(rdata, aes(x = C))+
  geom_density()
p9 <- ggplot(rdata, aes(x = J))+
  geom_density()
p12 <- ggplot(rdata, aes(x = M))+
  geom_density()

grid.arrange(p1, p2, p9, p12, ncol=2)

#### Density plots after transformating.

p1 <- ggplot(rdata, aes(x = X))+
  geom_density()
p2 <- ggplot(rdata, aes(x = (C-8.8)^.5))+
  geom_density()
p9 <- ggplot(rdata, aes(x = J^.5))+
  geom_density()
p12 <- ggplot(rdata, aes(x = M))+
  geom_density()

grid.arrange(p1, p2, p9, p12, ncol=2)

## This transformation is good, but it can make interperation tough. Therefore, we first use the original forms.

#### Scatterplots

require(MASS)

p2 <- ggplot(rdata, aes(C, X))+
  geom_point()+
  geom_smooth(color="firebrick", method="rlm")
p9 <- ggplot(rdata, aes(J, X))+
  geom_point()+
  geom_smooth(color="firebrick", method="rlm")
p12 <- ggplot(rdata, aes(M, X))+
  geom_point()+
  geom_smooth(color="firebrick", method="rlm")

grid.arrange(p2, p9, p12, ncol=2)


#### Regression Diagnostics for Model

par(mfrow=c(2,2))                   
plot(model)
par(mfrow=c(1,1))

## All in one
require(ggplot2)

ggdiagnostic<-function(model){
  diagPlot<-function(model){
    p1<-ggplot(model, aes(.fitted, .resid))+geom_point()
    p1<-p1+stat_smooth(method="loess")+
      geom_hline(yintercept=0, col="red", linetype="dashed")+
      geom_hline(y = 3, linetype = "dashed", colour = "red") +
      geom_hline(y = -3, linetype = "dashed", colour = "red") 
    p1<-p1+xlab("Fitted values")+ylab("Residuals")
    p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw()
    
    p2<-ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
    p2<-p2+geom_abline(aes(qqline(.stdresid)))+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
    p2<-p2+ggtitle("Normal Q-Q")+theme_bw()
    
    p3<-ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+geom_point(na.rm=TRUE)
    p3<-p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value")
    p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
    p3<-p3+ggtitle("Scale-Location")+theme_bw()
    
    p4<-ggplot(model, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity")
    p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
    p4<-p4+ggtitle("Cook's distance")+theme_bw()
    
    p5<-ggplot(model, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE)
    p5<-p5+stat_smooth(method="loess", na.rm=TRUE)
    p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
    p5<-p5+ggtitle("Residual vs Leverage Plot")
    p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5))
    p5<-p5+theme_bw()+theme(legend.position="bottom")
    
    p6<-ggplot(model, aes(.hat, .cooksd))+geom_point(na.rm=TRUE)+stat_smooth(method="loess", na.rm=TRUE)
    p6<-p6+xlab("Leverage hii")+ylab("Cook's Distance")
    p6<-p6+ggtitle("Cook's dist vs Leverage hii/(1-hii)")
    p6<-p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")
    p6<-p6+theme_bw()
    
    return(list(rvfPlot=p1, qqPlot=p2, sclLocPlot=p3, cdPlot=p4, rvlevPlot=p5, cvlPlot=p6))
  }
  
  diagPlts<-diagPlot(model)
  library(gridExtra)
  lbry<-c("grid", "gridExtra")
  lapply(lbry, require, character.only=TRUE, warn.conflicts = FALSE, quietly = TRUE)
  do.call(grid.arrange, c(diagPlts, main="Diagnostic Plots", ncol=3))
}

ggdiagnostic(model)



################################## Normalize Data ##################################################

require(gdata)
rdata<-read.csv(file.choose())

#### Normalize data

library(dplyr)
sdata<-rdata %>% mutate_each_(funs(scale),vars=c("X", "C", "D", "E", "F", "H", "I", "J", "M"))

################## Exploratory Data Analysis ##########################

#### Summary

summary(sdata)
cor(sdata)
pairs(sdata)


#### Density plots

require(gridExtra)
require(ggplot2)

p1 <- ggplot(sdata, aes(x = X))+
  geom_density()
p2 <- ggplot(sdata, aes(x = C))+
  geom_density()
p3 <- ggplot(sdata, aes(x = D))+
  geom_density()

p4 <- ggplot(sdata, aes(x = E))+
  geom_density()
p5 <- ggplot(sdata, aes(x = F))+
  geom_density()
p7 <- ggplot(sdata, aes(x = H))+
  geom_density()

p8 <- ggplot(sdata, aes(x = I))+
  geom_density()
p9 <- ggplot(sdata, aes(x = J))+
  geom_density()
p12 <- ggplot(sdata, aes(x = M))+
  geom_density()

grid.arrange(p1, p2, p3, p4,
             p5, p7, p8, 
             p9, p12, ncol=3)

#### Scatterplots

p1 <- ggplot(sdata, aes(X, X))+
  geom_point()
p2 <- ggplot(sdata, aes(C, X))+
  geom_point()
p3 <- ggplot(sdata, aes(D, X))+
  geom_point()

p4 <- ggplot(sdata, aes(E, X))+
  geom_point()
p5 <- ggplot(sdata, aes(F, X))+
  geom_point()
p7 <- ggplot(sdata, aes(H, X))+
  geom_point()

p8 <- ggplot(sdata, aes(I, X))+
  geom_point()
p9 <- ggplot(sdata, aes(J, X))+
  geom_point()
p12 <- ggplot(sdata, aes(M, X))+
  geom_point()

grid.arrange(p1, p2, p3, 
             p4, p5, p7, 
             p8, p9, p12, ncol=3)

#### Model Building & Selection

library(leaps)

model1 = regsubsets(X ~ C + D + E + F + H + I + J + M, 
                    data = sdata)

summary(model1)
summary(model1)$cp #Provides Mallow’s Cp for each p
summary(model1)$bic #Provides BIC for each p

## The results of normalized data is the same as non-normalized data.
