###########################
## Ryan McCollum
## STA 563 Final MLR Project
##
## Explores crab measurement dataset and uses multiple types of model selection and model
## comparison criteria on a multiple regression model with numeric and categorical predictors



library(GGally)
library(lattice)
library(leaps)
library(car)
library(boot)
library(dplyr)
library(tidyverse)
library(caret)
library(ggplot2)
getwd()
setwd("C:/DESKTOP/STA 563")


CrabData <- read.csv("crab-data.csv")
head(CrabData)

CrabData_w_Sexes <- CrabData %>%
  mutate(Sexes = case_when(Sex == 'I' ~ "Indeterminate",
                                  Sex == 'F' ~ "Female",
                                  Sex == 'M'~ "Male"))

#Check how many of each sex there are
CrabDataM <- CrabData %>%
  filter(Sex=='M')
CrabDataF <- CrabData %>%
  filter(Sex=='F')
CrabDataI <- CrabData %>%
  filter(Sex=='I')

Sexes <- c("Male", "Female", "Indeterminate")
frequency <- c(1435, 1225, 1233)

df <- data.frame(Sexes, frequency)

# See if there is significant difference between Sexes
boxplot(Age ~ as.factor(Sexes), CrabData_w_Sexes)

ggplot(df, aes(x=as.factor(Sexes), y=frequency)) +
  geom_col() +
  theme_minimal()


# Need to change indicator variables
CrabData_W_Dummys <- CrabData %>%
  mutate( SexI = case_when(Sex == 'I' ~ 1,
                           Sex == 'F' ~ 0,
                           Sex == 'M'~ 0),
          SexF = case_when(Sex == 'F' ~ 1,
                           Sex == 'M' ~ 0,
                           Sex == 'I'~ 0)) %>%
          # So Male is baseline 
  select(-Sex)
head(CrabData_W_Dummys)          

# Check correlation between variables
cor(CrabData_W_Dummys, maxdec=3)

ggpairs(CrabData_W_Dummys)
ggsave(filename="sta563_Project2-GGpairs.png",
       width=11.5, height=8, dpi=900, bg="white")

No.Interactions.model <- lm(Age~(Length + Diameter + Height + Weight + 
                        Shucked.Weight + Viscera.Weight + Shell.Weight + SexI + SexF), 
                 data=CrabData_W_Dummys)
summary(No.Interactions.model)
vif(No.Interactions.model)

# Very High VIFS for length, Diameter, and all weight variables
# Probably want 1 of length / diameter and 1 or two maximum of the weight variables
# Makes sense to keep diameter and weight of the correlated variables

GettingRidOfHighVifs.model <-  lm(Age~(Diameter + Height + Weight + SexI + SexF), 
                   data=CrabData_W_Dummys)
summary(GettingRidOfHighVifs.model)
vif(GettingRidOfHighVifs.model)

# Some slightly concerning VIFs with Diameter and Weight, but it is expected that there is some correlation
# btwn these 2 variables, so since the VIFs aren't agregious, my judgement is that all variables in the new model can be fit.
# Might be useful to study interaction term between the sex of a crab and their diameter, weight and height
Full.model <-  lm(Age~(Diameter + Height + Weight + SexI + SexF + 
                         Diameter*SexI + Diameter*SexF + Height*SexI + 
                         Height*SexF + Weight*SexI + Weight*SexF), 
                                  data=CrabData_W_Dummys)
summary(Full.model)



########################################
# What if want to do predictive model

Full.Model <- lm(Age~((Length + Diameter + Height + Weight + 
                         Shucked.Weight + Viscera.Weight + Shell.Weight + SexI + SexF) +
                        (Length + Diameter + Height + Weight + 
                           Shucked.Weight + Viscera.Weight + Shell.Weight)*(SexI+SexF)),
                 data=CrabData_W_Dummys)

avPlots(Full.Model)


# Now pick best subsets (BIC), forward and backward model, with interactions between
#   Sex indicators and measurements
Full.best <- regsubsets(Age~((Length + Diameter + Height + Weight + 
                                  Shucked.Weight + Viscera.Weight + Shell.Weight + SexI + SexF) +
                               (Length + Diameter + Height + Weight + 
                                  Shucked.Weight + Viscera.Weight + Shell.Weight)*(SexI+SexF)),
                          data=CrabData_W_Dummys, nvmax=12)
                        
best.summary=summary(Full.best)

par(mfrow=c(2,2))

plot(best.summary$bic, xlab="Number of Variables", ylab="BIC")
BIC.min=which.min(best.summary$bic)
points(BIC.min, best.summary$bic[BIC.min], col='darkblue', cex=2, pch=20)

plot(best.summary$cp, xlab="Number of Variables", ylab="CP")
CP.min=which.min(best.summary$cp)
points(CP.min, best.summary$cp[CP.min], col='darkblue', cex=2, pch=20)

# How about adjusted R^2, and we can plot R^2 just to see how must adjusted R^2 affects the outcome
plot(best.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq")
adjr2.max=which.max(best.summary$adjr2)
points(adjr2.max, best.summary$adjr2[adjr2.max], col='darkblue', cex=2, pch=20)

plot(best.summary$rsq, xlab="Number of Variables", ylab="RSq")



# CP and Adjusted R2 suggest 12 variables, BIC suggests 10
# All measurements seem to suggest anywhere between 8-12 variables will make a good model
coef(Full.best, BIC.min)
coef(Full.best, CP.min)
# Considering BIC penelizes more for less relevant parameters and it chose a smaller number of variables,
# I think the BIC model with 10 variables is probably a better suggestion than the CP one with 12

#BIC model with 10:  
# 4.8519275 + 1.9146033*Diameter + 7.2572292*Height + 0.3339372*Weight 
# - 0.7081950*Shucked.Weight - 0.3544657*Viscera.Weight +  0.3096686*Shell.Weight - 1.7359903*SexI 
# + 2.2048661*SexF - 5.7176934*Height:SexF + 0.1479948*Shucked.Weight:SexI 

# CP model with 12:  
# 5.56495716 - 1.83134904*Length + 3.61899712*Diameter + 6.46088404*Height + 0.33178286*Weight 
# - 0.69222906*Shucked.Weight - 0.33862716*Viscera.Weight + 0.31660240*Shell.Weight - 2.56408238*SexI 
# + 1.99269819*SexF + 3.85093966*Height:SexI - 5.14575161*Height:SexF + 0.09826259*Shucked.Weight:SexI 

## Most variables values are the same between the 2, but the BIC model uses all of the variables in
# the CP model except length and Height:SexI



# now forward and backward
Forward.full <- regsubsets(Age~((Length + Diameter + Height + Weight + 
                                   Shucked.Weight + Viscera.Weight + Shell.Weight + SexI + SexF) +
                                  (Length + Diameter + Height + Weight + 
                                     Shucked.Weight + Viscera.Weight + Shell.Weight)*(SexI+SexF)),
                           data=CrabData_W_Dummys, nvmax=12, method='forward')

Backward.full <- regsubsets(Age~((Length + Diameter + Height + Weight + 
                                    Shucked.Weight + Viscera.Weight + Shell.Weight + SexI + SexF) +
                                   (Length + Diameter + Height + Weight + 
                                      Shucked.Weight + Viscera.Weight + Shell.Weight)*(SexI+SexF)),
                            data=CrabData_W_Dummys, nvmax=12, method='backward')

forward.summ <- summary(Forward.full)
backward.summ <- summary(Backward.full)

forward.summ
backward.summ

# Forward model with 8 variables (Simplest model in the suggested range by BIC, AIC, ADJR2): 
# Diameter + Height + Weight + Shucked.Weight + Viscera.Weight + Shell.Weight + SexI + Height:SexI
# Interestingly, picked only variables that were also in the AIC model, and 7 of the variables are in
# the BIC model. Height:SexI is the only variable picked that is not in the BIC model, but it is in the AIC model

# Backward model with 8 variables:
# Height + Weight + Shucked.Weight + Shell.Weight + SexI + SexF + Height:SexF + Shucked.Weight:SexI

# Since the crab data set is a fairly large data set, ~4000 observations, Kfold and LOOCV are valid
# model testing options
# Since LOOCV high variability in sample predictions and holdout high Bias, use 10-Kfold as a compromise

# AIC model:
crab.fit.AIC = glm(Age ~ Length + Diameter + Height + Weight + Shucked.Weight + Viscera.Weight + Shell.Weight +
                   SexI + SexF + Height:SexF + Shucked.Weight:SexI + Height:SexI, data=CrabData_W_Dummys)
AIC.err <- cv.glm(CrabData_W_Dummys, crab.fit.AIC, K=10)

# BIC model:
crab.fit.BIC = glm(Age ~ Diameter + Height + Weight + Shucked.Weight + Viscera.Weight + Shell.Weight +
                     SexI + SexF + Height:SexF + Shucked.Weight:SexI, data=CrabData_W_Dummys)
BIC.err <- cv.glm(CrabData_W_Dummys, crab.fit.BIC, K=10)

# Forward model:
crab.fit.forward = glm(Age ~ Diameter + Height + Weight + Shucked.Weight + Viscera.Weight + Shell.Weight
                   + SexI + Height:SexI, data=CrabData_W_Dummys)
forward.err <- cv.glm(CrabData_W_Dummys, crab.fit.forward, K=10)

# Backward model:
crab.fit.backward = glm(Age ~ Height + Weight + Shucked.Weight + Shell.Weight + SexI + SexF +
                          Height:SexF + Shucked.Weight:SexI, data=CrabData_W_Dummys)
backward.err <- cv.glm(CrabData_W_Dummys, crab.fit.backward, K=10)

# Full model:
crab.fit.Full = glm(Age~((Length + Diameter + Height + Weight + 
                            Shucked.Weight + Viscera.Weight + Shell.Weight + SexI + SexF) +
                           (Length + Diameter + Height + Weight + 
                              Shucked.Weight + Viscera.Weight + Shell.Weight)*(SexI+SexF)),
                    data=CrabData_W_Dummys)
Full.err <- cv.glm(CrabData_W_Dummys, crab.fit.Full, K=10)

# Compare RMSE values:
sqrt(AIC.err$delta[1])
sqrt(BIC.err$delta[1])
sqrt(forward.err$delta[1])
sqrt(backward.err$delta[1])
sqrt(Full.err$delta[1])


# Can check results with LOOCV and Holdout methods:

## LOOCV 
# AIC model:
  #AIC.err.L <- cv.glm(CrabData_W_Dummys, crab.fit.AIC)

# BIC model:
  #BIC.err.L <- cv.glm(CrabData_W_Dummys, crab.fit.BIC)

# Forward model:
  #forward.err.L <- cv.glm(CrabData_W_Dummys, crab.fit.forward)

# Backward model:
  #backward.err.L <- cv.glm(CrabData_W_Dummys, crab.fit.backward)

# Full model:
  #Full.err.L <- cv.glm(CrabData_W_Dummys, crab.fit.Full)

# Compare RMSE values:
  #sqrt(AIC.err.L$delta[1])
  #sqrt(BIC.err.L$delta[1])
  #sqrt(forward.err.L$delta[1])
  #sqrt(backward.err.L$delta[1])
  #sqrt(Full.err.L$delta[1])

# Previous section takes forever to run, here are the results:
# AIC: 2.173386
# BIC: 2.17584
# Forward: 2.185303
# Backward: 2.187578
#Full: 2.189951

### Holdout (70/30): 
 set.seed(12052023)
 in_train <- createDataPartition(CrabData_W_Dummys$Age, p = 7/10, list = FALSE)
 
 training <- CrabData_W_Dummys[ in_train,]
 holdout  <- CrabData_W_Dummys[-in_train,]
 
 
 crab.fit.AIC_trained = glm(Age ~ Length + Diameter + Height + Weight + Shucked.Weight + Viscera.Weight + Shell.Weight +
                      SexI + SexF + Height:SexF + Shucked.Weight:SexI + Height:SexI, data=training)
 AIC.err.H <- sqrt(mean((holdout$Age-predict(crab.fit.AIC_trained, holdout))^2))
 
 # BIC model:
 crab.fit.BIC_trained = glm(Age ~ Diameter + Height + Weight + Shucked.Weight + Viscera.Weight + Shell.Weight +
                      SexI + SexF + Height:SexF + Shucked.Weight:SexI, data=training)
 BIC.err.H <- sqrt(mean((holdout$Age-predict(crab.fit.BIC_trained, holdout))^2))
 
 # Forward model:
 crab.fit.forward_trained = glm(Age ~ Diameter + Height + Weight + Shucked.Weight + Viscera.Weight + Shell.Weight
                        + SexI + Height:SexI, data=training)
 forward.err.H <- sqrt(mean((holdout$Age-predict(crab.fit.forward_trained, holdout))^2))
 
 # Backward model:
 crab.fit.backward_trained = glm(Age ~ Height + Weight + Shucked.Weight + Shell.Weight + SexI + SexF + Height:SexF
                                 + Shucked.Weight:SexI, data=training)
 backward.err.H <- sqrt(mean((holdout$Age-predict(crab.fit.backward_trained, holdout))^2))
 
 # Full model:
 crab.fit.Full_trained = glm(Age~((Length + Diameter + Height + Weight + 
                             Shucked.Weight + Viscera.Weight + Shell.Weight + SexI + SexF) +
                            (Length + Diameter + Height + Weight + 
                               Shucked.Weight + Viscera.Weight + Shell.Weight)*(SexI+SexF)),
                     data=training)
 Full.err.H <- sqrt(mean((holdout$Age-predict(crab.fit.Full_trained, holdout))^2))

 # Compare values:
 AIC.err.H
 BIC.err.H
 forward.err.H
 backward.err.H
 Full.err.H
 # 4.8519275 + 1.9146033*Diameter + 7.2572292*Height + 0.3339372*Weight 
 # - 0.7081950*Shucked.Weight - 0.3544657*Viscera.Weight +  0.3096686*Shell.Weight - 1.7359903*SexI 
 # + 2.2048661*SexF - 5.7176934*Height:SexF + 0.1479948*Shucked.Weight:SexI
 par(mfrow=c(2,2))
 boxplot(Diameter ~ as.factor(Sexes), CrabData_w_Sexes)
 boxplot(Height ~ as.factor(Sexes), CrabData_w_Sexes)
 boxplot(Weight ~ as.factor(Sexes), CrabData_w_Sexes)
 boxplot(Shucked.Weight ~ as.factor(Sexes), CrabData_w_Sexes)
 par(mfrow=c(2,2))
 boxplot(Viscera.Weight ~ as.factor(Sexes), CrabData_w_Sexes)
 boxplot(Shell.Weight ~ as.factor(Sexes), CrabData_w_Sexes)
 
 
 new <- data.frame(SexI=c(1,0), SexF=c(0,1), Length=c(.9,9), Diameter=c(.95, 1), Height=c(.4, .4), 
                   Weight=c(18, 20), Shucked.Weight=c(8.5, 8.7), Viscera.Weight=c(4.5, 4.7), Shell.Weight=c(5, 5.2))
 predict.glm(crab.fit.BIC, new)
 
 summary(crab.fit.BIC)
 summary(crab.fit.AIC)
 
 # All suggest AIC and BIC as being the 2 best models. BIC is prefered by the Holdout method and AIC by Kfold and LOOCV 
 