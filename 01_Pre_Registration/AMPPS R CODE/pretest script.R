## Install these packages in this order:
install.packages("Matrix")
install.packages("lme4")
install.packages("simr")
install.packages("nlme")
install.packages("lmerTest")
install.packages("truncnorm")
install.packages("ggplot2")
install.packages("HLMdiag")
install.packages("DHARMa")
install.packages("car")
install.packages("lm.beta")

library("lme4")
library("Matrix")
library("lmerTest")
library("simr")
library("truncnorm")
library("readr")
library("ggplot2")
library("HLMdiag")
library("DHARMa")
library("car") #for the Levene test 
library("lm.beta")

set.seed(333) # sets seed for reproducible randomization 

pretestkb <- read_csv("pretest.csv") # loads clean pretest data - found at https://osf.io/n5b3w/

library(foreign)
FULLpretestdata = read.spss("C:\\Users\\B\\Documents\\Gerald Emma pretest clean.sav", 
                                to.data.frame=TRUE)

pretestkb$previg <- as.factor(pretestkb$previg) #sets vignettes as factor 
pretestkb$prepar <- as.factor(pretestkb$prepar) #sets participants as factor 
pretestkb$precon <- as.factor(pretestkb$precon) #sets condition as factor 

## makes variables object in the environment 

previg <- pretestkb$previg
prepar <- pretestkb$prepar
precon <- pretestkb$precon
prekb <- pretestkb$prekb

## for condition (precon), 0 is set to Gettier, 1 to knowledge, and 2 to ignorance 

## simple visual comparison across conditions 

plot(pretestkb$precon, pretestkb$prekb)

## creates null model 
modelpreF <- lmer(prekb ~ precon + (1|prepar), data = pretestkb)
summary(modelpreF) # fixed effects model 

nrow(pretestkb)
# [1] 495
length(getME(modelpreF,"theta"))
# [1] 1
length(fixef(modelpreF))
# [1] 3

## sets function to find standardized betas and errors for fixed effect of condition 
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

library("lme4")

## get unstandardized betas and intercept 
fixef(modelpreF)

## (Intercept)     precon1     precon2 
## 49.03040        27.58105   -38.87243 

## get standardized betas and errors 
stdCoef.merMod(modelpreF)

## stdcoef      stdse
## (Intercept)  0.0000000 0.00000000
## precon1      0.3173116 0.03990607
## precon2     -0.4392050 0.03986466

sigma(modelpreF)
## [1] 29.62546

modelpreF2 <- lmer(prekb ~ precon + (1|prepar), data = pretestkb)
summary(modelpreF2) ## fixed effects model without fixed effect for vignette 

anova(modelpreF, modelpreF2)

## Test of model fit between the two fixed effects models:

## Data: pretestkb
## Models:
## modelpreF2: prekb ~ precon + (1 | prepar)
## modelpreF: prekb ~ precon + previg + (1 | prepar)
##             Df AIC     BIC      logLik   deviance  Chisq Chi Df Pr(>Chisq)    
## modelpreF2  5  4803.0  4824.0  -2396.5   4793.0                             
## modelpreF   7  4762.7  4792.1  -2374.3   4748.7    44.314    2  2.384e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## The first fixed effect model appears to have the best model fit 

modelpreR <- lmer(prekb ~ precon + (1|previg) + (1|prepar), data = pretestkb)
summary(modelpreR) # random effects model  

## Test of model fit between fixed and random effects models: 

anova(modelpreF, modelpreR)

## Models:
## modelpreR (random effects model): prekb ~ precon + (1 | previg) + (1 | prepar)
## modelpreF (fixed effects model): prekb ~ precon + previg + (1 | prepar)
##          Df  AIC    BIC   logLik   deviance  Chisq Chi  Df  Pr(>Chisq)    
##modelpreR  6 4771.6 4796.8 -2379.8   4759.6                             
##modelpreF  7 4762.7 4792.1 -2374.3   4748.7   10.903      1  0.0009603 ***
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## It appears that the fixed effect model including vignettes fits model best.

## Just in case, I compared the model fit of a single level linear model 
## to the fixed effect models: 

modellm <- lm(prekb ~ precon, data = pretestkb)

anova(modelpreF2, modellm)

## results indicate that modelpreF2 fits better (p = .01)

anova(modelpreF, modellm)

## results indicate that modelpreF fits better (p < .00)
## Therefore, for the assumption checks below, this model will be used 

## Assumption checks were done using 
## this tutorial: https://ademos.people.uic.edu/Chapter18.html

## Assumption Check 1: Linearity

ranef(modelpreF)

prekb <- pretestkb$prekb # takes knowledge attribution column from data and
# and turns it into an object

residuals <- c(resid(modelpreF), 1) # model residuals 
## I could not figure out why resid(modelpreF) kept giving me only 494 objects
## instead of 495, but couldn't figure it out. Without the matrix of residuals
## being the same length as the matrix of knowledge attribution scores, 
## I could not plot them. So, I simply added a 1 to the end of the residuals 
## matrix. I highly doubt this should effect this visual test though. 
               
Plot.modelpreR.Linearity <-plot(residuals, prekb) # plots residuals against observed 

## Levine's Test of Homogeneity of Variance

pretestkb$modelpreF.Res<- residuals #extracts the residuals and places 
# them in a new column in our original data table
pretestkb$Abs.modelpreF.Res <-abs(pretestkb$modelpreF.Res) #creates a new 
# column with the absolute value of the residuals
pretestkb$modelpreF.Res2 <- pretestkb$Abs.modelpreF.Res^2 #squares the 
# absolute values of the residuals to provide the more robust estimate
Levene.modelpreF <- lm(modelpreF.Res2 ~ prepar, data = pretestkb) #ANOVA of 
# the squared residuals
anova(Levene.modelpreF) #displays the results

## Analysis of Variance Table

## Response: modelpreF.Res2
##           Df  Sum Sq     Mean Sq   F value   Pr(>F)
## prepar    164 183865477  1121131   1.0984    0.238
## Residuals 330 336826277  1020686   

## Since the p value is greater than 0.05, we can say that the variance of the 
## residuals is equal and therefore the assumption of homoscedasticity is met

## Visual check of Homogeneity of Variance
Plot.modelpreF <- plot(modelpreF) #creates a fitted vs residual plot
Plot.modelpreF

## test of normally distributed residuals 

##qq plot (similar to a diagnostic plot provided by the lm function) 
## for an estimation of the linearity of the residuals
require("lattice")

qqmath(modelpreF, id=0.001) #id: identifies values that may be exerting 
## undue influence on the model (i.e. outliers)

## There is some deviation from from the expected normal line towards the tails, 
## but overall the line looks straight and therefore pretty normal and suggests 
## that the assumption is not violated. 

## Additional test for normality of residuals 

shapiro.test(residuals)

## Shapiro-Wilk normality test

## data:  residuals
## W = 0.99697, p-value = 0.4917

## All 3 assumptions seem to have been met by this data 