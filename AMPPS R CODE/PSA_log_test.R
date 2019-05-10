

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

install.packages("read_cvs")
library(read_cvs)
pretestBI <- read_csv("binarypretest.csv") # loads clean pretest data - found at https://osf.io/n5b3w/

pretestBI$previg <- as.factor(pretestBI$previg) #sets vignettes as factor 
pretestBI$prepar <- as.factor(pretestBI$prepar) #sets participants as factor 
pretestBI$precon <- as.factor(pretestBI$precon) #sets condition as factor 
pretestBI$biK <- as.factor(pretestBI$biK) #sets knowledge as factor 
pretestBI$biR <- as.factor(pretestBI$biR) #sets reasonableness as factor 

## makes variables object in the environment 

previg <- pretestBI$previg
prepar <- pretestBI$prepar
precon <- pretestBI$precon
prekb <- pretestBI$prekb
biK <- pretestBI$biK
biR <- pretestBI$biR

## for condition (precon), 0 is set to Gettier, 1 to Ignorance, and 2 to Knowledge
## in biK and biR, attributions equal to or more than 50 are coded as 0 (success)
## and attributions less than 50 are coded as 1 (failure) 

## count data 

Gettier <- table(pretestBI$biK, pretestBI$precon == 0)
xg<- Gettier[1,2]
ng <- sum(Gettier[,2])
p <- .5
View(Gettier)
str(Gettier)
summary(Gettier)

binom.test(xg,ng,p)

IgnoranceControl <- table(pretestBI$biK, pretestBI$precon == 1)
xi <- IgnoranceControl[1,2]
ni <- sum(IgnoranceControl[,2])
p <- .5
View(IgnoranceControl)

binom.test(xi,ni,p)

KnowledgeControl <- table(pretestBI$biK, pretestBI$precon == 2)
xk <- KnowledgeControl[1,2]
nk <- sum(KnowledgeControl[,2])
p <- .5
View(KnowledgeControl)

binom.test(xk,nk,p)


m0 <- glmer(biK ~ precon,
            data = pretestBI,
            family = binomial) 

m1 <- glmer(biK ~ precon + (1 | previg) + (1 | prepar),
            data = pretestBI,
            family = binomial) 

pcm1 <- powerCurve(m1, along = "prepar", sim = 10, data = pretestBI)


doTest(m1, fixed("precon", "lr"))

psm1 <- powerSim(m1, fixed("precon", "z"), nsim=10)


m1x1 <- extend(m1, along="prepar", n=2000) ## sets participant N at 2000 which is 6000 observations
nrow(getData(m1x1)) ## check for accuracy 


powerCurve(m1x1, along = "prepar", nsim =10) ## power curve for model 1










##______________________________________________________________________________________##

## DATA SIMULATION AND POWER ANALYSIS 


count <- 360 ## participant count
gcount <- count/3 ## group count 
set.seed(333)

## Below, I use the means and standard deviations of our pretest data to 
## simulate new distributions for this power analysis

## knowledge attribution rates in Gettier Case condition  

G <- c(rep(0,0.55*count),rep(1,0.45*count)) ## sets proportions from pretest
## 0 = Only believes and 1 = Knows
GS <- sample(G) # randomizes list 
prop.table(summary(as.factor(GS)))


## knowledge attribution rates in Knowledge Control condition 

I <- c(rep(0,0.93123*count),rep(1,0.07*count)) ## sets proportions from pretest
## 0 = Only believes and 1 = Knows
IS <- sample(I) # randomizes list 
prop.table(summary(as.factor(IS)))

## knowledge attribution rates in Ignorance Control condition  

K <- c(rep(0,0.23*count),rep(1,0.773*count)) ## sets proportions from pretest
## 0 = Only believes and 1 = Knows
KS <- sample(K) # randomizes list 
prop.table(summary(as.factor(KS)))

## Creates total knowledge DV distribution by combining all conditions:
kbsm =c(GS, KS, IS)

ocount <- 1080 ## total observations 
count <- 360 ## participant count
gcount <- count/3
viga <- c(0, 1, 2) ## first vignette assignment 
vig1 <- rep(viga, times = gcount) 
vigb <- c(2, 0, 1)  ## second vignette assignment
vig2 <- rep(vigb, times = gcount) 
vigc <- c(1, 2, 0)  ## third vignette assignment
vig3 <- rep(vigc, times = gcount) 
vig <- c(vig1, vig2, vig3) # vignette assignment 
con1 <- rep(0, each = count) # Gettier case
con2 <- rep(1, each = count) # knowledge control  
con3 <- rep(2, each = count) # ignorance control 
con <- c(con1, con2, con3) # condition assignment 
lab <- rep(1:9, times = gcount)
par <- rep(1:count, times = 3) # participant count

## creates dataset by binding the above variables: 
bisimdata <-as.data.frame(cbind(con, vig, lab, par, kbsm, 1:ocount)) 

## sets factors 
bisimdata$vig <- as.factor(bisimdata$vig) #sets vignettes as factor 
bisimdata$par <- as.factor(bisimdata$par) #sets participants as factor 
bisimdata$con <- as.factor(bisimdata$con) #sets con as factor
bisimdata$lab <- as.factor(bisimdata$lab) #sets labs as factor 

## summaries and data view 
str(bisimdata)
summary(bisimdata)
View(bisimdata)


# this specifies model parameters: 
b <- c(40, .1, .3) # fixed intercept and slope (low estimate, 
## assuming regression toward mean)
v <- list(0.3, 0.1, 0.2, .2)# random intercept variance 
## (assuming somewhat high variance)
S <- 1 # residual standard deviation

## You can also change the size of the fixed effect, sigma, or VarCorr 
## of the model using these commands:
# fixef(modelA) ["condition"] <- value
# VarCorr(object) <- value
# sigma(object) <- value


M1 <- glmer(kbsm ~ con + (1 | vig), 
            data = bisimdata,
            family = binomial) 
## Note: I  tried using par and lab as random factors, but model was overfit


powerM1 <- powerCurve(M1, along = "par", nsim =100) ## power curve for model 1

