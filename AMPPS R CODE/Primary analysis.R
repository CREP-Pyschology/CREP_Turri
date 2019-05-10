## Install these packages in this order:
install.packages("Matrix") # needed for lme4
install.packages("lme4") # multilevel model package 
install.packages("simr") # data simulation package 
install.packages("nlme") # additional multilevel package 
install.packages("lmerTest") # package for simulating multilevel models 
install.packages("truncnorm") # used to simulate data / generate distributions 
install.packages("reghelper") # used to get standardized betas
install.packages("sjPlot") # used to plot multilevel models 

## load required packages
library(lme4) # capable of creating MLMs 
library(Matrix) # needed for lme4
library(lmerTest) # needed for some lme4 functions 
library(simr) # houses the simulation function simR and powerSim 
library(truncnorm) # can generate truncated distributions 
library(readr) # reads files 
library(reghelper) # standardizes betas 
library(sjPlot) # table functions
library(sjmisc) # sample data
library(lme4) # fitting models

# installing/loading the package:
if(!require(installr)) {
  install.packages("installr"); require(installr)} #load / install+load installr

# using the package:
updateR() 
## this will start the updating process of your R installation.  
## It will check for newer versions, and if one is available, will 
## guide you through the decisions you need to make.

## The following script sets up the values for the dataset: 
## anything with "vig" in it is vignettes (e.g., "Darrel", "Emma")
## anything with "con" in it is condition (e.g., knowledge control, Gettier case)
## anything with "lab" in it is lab 
## anything with "par" in it is participant


## DATA SIMULATION AND POWER ANALYSIS 


count <- 288 ## participant count
gcount <- count/3 ## group count 
set.seed(333)

## Below, I use the means and standard deviations of our pretest data to 
## simulate new distributions for this power analysis

## knowledge attribution rates in Gettier Case condition  
Gs <- rtruncnorm(count, a=0, b=100, mean=58.28, sd= 38.684)

## knowledge attribution rates in Knowledge Control condition  
Ks <- rtruncnorm(count, a=0, b=100, mean=76.96, sd=32.292)

## knowledge attribution rates in Ignorance Control condition  
Is <- rtruncnorm(count, a=0, b=100, mean=8.35, sd = 19.349) 

## Creates total knowledge DV distribution by combining all conditions:
kbsm =c(Gs, Ks, Is)

ocount <- 1080 ## total observations 
count <- ocount/3 ## participant count
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
newsdataset <-as.data.frame(cbind(con, vig, lab, par, kbsm, 1:ocount)) 

## sets factors 
newsdataset$vig <- as.factor(newsdataset$vig) #sets vignettes as factor 
newsdataset$par <- as.factor(newsdataset$par) #sets participants as factor 
newsdataset$con <- as.factor(newsdataset$con) #sets con as factor
newsdataset$lab <- as.factor(newsdataset$lab) #sets labs as factor 

## summaries and data view 
str(newsdataset)
summary(newsdataset)
View(newsdataset)


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

## model to estimate fixed effect of condition 

PCModel<- makeLmer(kbsm ~ con + (1 | vig) + (1|lab) + (1 | lab/par), 
                  fixef = b, VarCorr = v, sigma = S, data = newsdataset)

summary(PCModel)

## POWER ANALYSES 
## WARNING: These commands may take a long time to run, 
## depending on your computer's RAM

cores <- 1 ## set how many of your CPU's cores to use. If you use all
## of your computer's cores, you may not be able to do anything else. 
## Fair warning. 

set.seed(333) ## sets random seed so these results are reproducible 

pC1 <- powerCurve(PCModel, along = "par")
print(pC1)
plot(pC1)

## Power for predictor 'con', (95% confidence interval),
## by number of levels in par:
## 3: 27.20% (24.46, 30.07) - 9 rows
## 43: 22.70% (20.14, 25.42) - 129 rows
## 82: 39.10% (36.06, 42.20) - 246 rows
## 122: 55.50% (52.36, 58.61) - 366 rows
## 162: 69.10% (66.13, 71.95) - 486 rows
## 201: 79.00% (76.34, 81.49) - 603 rows
## 241: 86.10% (83.80, 88.19) - 723 rows
## 281: 90.20% (88.19, 91.97) - 843 rows
## 320: 93.70% (92.01, 95.13) - 960 rows
## 360: 96.50% (95.17, 97.55) - 1080 rows

##Time elapsed: 0 h 43 m 1 s

## You can also change the size of the fixed effect, sigma, or VarCorr 
## of the model using these commands:
# fixef(modelA) ["condition"] <- value
# VarCorr(object) <- value
# sigma(object) <- value

# this specifies model parameters: 
b1 <- c(49.03040, 27.58105, -38.87243) # fixed intercept and slope 
## estimates based on pretest data: 
## intercept: 49.03040  
## Gettier to knowledge condition fixed intercept: 27.58105   
## Gettier to ignorance condition fixed intercept: -38.87243 

b2 <- c(49.03040, 27.58105, -38.87243, 5, 5)
V <- list(.2) # random intercept variance 
v1 <- list(20, 30, 20, 10) # random intercepts for vignettes and labs, respectively
v2 <- list(20,30,20,40,10,20) # used for interaction model 
s <- 29.62546 # residual standard deviation (based on pretest)

## model to estimate fixed effect of condition 

# Model without respect to grouping
m0 <- lm(kbsm ~ con, data=newsdataset)
predict(m0)
newsdataset$simple.model <- predict(m0)

# MAKE model with random intercept for vignette and lab
model1<- makeLmer(kbsm ~ con + (1 | vig) + (1|lab) + (1 | lab/par), 
                  fixef = b1, VarCorr = v1, sigma = s, data = newsdataset)

pcv1 <- powerCurve(model1, along = "par") # plotted in submission 

# Model with varying intercept for lab 
m1 <- lmer(kbsm ~ con + (1|lab), data = newsdataset)
newsdataset$random.intercpet.preds <- predict(m1)

# Visualize random intercept
vary.int.graph <- ggplot(data=newsdataset, aes(x=con, 
  y=random.intercpet.preds, group = lab, colour = lab)) +
  geom_line() + 
  labs(x="Condition", y="Predicted Knowledge Attribution") +
  ggtitle("Varying Intercept Knowledge Prediction") + 
  scale_colour_discrete('Lab')     
print(vary.int.graph)

# Model with varying slope for lab 
m2 <- lmer(kbsm ~ con + (0 + con|lab), data=newsdataset)
newsdataset$random.slope.preds <- predict(m2)

# Visualize random slope
vary.slope.graph <- ggplot(data=newsdataset, aes(x=con, 
  y=random.slope.preds, group = lab, colour = lab)) +
  geom_line() + 
  labs(x="Condition", y="Predicted Knowledge Attribution") +
  ggtitle("Varying Slope Knowledge Prediction") + 
  scale_colour_discrete('Lab')

# Model with varying slope and intercept for lab
m3 <- lmer(kbsm ~ con + (1 + con|lab), data=newsdataset)
newsdataset$random.slope.int.preds <- predict(m3)

## ALTERNATIVE MODEL 3 WITH RANDOM SLOPE AND INTERCEPT
## m3<- makeLmer(kbsm ~ con + (1 + con | lab), 
##                   fixef = b1, VarCorr = v2, sigma = s, data = newsdataset)
## newsdataset$random.slope.int.preds <- predict(m3)

# Visualize random slope + intercept
vary.slope.int.graph <- ggplot(data=newsdataset, aes(x=con, 
  y=random.slope.int.preds, group = lab, colour = lab)) +
  geom_line() + 
  labs(x="Condition", y="Predicted Knowledge Attribution") +
  ggtitle("Varying Slope and Intercept Knowledge Prediction") + 
  scale_colour_discrete('Lab')
print(vary.slope.int.graph)

# Visualize with data
model.with.data <- ggplot(data=newsdataset, aes(x=con, 
  y=random.slope.int.preds, group = lab, colour = lab)) +
  geom_line() + 
  geom_point(aes(x=con, y=kbsm, group = lab, colour = lab)) +
  labs(x="Condition", y="Predicted Knowledge Attribution") +
  ggtitle("Varying Slope and Intercept Knowledge Prediction") + 
  scale_colour_discrete('Lab')
print(model.with.data)


library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

## For the below plot, these commands change the appearance of the plot:
  # The "colors" argument either takes the name of a valid colorbrewer palette 
  # (see also the related vignette), "bw" or "gs" for black/white or 
  # greyscaled colors, or a string with a color name.
  # "value.offset" and "value.size" adjust the positioning and size of 
  # value labels, if shown.
  # "dot.size" and "line.size" change the size of dots and error bars.
  # "vline.color" changes the neutral "intercept" line.
  # "width", alpha and scale are passed down to certain ggplot-geoms, 
  # like geom_errorbar() or geom_density_ridges().

plot_model(
  model1, 
  colors = "gs", 
  show.values = TRUE,
  value.offset = .4,
  value.size = 3,
  dot.size = 4,
  line.size = 1,
  vline.color = "blue",
  width = 1
)

summary(model1)
library(sjPlot)
tab_model(model1, model2)
anova(model1, model2)

## Fixed effects:
##   Estimate Std. Error t value
## (Intercept) 10.00000    0.10380   96.34
## con1        36.54000    0.08333  438.48
## con2        67.02000    0.08333  804.24

nrow(newsdataset)
# [1] 864
length(getME(model1,"theta"))
# [1] 1
length(fixef(model1))
# [1] 3

library(reghelper)
modelsb1 <- beta(m1)

## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: kbsm.z ~ (+con1.z + con2.z) + (1 | vig)
## Data: data
##
## REML criterion at convergence: 1853.4
##
## Scaled residuals: 
## Min      1Q  Median      3Q     Max 
## -2.7531 -0.6651  0.0640  0.6690  3.6364 
## 
## Random effects:
## Groups   Name        Variance Std.Dev.
## vig      (Intercept) 0.1282   0.3581  
## Residual             0.4875   0.6982  
## Number of obs: 864, groups:  vig, 3
## 
## Fixed effects:
## Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)  4.328e-14  2.081e-01  2.000e+00    0.00        1    
## con1.z       3.440e-01  2.744e-02  8.590e+02   12.54   <2e-16 ***
## con2.z      -4.105e-01  2.744e-02  8.590e+02  -14.96   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Correlation of Fixed Effects:
## (Intr) con1.z
## con1.z 0.000        
## con2.z 0.000  0.500 

## Standardized intercept = 4.328e-14 (practically zero) (+/- 0.2081)
## Standardized Beta for difference between Gettier and ignorance = 0.344 (+/- 0.02744)
## Standardized Beta for difference between Gettier and ignorance = -0.4105 (+/- 0.02744)



## Script for Primary Knowledge Attribution Analyses



## fixed or random specifications 

fixedmodel <- lmer(kbsm ~ con + (1 | vig),
                   data = newsdataset) # fully fixed model 

ranlabmodel <- lmer(kbsm ~ con + 1 | vig + (1|lab) + (1 | lab/par), 
     data = newsdataset) # random effect for lab 

anova(fixedmodel, ranlabmodel) # compfixedmodel <- lmer(kbsm ~ con + vig + lab, 
# fully fixed model is compared. Model with smallest BIC will 
# be used to improve model fit in further comparisons 

ranvigmodel <- lmer(kbsm ~ con + (1 | vig) + lab, 
  data = newsdataset) # random effect for vignette/stimulus 

anova(fixedmodel, ranvigmodel) # or anova(ranlabmodel, ranvigmodel) # compares 
# random vignette model to whichever model had best fit from prior test 

ranmodel <- lmer(kbsm ~ con + (1 | vig) + (1|lab) + (1 | lab/par), 
  data = newsdataset) # fully random effect model 

## compare fully random effect model with previous model of best fit 
anova(ranmodel, fixedmodel) 
anova(ranmodel, ranvigmodel)
anova(ranmodel, ranlabmodel)
anova(ranmodel, ranvigmodel)

## add predictors 
null <- lm(kbsm ~ con, data = dataset)

model1 <- lmer(kbsm ~ con + (1 | vig), 
               data = dataset)

anova(null, model1)

model2 <-lmer(kbsm ~ con + (1|lab) + (1 | lab/par), 
             data = dataset)

anova(model1, model2)

model3 <- lmer(kbsm ~ con + (1 | vig) + (1|lab) + (1 | lab/par) + 
                 groupsize + test setting, data = dataset)

anova(model2, model3)

model4 <- lmer(kbsm ~ con + (1 | vig) + (1|lab) + (1 | lab/par) + 
                 groupsize + test setting + education, data = dataset)

anova(model3, model4)

model4 <- lmer(kbsm ~ con + (1 | vig) + (1|lab) + (1 | lab/par) + 
                 groupsize + test setting + education + gender, data = dataset)

anova(model4, model5)


## COVARIATE ANALYSIS 

## We will add in each predictor/covariate one at a time and compare model fit 
## just as we have done in the above model comparisons.
## Model with the smallest BIC will be identified as the model of best fit 


## For author order, authors were split into two groups, minus authors with a set order
## Hall, Wagge, Pfuhl, Steiger, Vergauwe, Hans ... Brandt, Chartier, & Grahe: 
## Group A - provided feedback during manuscript construction/design planning 
## Group B - only committed to providing data
## Group A and Group B will be randomized independently and the subsequent author order 
## will be the randomized order of Group A followed by the randomized order of Group B

AuthorOrder <- read_csv("AuthorO.csv")
set.seed(333)
RandAuthor <- AuthorOrder[sample(1:nrow(AuthorOrder)), ]
View(RandAuthor)
install.packages("xlsx")
library(xlsx)
write.xlsx(RandAuthor, "C:/Users/B/Documents/Randomized Author Order.xlsx")
