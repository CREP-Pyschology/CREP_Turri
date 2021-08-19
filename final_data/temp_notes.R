
#lab id, project id, purpose, previous, materials 


## Descriptive Statistics

In total, [X] labs applied to participate in this multilab replication. [X] labs were unable to participate, [X] did not collect enough data; [X] dropped out prior to data collection, resulting in a final lab count of [X]. Contributing labs represent [X] continents ([X from Africa, X from South America, X from North America, X from Asia, X from Europe, and X from Oceania) with participants residing in [X] countries [X from Brazil, X from Switzerland, X from Singapore, and so on]. [X labs committed to collecting the minimum participant sample size (N = 50), and X labs committed to collecting a larger, more representative sample (N = 100) for the purposes of exploratory analyses. 

```{r}

# number of labs - final
length(unique(valid_data$lab_id))

# number of participants for each lab
table(valid_data$lab_id[duplicated(valid_data$id)])
```

## Analysis

### Knowledge

To assess the model fit of our data, we used the commonly used 
nested model test using maximum likelihood estimation (Snijders & Bosker, 2012). Next, we wanted to determine if the effects of the primary independent variable (belief condition) on knowledge attribution differed by vignette, participant, or lab. To accomplish this, we built an unconditional base model for the knowledge attribution predictor to calculate the intra-class correlation coefficients (ICC) for vignette, participant, and lab variation. The ICCs for vignettes, participants, and labs in the dataset measures the percentage of variation explained by each level, such that vignettes accounted for [X.XX%, 95% CI [X.XX, X.XX] of the raw variation in the dataset, participants accounted for [X.XX%, 95% CI [X.XX, X.XX] of the raw variation in the dataset, and labs accounted for [X.XX%, 95% CI [X.XX, X.XX] of the raw variation in the dataset. 

```{r}
# ICCs vignette
knowledge_vignette <- glmer(var_know_bin ~ (1 | vignette), 
                         data = valid_data,
                         family = binomial, 
                         control = glmerControl(optimizer = "bobyqa"),
                         nAGQ = 1)

summary(knowledge_vignette)

table(valid_data$vignette, valid_data$var_know_bin)

r.squaredGLMM(knowledge_vignette)

# ICCs participants
knowledge_id <- glmer(var_know_bin ~ (1 | id), 
                         data = valid_data,
                         family = binomial, 
                         control = glmerControl(optimizer = "bobyqa"),
                         nAGQ = 1)

summary(knowledge_id)

r.squaredGLMM(knowledge_id)

# ICCs labs
knowledge_lab_id <- glmer(var_know_bin ~ (1 | lab_id), 
                         data = valid_data,
                         family = binomial, 
                         control = glmerControl(optimizer = "bobyqa"),
                         nAGQ = 1)

summary(knowledge_lab_id)

r.squaredGLMM(knowledge_lab_id)


```

chisquare then if the chisquare is significant do the pearsons correlation 
- if the goal is to say they are the same pattern then use the pearson
- if the goal is to say they are different then use chisquare 

```{r}
table(valid_data$var_know_bin, valid_data$vignette)
chisq.test(valid_data$var_know_bin, valid_data$vignette)
```



Given that we are primarily interested in the relationship between the hypothesized level-2 between-subjects predictor (X1) and the two hypothesized outcome variables (knowledge, Y1; and reasonableness, Y2), we first performed the analysis using solely the primary hypothesized independent variable (belief condition) without any other covariates for the purpose of trying to estimate the overall individual level effect (fixed slope) on the primary hypothesized outcome (i.e., null model). In other terms, we determined the effect on knowledge attribution across all samples, not accounting for covariates, vignette differences, or lab differences. We found that the overall effect of belief condition was [insignificant/small/medium/large, β = .XX, 95% CIs [X.XX, X.XX]]. We then tested the model fit for each analysis using likelihood ratio (LR) chi-square difference tests to determine whether each unit level should be tested as a random or fixed factor and whether covariates improved the model (Gelman & Hill, 2007, Chapter 17; see Table 1).

Within these models, vignette was tested as a random (within-subjects) factor, condition was tested as a fixed (between-subjects) factor, and labs were tested as a random (between-subjects) factor. We also fitted these models with several exploratory covariates, including participant gender, years of education, age, and three test setting lab variables (online vs. in person; in group vs. individually; compensated or not compensated).

knowledge_null <- glmer(var_know_bin ~ 1 + (1 | vignette) + 
                           (1 | lab_id) + (1 | lab_id / id), 
                         data = valid_data,
                         family = binomial, 
                         control = glmerControl(optimizer = "bobyqa"),
                         nAGQ = 1)


knowledge_model <- glmer(var_know_bin ~ cond + (1 | vignette) + 
                           (1 | lab_id) + (1 | lab_id / id), 
                         data = valid_data,
                         family = binomial, 
                         control = glmerControl(optimizer = "bobyqa"),
                         nAGQ = 1)

summary(knowledge_model)

table(valid_data$cond, valid_data$var_know_bin)

r.squaredGLMM(knowledge_model)
```

```{r}
knowledge_model2 <- lmer(var_know_vas ~ cond + (1 | vignette) + 
                           (1 | lab_id) + (1 | lab_id / id), 
                    data = valid_data,
                    na.action = "na.omit")

summary(knowledge_model2)

emmeans(knowledge_model2, "cond")

r.squaredGLMM(knowledge_model2)
```

### Reasonableness Attribution

random effects:
- vignette 
- lab
- participant 
- country (may code this several ways)

fixed effects:
- condition
- test setting
- education
- gender
- east versus west (exploratory)