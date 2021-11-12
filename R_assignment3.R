# Clean and set working directory
rm(list=ls())
setwd("C:/Users/Jean Pierre/Documents/0-Local Workstation/0-RUG/0-courses/1a courses/Multivariate/Assignments/Assignment3/docs_ddbb")

set.seed(2021) # to reproduce results when using random numbers

# PACKAGES-----------------------
# install.packages("haven")
# install.packages("lavaan")
# install.packages("psych")
# install.packages("GPArotation")
# install.packages("polycor")
# install.packages("DescTools")
# install.packages("ResourceSelection")
# install.packages("ggeffects")
# install.packages("moments")
# install.packages("Hmisc")
# install.packages("pastecs")
# install.packages("car")
# install.packages("MASS")
#install.packages("lmtest")  # To use the likelihood ratio test lrtest()

#Replication packages
#install.packages("tidyverse")
#install.packages("caret")
library(tidyverse)
library(caret)


# Loading libraries-----------------------
library(haven)
library(lavaan)
library(psych)
library(GPArotation)
library(polycor)
library(DescTools)
library(ResourceSelection)
library(ggeffects)        
library(moments) # to assess skewness and kurtosis
library(Hmisc) # to use the describe fx
library(pastecs) # to test for normality w/ shapiro-wilk
library(car) # durbin-watson test for autocorrelation of errors + VIF
library(MASS)
library(lmtest) # likelihood ratio test between nested models


### Load the data base
db0 <- read_sav("MVDA_Assignment3_FA&LRA_ProductReturns(1).sav")

### preserve all output numbers as decimals
options(scipen = 9)


# Checking ranges and distribution of variables
head(db0)
stat.desc(db0) #getting means, sd, median, min max ranges to check ok values
skewness(db0) #getting assymetry

#replace Gender coding to 0 = male and 1 = female
db0$gender_recode <- db0$Gender
db0$gender_recode[which(db0$gender_recode == 1)] <- 0  # 1 are males and 2 are females -> 0 should be males
db0$gender_recode[which(db0$gender_recode == 2)] <- 1  # and now 1 are females
table(db0$gender_recode, db0$Gender) # 893 men coded as 1 are now coded as 0 and 5727 women coded as 2 are now coded as 1
db0$female <- db0$gender_recode
#Getting the right statistics for female proportion
stat.desc(db0$female)
skewness(db0$female)

#tabulating categorical variables: ProductCategory and Style
table(db0$ProductCategory)
table(db0$Style)

# Getting the polychoric correlation matrix bc there are dummies in there
polymatrix <- cbind(db0$female, db0$Age, db0$OrderAmount, db0$OrderValue, db0$ItemAmount, db0$ItemPrice, db0$ItemDiscount, db0$ItemReturn,
                    db0$Att1, db0$Att2, db0$Att3, db0$Att4, db0$Att5, db0$Att6, db0$Att7, db0$Att8, db0$Att9, db0$Att10, db0$Att11)
likerts    <- cbind(db0$Att1, db0$Att2, db0$Att3, db0$Att4, db0$Att5, db0$Att6, db0$Att7, db0$Att8, db0$Att9, db0$Att10, db0$Att11)


cor(polymatrix, method = c("spearman"))

#Inspecting problem cases based on descriptive statistics
##1) assymetry in ages
age_dens <- density(db0$Age)
plot(age_dens) ## left skew with two peaks at around 26 and 46 years old
##OK

##2) assymetry in products in the basket
basket_dens <- density(db0$OrderAmount)
plot(basket_dens)
##Ok monotonous decrease

##3) the minimum value for certain baskets is low
describe(db0$OrderValue) ## Ok, it is possible to have baskets worth 1 euro. lowest values ok from 1 to 7.99 OK
basket_val_dens <- density(db0$OrderValue)
plot(basket_val_dens)
## Assymetric to the right consider log


##4) concern is about zero price of items
describe(db0$ItemPrice) # some observations have zero price for the item
db0$zero_price <- 0
db0$zero_price[which(db0$ItemPrice == 0)] <- 1
table(db0$zero_price) ## 15 observations have paid zero euros for the item
table(db0$zero_price, db0$ItemDiscount) ## In all 15 cases with price zero there was a discount applied. All ok, the obs stay.
price_dens <- density(db0$ItemPrice)
plot(price_dens)
##OK


##Exploratory PCA to uncover dimensions in item responses
### Looking at the items, there seems to be 4 themes: 
### (1) loyalty to online shops: items 1, 4, 9 and 10
### (2) dependence to online shopping: items 6, 7 and 11
### (3) price relevance: items 2, 3 and 8
### (4) trust in online shopping: only item 5

#Bartlett's test for sphericity (identity) of the correlation matrix
items_matrix <- cor(likerts, method = c("pearson"))
cortest.bartlett(items_matrix, n = 6620) ## Result of the Bartlett test for sphericity (identity) is Chi2: 29230.95, df: 55, sig<.001

# KMO MSA criterion
KMO(items_matrix)

#PCA is warranted, carrying on
principal(likerts , nfactors = 4 , rotate ="none", scores = TRUE , covar = FALSE)
principal(likerts , nfactors = 4 , rotate ="varimax", scores = TRUE , covar = FALSE)

#Horn test to check if 4 factors is ok
par(mar=c(1,1,1,1))
horn_test <- fa.parallel(likerts, fa="both", fm="ml", cor="cor", n.iter=1000)
plot(horn_test)


#Eigenvalues and Horn's test suggest three factors. Let's see where trust will be forced to be (item 5)
principal(likerts , nfactors = 3 , rotate ="none", scores = TRUE , covar = FALSE)
principal(likerts , nfactors = 3 , rotate ="varimax", scores = TRUE , covar = FALSE)

## Ok so item 5 is out as its loading is poor.
likerts_v2    <- cbind(db0$Att1, db0$Att2, db0$Att3, db0$Att4, db0$Att6, db0$Att7, db0$Att8, db0$Att9, db0$Att10, db0$Att11)

#Last PCA to check one last time
principal(likerts_v2 , nfactors = 3 , rotate ="none", scores = TRUE , covar = FALSE)
horn_test <- fa.parallel(likerts_v2, fa="both", fm="ml", cor="cor", n.iter=1000)
plot(horn_test)
## Horn test suggest that 3 components are enough.
## OK we are set then
#Component 1 is items 1, 4, 9 and 10
#Component 2 is items 6, 7 and 11
#Component 3 is items 2, 3 and 8

## testing reliability of the three components
### Component 1
component1 <- cbind(db0$Att1, db0$Att4, db0$Att9, db0$Att10)
psych::alpha(component1)

### Component 2
component2 <- cbind(db0$Att6, db0$Att7, db0$Att11)
psych::alpha(component2)

### Component 3
component3 <- cbind(db0$Att2, db0$Att3, db0$Att8)
psych::alpha(component3)



# Extracting the scores from the PCA with varimax rotation
pca_final <- principal(likerts_v2 , nfactors = 3 , rotate ="varimax", scores = TRUE , covar = FALSE)
pca_scores <- pca_final$scores
db0$final_score_c1 <- pca_scores[,1] # scores for component 1
db0$final_score_c2 <- pca_scores[,2] # scores for component 2
db0$final_score_c3 <- pca_scores[,3] # scores for component 3
# Checking that their correlation is zero
components <- cbind(db0$final_score_c1, db0$final_score_c2, db0$final_score_c3) # binding all three scores in a single group variable
cor(components,  method = c("pearson")) # correlations are zero. OK

# Scores for Component 1
summary(db0$final_score_c1)

# Scores for Component 2
summary(db0$final_score_c2)

# Scores for Component 3
summary(db0$final_score_c3)



# Creating sumscores and comparing their univariate statistics and correlations to pick the sumscores in the end.

## sumscores for the three components
### C1: Online shops loyalty
db0$sum_c1 <- db0$Att1 + db0$Att4 + db0$Att9 + db0$Att10
db0$sum_c2 <- db0$Att6 + db0$Att7 + db0$Att11
db0$sum_c3 <- db0$Att2 + db0$Att3 + db0$Att8

## getting sums and component scores together
sums_scores <- cbind(db0$sum_c1, db0$sum_c2, db0$sum_c3, db0$final_score_c1, db0$final_score_c2, db0$final_score_c3)

## Descriptive statistics and correlations
stat.desc(sums_scores)
skewness(sums_scores)
cor(sums_scores, method = c("pearson"))



## Logit model 0: starting with the intercept
logit0 <- glm(ItemReturn ~ 1, data=db0, family = binomial(link = "logit"))
logit0
summary(logit0)
table(db0$ItemReturn)
prop_returns <- 875/(875+5745)
prop_returns ## Proportion of returns is 13.22%  (0.1321752)
#-----------------------
# Summary of model 0:
#   1) the intercept model should lead to the sample returns proportion
#   2) b0 = -1.88186 
#   3) Probability is: exp(-1.88186)/(1+exp(-1.88186)) = .1321754
#-----------------------

## Logit model 1: adding one binary independent var
logit1 <- glm(ItemReturn ~ 1 + female, data=db0, family=binomial(link="logit"))
summary(logit1)
lrtest(logit0, logit1) ## significantly better (1 var added, discount)
#-----------------------
# Summary of model 1:
#   1) This model assess the difference of males vs females in the item return prob
#   2) b0 = -2.3749 // This is the log of odds for returning a product being male 
#   3) Probability is: exp(-2.3749)/(1+exp(-2.3749)) = 8.5% (0.08510683)
#   4) The coefficient for females
#   4.1) b1 = 0.5556 (this is the increase in the log of odds ratio)
#   4.2) exponentiating gives the increase in the odds ratio
#   4.3) exp(0.5556) = 1.742986, then substract 1 and x100% gives the magnitude
#   4.4) Being a female customer increases the odds ratio of returning the item
#         by 74.30%
#-----------------------

## Logit model 2: single continuous independent var
logit2 <- glm(ItemReturn ~ 1 + OrderValue, data=db0, family=binomial(link="logit"))
summary(logit2)
lrtest(logit2, logit0) # model is better than baseline
#-----------------------
# Summary of model 2:
#   1) This model assess the effect of basket value (euro) on returning items
#   2) b0 = -2.497 // This is the log of odds for returning a product at zero euro 
#   3) Probability is: exp(-2.497)/(1+exp(-2.497)) = 7.6% (0.07606876)
#   4) The coefficient for value of the basket in euros is:
#   4.1) b1 = 0.0068038 (this is the increase in the log of odds)
#   4.2) exponentiating gives the increase in the odds
#   4.3) exp(0.0068038) = 1.006827, then substract 1 and x100% gives the magnitude
#   4.4) for a one euro increase in basket value, we expect a
#         0.68% increase in the odds of returning the item
#-----------------------

## Logit model 3: multiple independent variables
logit3 <- glm(ItemReturn ~ 1 + female + OrderValue + sum_c2, 
              data = db0, family=binomial(link="logit"))
summary(logit3)
lrtest(logit3, logit0) ## difference is statistically significant
#-----------------------
# Summary of model 3:
#   1) Modelling the effect of female, basket value and score in the 
#   2) b0 = -3.1374006 // log odds when all is zero
#   3) Prob: exp(-3.1374006)/(1+exp(-3.1374006)) = 4.16% (0.04159061)
#   4) Coefficients (logodds) and change in odds
#       4.1) female:        b1=0.5988332 -> ((exp(b1)-1)x100% -> 82.00% increase 
#       4.2) basket value:  b2=0.0061635 -> ((exp(b2)-1)x100% ->  0.61% increase
#       4.3) habit score:   b3=0.0113312 -> ((exp(b3)-1)x100% ->  1.14% increase
#     increase in the odds of returning the item vs not returning them
#-----------------------

# Checking the CI's and the exponentiated logOdds coefficients
confint(logit3)
exp(confint(logit3))
exp_coef_logit3 <- exp(logit3$coefficients)
list(exp_coef_logit3)

prediction_logit3 <- ggpredict(logit3)
prediction_logit3 ## NICE WAY TO GET THE ACTUAL PROBABILITIES!!!!

#checking correct prediction of cases
db0$return_prediction <- predict(logit3, type="response")
db0$return_pr_yn_v50  <- ifelse(return_prediction > 0.5, 1, 0) 
db0$return_pr_yn_v25  <- ifelse(return_prediction > 0.25, 1, 0) 
db0$return_pr_yn_v13  <- ifelse(return_prediction > 0.125, 1, 0) 

# Checking accuracy at 50% threshold (v50)
table(db0$return_pr_yn_v50, db0$ItemReturn) ## correctly id returns: 0.5%
mean(db0$return_pr_yn_v50 == db0$ItemReturn) ## diagonal is 86.6% correct

# Checking accuracy at 25% threshold (v25)
table(db0$return_pr_yn_v25, db0$ItemReturn) ## correctly id returns: 13.7%
mean(db0$return_pr_yn_v25 == db0$ItemReturn) ## diagonal is 85.42% correct

# Checking accuracy at 12.5% threshold (v13)
table(db0$return_pr_yn_v13, db0$ItemReturn) ## correctly id returns: 57.49%
mean(db0$return_pr_yn_v13 == db0$ItemReturn) ## diagonal is 61.17% correct

#checking categorical vars
table(db0$ProductCategory, db0$ItemReturn)

table(db0$Style, db0$ItemReturn)


# Splitting the dataset into training and cross-validation
#----------------------------------------------------------
## 1) Generate a unique identifier for each observation
db0$unique_id <- 1:nrow(db0)
## 2) Set seed again and get the 20% subsample for cross-validation
set.seed(2021) # to reproduce results when using random numbers
db0_validate <- db0[sample(1:nrow(db0), 1324, replace=FALSE),] # 20% x 6620= 1324
id_obs <- db0_validate$unique_id
db0$in_holdout <- 0
db0$in_holdout[which(is.element(db0$unique_id, id_obs) == TRUE)] <- 1
summary(db0$in_holdout)
table(db0$in_holdout) ## 1324 obs in holdout (cross-validate) 5296 to train




### Training model 1 - intercept only
logmodel1 <- glm(ItemReturn ~ 1 , data = subset(db0, db0$in_holdout == 0),
                 family = binomial(link = "logit")) ## only n= 5296
summary(logmodel1)
PseudoR2(logmodel1, which="all")

#### GETTING THE PREDICTIONS FOR LOGMODEL 1
prediction_logmodel1 <- ggpredict(logmodel1)
#checking correct prediction of cases
item_return <- db0$ItemReturn[which(db0$in_holdout == 0)]
ovl_pred_logmodel1 <- predict(logmodel1, type="response")
dicot_predict_logmod1_50  <- ifelse(ovl_pred_logmodel1 > 0.5, 1, 0) 
dicot_predict_logmod1_25  <- ifelse(ovl_pred_logmodel1 > 0.25, 1, 0) 
#checking 50% threshold accuracy
table(dicot_predict_logmod1_50, item_return) ## 
mean(dicot_predict_logmod1_50 == item_return) ## diagonal is 86.57% correct
#checking 25% threshold accuracy
table(dicot_predict_logmod1_25, item_return) ## 
mean(dicot_predict_logmod1_25 == item_return) ## diagonal is 86.57% correct








### Training model 2 - adding personal characteristics
logmodel2 <- glm(ItemReturn ~ 1 + Age + female, 
                 data = subset(db0, db0$in_holdout == 0),
                 family = binomial(link = "logit"))
summary(logmodel2)
lrtest(logmodel2, logmodel1) #sig
PseudoR2(logmodel2, which="all")

#### GETTING THE PREDICTIONS FOR LOGMODEL 2
prediction_logmodel2 <- ggpredict(logmodel2)
#checking correct prediction of cases
ovl_pred_logmodel2 <- predict(logmodel2, type="response")
dicot_predict_logmod2_50  <- ifelse(ovl_pred_logmodel2 > 0.5, 1, 0) 
dicot_predict_logmod2_25  <- ifelse(ovl_pred_logmodel2 > 0.25, 1, 0) 
#checking 50% threshold accuracy
table(dicot_predict_logmod2_50, item_return) ## 
mean(dicot_predict_logmod2_50 == item_return) ## diagonal is 86.57% correct
#checking 25% threshold accuracy
table(dicot_predict_logmod2_25, item_return) ## 
mean(dicot_predict_logmod2_25 == item_return) ## diagonal is 86.57% correct






### Training model 3 - adding basket characteristics
logmodel3 <- glm(ItemReturn ~ 1 + Age + female + OrderAmount, 
                 data = subset(db0, db0$in_holdout == 0),
                 family = binomial(link = "logit"))
summary(logmodel3)
lrtest(logmodel3, logmodel1) #sig
lrtest(logmodel3, logmodel2) #sig
PseudoR2(logmodel3, which="all")
#### GETTING THE PREDICTIONS FOR LOGMODEL 3
prediction_logmodel3 <- ggpredict(logmodel3)
#checking correct prediction of cases
ovl_pred_logmodel3 <- predict(logmodel3, type="response")
dicot_predict_logmod3_50  <- ifelse(ovl_pred_logmodel3 > 0.5, 1, 0) 
dicot_predict_logmod3_25  <- ifelse(ovl_pred_logmodel3 > 0.25, 1, 0) 
#checking 50% threshold accuracy
table(dicot_predict_logmod3_50, item_return) ## 
14/(4517+14) # false positive --> 0.31%
707/(707+4)  # false negative --> 99.44%
mean(dicot_predict_logmod3_50 == item_return) ## diagonal is 86.39% correct
#checking 25% threshold accuracy
table(dicot_predict_logmod3_25, item_return) ## 
76/(76+4509) # false positive --> 1.66%
685/(685+26) # false negative --> 96.34%
mean(dicot_predict_logmod3_25 == item_return) ## diagonal is 85.63% correct


### Training model 4 - Adding item characteristics
logmodel4 <- glm(ItemReturn ~ 1 + Age + female + OrderAmount + ItemAmount +
                   ItemPrice + ItemDiscount, 
                 data = subset(db0, db0$in_holdout == 0),
                 family = binomial(link = "logit"))
summary(logmodel4)
lrtest(logmodel4, logmodel1) #sig
lrtest(logmodel4, logmodel3) #sig
PseudoR2(logmodel4, which="all")
#### GETTING THE PREDICTIONS FOR LOGMODEL 4
prediction_logmodel4 <- ggpredict(logmodel4)
#checking correct prediction of cases
ovl_pred_logmodel4 <- predict(logmodel4, type="response")
dicot_predict_logmod4_50  <- ifelse(ovl_pred_logmodel4 > 0.5, 1, 0) 
dicot_predict_logmod4_25  <- ifelse(ovl_pred_logmodel4 > 0.25, 1, 0) 
#checking 50% threshold accuracy
table(dicot_predict_logmod4_50, item_return) ## 
14/(4517+14) # false positive --> 0.31%
707/(707+4)  # false negative --> 99.44%
mean(dicot_predict_logmod4_50 == item_return) ## diagonal is 86.39% correct
#checking 25% threshold accuracy
table(dicot_predict_logmod4_25, item_return) ## 
109/(109+4476) # false positive --> 2.38%
668/(668+43) # false negative --> 93.95%
mean(dicot_predict_logmod4_25 == item_return) ## diagonal is 85.33% correct


### Training model 5 - Adding psychometric scales
logmodel5 <- glm(ItemReturn ~ 1 + Age + female + OrderAmount + ItemAmount +
                   ItemPrice + ItemDiscount + sum_c1 + sum_c2 + sum_c3, 
                 data = subset(db0, db0$in_holdout == 0),
                 family = binomial(link = "logit"))
summary(logmodel5)
lrtest(logmodel5, logmodel1) #sig
lrtest(logmodel5, logmodel4) #sig
PseudoR2(logmodel5, which="all")
## Testing to see if interaction terms are warranted
db2 <- db0[which(db0$in_holdout == 0),]
logmodel5_1 <- glm(ItemReturn ~ 1 + Age + female + OrderAmount + ItemAmount +
                   ItemPrice + ItemDiscount + sum_c1 + sum_c2 + sum_c3, 
                 data = subset(db0, db0$in_holdout == 0),
                 family = binomial(link = "logit"))
summary(logmodel5_1)
hoslem.test(db2$ItemReturn,fitted(logmodel5_1)) ## current specification is enough


#### GETTING THE PREDICTIONS FOR LOGMODEL 5
prediction_logmodel5 <- ggpredict(logmodel5)
#checking correct prediction of cases
ovl_pred_logmodel5 <- predict(logmodel5, type="response")
dicot_predict_logmod5_50  <- ifelse(ovl_pred_logmodel5 > 0.5, 1, 0) 
dicot_predict_logmod5_25  <- ifelse(ovl_pred_logmodel5 > 0.25, 1, 0) 
#checking 50% threshold accuracy
table(dicot_predict_logmod5_50, item_return) ## 
9/(9+4576)  # false positive  0.20%
707/(707+4) # false negative 99.44%
mean(dicot_predict_logmod5_50 == item_return) ## diagonal is --.-% correct
#checking 25% threshold accuracy
table(dicot_predict_logmod5_25, item_return) ## 
143/(143+4442) # false positive  3.12%
641/(641+70)   # false negative 90.15%
mean(dicot_predict_logmod5_25 == item_return) ## diagonal is --.-% correct



### Final analysis: checking predictive cap. on holdout sample
predictions <- logmodel5 %>% predict(subset(db0,db0$in_holdout == 1))
returns_holdout <- db0$ItemReturn[which(db0$in_holdout == 1)]
predictions_50 <- ifelse(predictions > 0.5, 1, 0) 
predictions_25 <- ifelse(predictions > 0.25, 1, 0) 
#checking hit-rate for holdout sample at 50% threshold
table(predictions_50, returns_holdout)
1/(1+1159) # false positives  0.09%}
164/(164+0) # false negatives 100%
mean(predictions_50 == returns_holdout) ## diagonal is 87.54% correct
#checking hit-rate for holdout sample at 25% threshold
table(predictions_25, returns_holdout)
2/(2+1158) # false positives  0.09%}
163/(163+1) # false negatives 100%
mean(predictions_25 == returns_holdout) ## diagonal is 87.54% correct



## Getting the odds variation and CI
# Checking the CI's and the exponentiated logOdds coefficients
summary(logmodel5)
confint(logmodel5)
exp(confint(logmodel5))
odds_var_logmodel5 <- exp(logmodel5$coefficients)
list(odds_var_logmodel5)

forecast_logmodel5 <- ggpredict(logmodel5)
forecast_logmodel5  ## GET THE ACTUAL PROBABILITIES










