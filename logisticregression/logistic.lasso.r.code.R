#######################################################################################################################
# Logistic and LASSO Logistic Regression Models
# Shery Cheong
# November 1st 2015

# set wd, import data
setwd("~/Desktop")
my.data <- data.frame(read.table("TrainingSet10K_101015.txt",sep=",",header=T))

#run clean data function (import the function from the clean_data.R script and run it)
my.data.clean <- clean_data(my.data)

# find percentage of missing variables
pMiss <- function(x){sum(is.na(x))/length(x)*100}
missing.percent <- data.frame(apply(my.data.clean,2,pMiss))
names(missing.percent) <- c("percent_missing")

# export list of missing variables
missing.subset <-subset(missing.percent,percent_missing>0)

# create function to clean up all missing variables and export cleaned dataframe

remove_missing <- function(my.data.clean){
  
  my.data.clean$DOB_Year[is.na(my.data.clean$DOB_Year)] <- mean(my.data.clean$DOB_Year[!is.na(my.data.clean$DOB_Year)])
  
  my.data.clean$PABS[is.na(my.data.clean$PABS)] <- "N"
  
  my.data.clean$CONS_CREDITCARDCOUNT_HH[is.na(my.data.clean$CONS_CREDITCARDCOUNT_HH)] <- 0
  
  my.data.clean$CONS_CREDITCARDCOUNT[is.na(my.data.clean$CONS_CREDITCARDCOUNT)] <- 0
  
  my.data.clean$CONS_EDUCATION[is.na(my.data.clean$CONS_EDUCATION)] <-0
  
  my.data.clean$DEMO_CHILDCNT[is.na(my.data.clean$DEMO_CHILDCNT)] <-0
  
  # my.data.clean$DEMO_MARITALSTATUS[is.na(my.data.clean$DEMO_MARITALSTATUS)] <- "U"
  
  # Add new category "U"
  # levels(my.data.clean$DEMO_RELIGION_CODE) <- c(levels(my.data.clean$DEMO_RELIGION_CODE),"U")
  # my.data.clean$DEMO_RELIGION_CODE[is.na(my.data.clean$DEMO_RELIGION_CODE)] <- "U"
  
  # my.data.clean$DEMO_WORKATHOME[is.na(my.data.clean$DEMO_WORKATHOME)] <- "N"
  
  # replace unknowns w/ mean value of home
  my.data.clean$HOME_ACTUALVALUE[is.na(my.data.clean$HOME_ACTUALVALUE)] <- mean(my.data.clean$HOME_ACTUALVALUE[!is.na(my.data.clean$HOME_ACTUALVALUE)])
  
  my.data.clean$HOME_BEDCNT[is.na(my.data.clean$HOME_BEDCNT)] <- mean(my.data.clean$HOME_BEDCNT[!is.na(my.data.clean$HOME_BEDCNT)])
  
  # levels(my.data.clean$HOME_EQUITYESTRANGE) <- c(levels(my.data.clean$HOME_EQUITYESTRANGE),"U")
  # my.data.clean$HOME_EQUITYESTRANGE[is.na(my.data.clean$HOME_EQUITYESTRANGE)] <- "U"
  
  my.data.clean$HOME_HOMEAGE[is.na(my.data.clean$HOME_HOMEAGE)] <- mean(my.data.clean$HOME_HOMEAGE[!is.na(my.data.clean$HOME_HOMEAGE)])
  
  my.data.clean$HOME_INTRATE[is.na(my.data.clean$HOME_INTRATE)] <- mean(my.data.clean$HOME_INTRATE[!is.na(my.data.clean$HOME_INTRATE)])
  
  my.data.clean$HOME_LOTSIZE[is.na(my.data.clean$HOME_LOTSIZE)] <- mean(my.data.clean$HOME_LOTSIZE[!is.na(my.data.clean$HOME_LOTSIZE)])
  
  # my.data.clean$HOME_MORTGAGE[is.na(my.data.clean$HOME_MORTGAGE)] <- "N"
  
  # create new factor "U"
  # levels(my.data.clean$HOME_VALUEESTIMATECODE) <- c(levels(my.data.clean$HOME_VALUEESTIMATECODE),"U")
  # my.data.clean$HOME_VALUEESTIMATECODE[is.na(my.data.clean$HOME_VALUEESTIMATECODE)] <- "U"
  
  my.data.clean$HOME_YEARBLT[is.na(my.data.clean$HOME_YEARBLT)] <- mean(my.data.clean$HOME_YEARBLT[!is.na(my.data.clean$HOME_YEARBLT)])
  
  my.data.clean$BHAV_INTERNET_ADS[is.na(my.data.clean$BHAV_INTERNET_ADS)] <- 0
  
  # This variable causes issues with too many factors, and not all of them being present in both test and train groups, so I removed it.
  my.data.clean$DEMO_ORIGINAL_COUNTRY <- NULL
  
  # for all lifestyle missing values, it will have default of 0 value
  lifestyle_vars <- grep("LIFESTYLE", names(my.data.clean), value=TRUE)
  my.data.clean[lifestyle_vars][is.na(my.data.clean[lifestyle_vars])] <- 0
  
  # All observations are of same level, so these variables cause NA errors in the model. We will remove them.
  my.data.clean$IsDeceased <- NULL
  my.data.clean$VH12MP <- NULL
  my.data.clean$VH02SP <- NULL
  my.data.clean$DEMO_NIELSEN_NREG <- NULL
  my.data.clean$DEMO_RELIGION_CODE <- NULL
  my.data.clean$BHAV_ALZHEIMERS <- NULL
  my.data.clean$LIFESTYLE_PROCESS <- NULL
  my.data.clean$LIFESTYLE_STOCKS <- NULL
  my.data.clean$LIFESTYLE_VCR <- NULL
  my.data.clean$Religion <- NULL
  
  my.data.clean <- na.omit(my.data.clean)
  
  return(my.data.clean)
}

# run function on my data
data1 <- remove_missing(my.data.clean)

# find percentage of missing variables, should be none left
missing.percent.new <- data.frame(apply(data1,2,pMiss))
names(missing.percent.new) <- c("percent_missing")
missing.subset.new <-subset(missing.percent.new,percent_missing>0)

# Remove independents in training data and re-factorize Y variable
data2 <- data1[data1$Party != "I",]

data2$Party <- factor(data2$Party)
data2$Party <- as.factor(data2$Party)

#######################################################################################################################
# LOGISTIC REGRESSION

# Separate cleaned data into training (80%) and testing sets (20%)
test.indices <-sample(nrow(data2), nrow(data2)/5)

test.data <- data2[test.indices,]
train.data <- data2[-test.indices,]

# Run logistic regression on train data. 0 is democrat, 1 is republican.
logistic.fit <- glm(Party ~.,data=train.data,family="binomial")
summary(logistic.fit)

# Note: The coefficient values of NA are because there is only one level of response for that variable. 
# I.e. all obs are in group 0, no one in group 1.

# Get predicted training probabilities. Class 1 is democrat and class 2 is republican
log.train.prob <- predict(logistic.fit,data=train.data,type="response")
log.train.class <- ifelse(log.train.prob > 0.5,2,1)

log.train.xtab <- table(train.data$Party,log.train.class)
prop.table(log.train.xtab)

# Results for train data on logistic regression:
# log.train.class
#         1         2
# D 0.3998062 0.1158226
# R 0.1058881 0.3784832

# 77.7% accuracy rate for training data

# Get predicted test probabilities using training model. Class 1 is democrat and class 2 is republican
log.test.prob <- predict(logistic.fit,newdata=test.data,type="response")
log.test.class <- ifelse(log.test.prob > 0.5,2,1)

log.test.xtab <- table(test.data$Party,log.test.class)
prop.table(log.test.xtab)

# Results for test data on logistic regression:
# log.test.class
#         1         2
# D 0.3423860 0.1396702
# R 0.1687682 0.3491756

# 69.1% accuracy rate for test data. Model may be overfitted due to high number of variables.

#######################################################################################################################
# LOGISTIC REGRESSION DIAGNOSTICS

# Check model fit

# Chi-Square Goodness of Fit Test using Residuals

# H0: The data are consistent with a specified distribution. 
# Ha: The data are not consistent with a specified distribution.

1 - pchisq(deviance(logistic.fit), df.residual(logistic.fit))

# Deviance is 3782.638, degrees of freedom = 3737
# p value = 0.2968553. The p-value is large and we cannot reject H0, and we conclude there is no evidence of lack of fit. 

# ANOVA Chi-sq test (Saturated vs. Reduced Models)
reduced.fit <- glm(Party ~ 1, data=train.data,family="binomial")
anova(reduced.fit,logistic.fit, test="Chisq")

# Analysis of Deviance Table

# Model 1: Party ~ 1 (Intercept only model)
# Model 2: Party ~ IsMostRecentRegistration + IsNewVoter + ConsumerVerified + ... + LIFESTYLE_WILDLIFE + 
#  LIFESTYLE_WOMENFS + LIFESTYLE_YOURSELF (Full model)

# H0: B1 = B2 = ... = Bp = 0
# H1: At least one Bi not equal to 0

# Resid. Df Resid. Dev  Df Deviance  Pr(>Chi)    
# 1      4126     5717.8                           
# 2      3722     3709.4 404   2008.4 < 2.2e-16 ***

# p value < 0.05, so saturated model is clearly better than an intercept only model

# Next, we look at the p-values for each variable. Since only a few of the variables are significant w/ p-value <0.05, we suspect there are
# many redundant parameters. 

library(ResourceSelection)
hoslem.test(logistic.fit$y, fitted(logistic.fit), g = 10)

# Hosmer and Lemeshow goodness of fit (GOF) test
# Asesses whether or not the observed event rates match expected event rates in subgroups of the model population

# data:  logistic.fit$y, fitted(logistic.fit)
# X-squared = 7.784, df = 8, p-value = 0.4549

# The p-value is large and we cannot reject H0, and we conclude there is no evidence of lack of fit. 

# Diagnostic Plots

# 3 assumptions of logistic regression

# 1) Linearity
# 2) Independence of errors
# 3) Assumes data does not have Multicollinearity

# 1) Linearity Check

par(mfrow=c(1,1))
res <- residuals(logistic.fit, type="pearson")
plot(log(predict(logistic.fit)), res,main="Logistic Regression: Residual Plot",xlab="Pearson Residuals",ylab="Log Predicted Values")

# Points should be around horizontal line, with a roughly constant variance.
# Plotted points has no significant curvature, so we can assume linearity is not seriously violated.

# 2) Independence check

# Durbin Watson test accepts either a linear model object or vector of residuals from a linear model. Using vector of Pearson residuals.

library(car)
durbinWatsonTest(res)

# Since we get a DW statistic of 2.005182 which is approximately equal to 2, this indicates there is no serial correlation. Independence of errors is not violated.

# 3 ) Multicollinearity check

# For correlation matrix, we need to change all variables with chars to numerical dummy variables
factors_only <- names(data2[,sapply(data2,is.factor)])
data2[factors_only] <- sapply(data2[factors_only],as.numeric)

data2$IsMostRecentRegistration <- ifelse(data2$IsMostRecentRegistration == "True",1,0)
data2$IsNewVoter <- ifelse(data2$IsNewVoter == "True",1,0)
data2$ConsumerVerified <- ifelse(data2$ConsumerVerified == "True",1,0)
data2$Language <- ifelse(data2$Language == "EN",1,0)
data2$IsDeceased <- ifelse(data2$Language == "True",1,0)

# Correlation matrix
correl_dummy_df <- data.frame(round(cor(data2, use = "pair"), 2))
correl_dummy_df

# VIF
options("scipen"=100, "digits"=5)
vif(logistic.fit)

# High VIF values for certain variables indicate presence of high correlations 
# hence the assumption of non-Multicollinearity is violated, and we need to try methods of variable selection such as regularized logistic regression.

#######################################################################################################################
# LASSO LOGISTIC REGRESSION

library(glmnet)

# Separate cleaned data into training (80%) and testing sets (20%)
test.indices <-sample(nrow(data2), nrow(data2)/5)

test.data <- data2[test.indices,]
train.data <- data2[-test.indices,]

# Create X and Y train and test sets

X.train <- as.matrix(subset(train.data, select = -c(Party)))
X.test <- as.matrix(subset(test.data, select = -c(Party)))

y.train <- factor(train.data$Party)
y.train <- as.factor(y.train)

y.test <- factor(test.data$Party)
y.test <- as.factor(y.test)

# Train LASSO on training data. Suspected multicollinearity in original model since many variables are highly correlated.
lasso.fit <- cv.glmnet(x=X.train,y=y.train,family="binomial")
lasso.coefs <- data.frame(coef.name = dimnames(coef(lasso.fit))[[1]], coef.value = matrix(coef(lasso.fit,s = "lambda.min")))
lasso.coefs.sorted <- lasso.coefs[order(-abs(lasso.coefs$coef.value)),]

# Predict on training data and get error rate
lasso.train.prob <- predict(lasso.fit, X.train, s = "lambda.1se", type = "response")
lasso.train.class <- predict(lasso.fit, X.train, s = "lambda.1se", type = "class")

lasso.train.xtab <- table(y.train,lasso.train.class)
prop.table(lasso.train.xtab)

# Predict on test data and get error rate
lasso.test.prob <- predict(lasso.fit, X.test, s = "lambda.1se", type = "response")
lasso.test.class <- predict(lasso.fit, X.test, s = "lambda.1se", type = "class")

lasso.test.xtab <- table(y.test,lasso.test.class)
prop.table(lasso.test.xtab)

# lower test rate, but slightly higher test rate. 

#lasso.train.class
#y.train       1       2
#1 0.33244 0.17737
#2 0.11025 0.37994

# lasso.test.class
#y.test       1       2
#1 0.31232 0.19302
#2 0.11736 0.37730

# Final list of coefficients

CF <- as.matrix(lasso.coefs)
CF.unique <- CF[CF!=0,]

# LASSO plots. First one is the variable coefficients vs. shrinkage parameter lambda. As lambda increases, many variables shrink to 0.
# Second one is the cross validation plot to select the lambda which minimizes the model's deviance.

op <- par(mfrow=c(1, 2),mar=c(5,5,7,5))
plot(lasso.fit$glmnet.fit, "lambda", label=TRUE,main="L1 Logistic Regression: Coefficient Paths")
plot(lasso.fit,main="L1 Logistic Regression: Selecting Lambda using CV")
