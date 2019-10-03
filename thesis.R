# THESIS: Application of Machine Learning on Predictive Maintenance
# data source: heavy truck operational data from SCANIA AB

# --------------- Missing data and Exploratory analysis ----------

# ---- 19/03/2019 ----------
# reading of the taining set
dataset0 <- read.csv('aps_failure_training_set.csv', na.strings = c("na"))
View(dataset0) # viewing the data

# Handling of categorical class variable
# by changing the 'pos' value to 1 and 'neg' value to 0
dataset0$class = factor(dataset0$class, levels = c('neg', 'pos'), labels = c(0, 1))
View(dataset0)

# plot with legend, Figure 4 plot
plot(factor(dataset0$class), col = c("gray", "black"), xlab = "Class", ylab= "Observation")
legend("topright", title = "Types of class",legend = c("negative", "positive"), col = c("gray", "black"), pch = 15, pt.cex = 2)


str(dataset)
complete = dataset[complete.cases(dataset),] # 591 entries
View(complete)

#------------ to do list ---------
# Do exploratory data analysis before missing value imputation (Not right)
# write about missing data and the type of missing data (mcar, mar, nmar)
# Do exploratory data analysis after imputation
# show imputed value to each feature with missing value
# On writing, write on chapter, more on machine learning and the methods used in this project.

# original dataset before handling of missing data
dataset_original <- dataset0

# checking the number of NAs in each column
View(dataset0[is.na(dataset0$ac_000),])

# missing value percentage, Figure 5 plot
percent <- function(x) {sum(is.na(x))/length(x) * 100}
plot(sapply(dataset0[,-1], percent), pch = 16,
     col = ifelse((sapply(dataset0[,-1], percent) > 0), "gray", "black"),
     xlab = "Feature Number", ylab= "Missing Value Percentage (%)") # copy this plot result
legend("topright", title = "NA Features",legend = c("without NA", "with NA"), 
       col = c("black", "gray"),pch = 16, pt.cex = 2)


# fancy plot
install.packages("VIM")
library(VIM)
dataset.mis <- subset(dataset, select = -c(class))
mice_plot <- aggr(dataset.mis, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(dataset.mis), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))


# using which, just for practice
View(dataset[which(dataset$ac_000 == 2130706432),])

# using Multiple Imputation method for missing values
library(mice)
library(VIM)
#install.packages('randomForest')
library(randomForest)
md.pattern(dataset[, 1:171]) # feature 1 is class label and feature 2 is complete
# impute
format(Sys.time(), "%a %b %d %X %Y")
impute <- mice(dataset[, 1:171], m = 2, maxit = 1, method = "cart", print = F)

#============ carried out second imputation
# some values are still missing after the first imputation
# second imputation is carried out
View(completeData1)
sapply(completeData1, percent)
plot(sapply(completeData1, percent)) # copy the result of this plot

impute2 <- mice(completeData1[, 1:171], m = 2, maxit = 1, method = "cart", print = F)
completeData3 <- complete(impute2,1)

plot(sapply(completeData3, percent)) # copy the result of this plot

# Still the same, now I can perform listwise or pairwise deletion.
completeData3 = completeData3[complete.cases(completeData3),]
View(completeData3) # 44,667 entries
plot(completeData3$class)
#============

completeData1 <- complete(impute,1)
write.csv(completeData1, file = "train.csv")
completeData2 <- complete(impute,2)
format(Sys.time(), "%a %b %d %X %Y")
View(completeData1)
View(completeData2)


# Another package for missing value imputation
# missForest
install.packages('missForest')
library(missForest)
format(Sys.time(), "%a %b %d %X %Y")
impute_mf <- missForest(dataset[, 1:171])
#completeData1 <- complete(impute,1)
format(Sys.time(), "%a %b %d %X %Y")

#Test
#impute_mf$OOBerror



# ======== Analysis with complete data with R (retrieved stored complete data) ==================

dataset <- read.csv("train.csv")
plot(sapply(dataset[,-1], percent), pch = 16,
     col = ifelse((sapply(dataset[,-1], percent) > 0), "gray", "black"),
     xlab = "Feature Number", ylab= "Missing Value Percentage (%)") # copy this plot result
legend("topright", title = "NA Features",legend = c("without NA", "with NA"), 
       col = c("black", "gray"),pch = 16, pt.cex = 2)

dataset = dataset[complete.cases(dataset),]
View(dataset)
plot(sapply(dataset[,-1], percent), pch = 16,
     col = ifelse((sapply(dataset[,-1], percent) > 0), "gray", "black"),
     xlab = "Feature Number", ylab= "Missing Value Percentage (%)") # copy this plot result
legend("topright", title = "NA Features",legend = c("without NA", "with NA"), 
       col = c("black", "gray"),pch = 16, pt.cex = 2)


# ======== 4 Feature selection techniques with R ==================

# 1a. # Create a correlation table for class versus all features ( not needed )
# Correlation that are closer to +1 and -1 are better and stronger to predict
# dependent variable
# Note: this technique assume linear relationship
cor(dataset, dataset$class)

# 1b. using Informattion gain for Feature selection ( significant )
library(FSelector)
weights <- information.gain(class~., dataset)


subset <- cutoff.k(weights, 30) # best 30 features using the weight
f <- as.simple.formula(subset, "class") # formula for the model
print(f)

plot(weights$attr_importance > 0.0001, pch = 16, 
     col = ifelse((weights$attr_importance > 0.0001) == TRUE, "black", "gray"),
      xlab = "Feature Number", ylab= "Feature Importance Weight") # copy this plot result
legend("center", title = "Feature Weight",legend = c("important", "not important"), 
       col = c("black", "gray"),pch = 16, pt.cex = 2)


length(which((weights$attr_importance > 0.0001) == TRUE))
which((weights$attr_importance > 0.0001) == TRUE)
# 94 features are statistically significant base on information gain
# and the result is the same as 
# subset of attributes which are significantly better than other
better <- cutoff.biggest.diff(weights)

# 2. Using Regression to determine feature importance ( not needed )
# features that are significantly important are features with p value =< 0.05

# Fit a logistic regression model
fit_glm = glm(class~., dataset, family = "binomial")
summary(fit_glm)
# with logistic regression, all features are significant except feature
# cd_000 because all its values are the same, that is, it is constant feature.

library(caret)
varImp(fit_glm)

# 3. Variable importance through random forest ( significant )
library(randomForest)
fit_rf = randomForest(factor(class) ~., data = dataset)

# Create an importance based on mean decreasing gini
importance(fit_rf)
varImp(fit_rf) # same as above

# Create a plot of importance scores by random forest
varImpPlot(fit_rf, pch = 16, col = "black",
           xlab = "Level of importance", ylab= "Important Feature")# copy this plot result
varImpPlot(fit_rf, pch = 16, col = "black")

hist(importance(fit_rf)) # copy this plot result

# 4. Variable importance through lasso method ( significant )

#================
install.packages("glmnet")
library(glmnet)

# ============== hint glmnet requires no NA values in the dataset

install.packages("mlbench")
library(mlbench)
install.packages("psych")
library(psych)
#pairs.panels(dataset[c(2,3,4,5)], cex = 2)
# Error in plot.new() : figure margins too large

cv_output <- cv.glmnet(x = as.matrix(dataset[,2:171]), y = dataset[,1],family = "binomial",
                       standardize = TRUE,alpha = 1, type.measure = "class")
# No error

plot(cv_output)
lambda_1se <- cv_output$lambda.1se # 0.0004608954
lambda_min <- cv_output$lambda.min # 5.952693e-05

coef(cv_output, s=lambda_1se)

#fit_glm <- glmnet( x = as.matrix(dataset[,2:171]), y = dataset[,1], family = "binomial", alpha = 1, lambda = cv_output$lambda.1se)


# ======== 5 machine learning models ==============

# Used dataset is train.csv

# ================ 1. Logistic regression ==========
# ---- 1a. with all the features in the data -----
dataset <- read.csv("train.csv") # dataset after imputation, but there are
# still some na values. (dim = 60000   171)
dataset = dataset[complete.cases(dataset),] # dataset without na. (dim = 44667   171)
plot(factor(dataset$class))

# create training and validation data from dataset using caTools
library(caTools)
split <- sample.split(dataset$class, SplitRatio = 0.70)

# get training and test data
trainset <- subset(dataset, split == TRUE)
testset <- subset(dataset, split == FALSE)

View(trainset)
# logistic regression model using all the features in trainset
logreg_all <- glm(class ~ ., data = trainset, family = binomial(link = "logit"))
summary(logreg_all)

# prediction accuracy
predicted <- predict(logreg_all, newdata = testset, type = "response")
y_pred_ac = ifelse(predicted >= 0.5, 1, 0)
cm <- table(testset[, 1], y_pred_ac)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9947015

# check the cut off that will minimise misclassification error
#install.packages("InformationValue")
library(InformationValue)
optCutOff <- optimalCutoff(testset$class, predicted)[1] 
# 0.01, no need for this with the model accuracy

# finding AUCROC
library(ROCR)
y_pred = prediction(predicted, testset$class)
roc <- performance(y_pred, "tpr", "fpr")
plot(roc)
abline(a=0, b=1)

#calculation of area under the curve AUC
auc <- performance(y_pred, "auc")
auc <- unlist(slot(auc, "y.values"))
# auc = 0.7959018

# another way to find AUCROC
#install.packages("precrec")
library(precrec)
precrec_obj <- evalmod(scores = predicted, labels = testset$class)
autoplot(precrec_obj)

# another way to find AUCROC
#install.packages("PRROC")
library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
# auc = 0.7959018

# ---- 1b. with features from information gain -----
dataset <- read.csv("train.csv")
dataset = dataset[complete.cases(dataset),]

library(FSelector)
weights <- information.gain(class~., dataset)

subset <- cutoff.k(weights, 30) # best 30 features using the weight
f <- as.simple.formula(subset, "class") # formula for the model
print(f)

plot(weights$attr_importance > 0.0001)
length(which((weights$attr_importance > 0.0001) == TRUE))
which((weights$attr_importance > 0.0001) == TRUE)
# 94 features are statistically significant base on information gain
# and the result is the same as 
# subset of attributes which are significantly better than other
better <- cutoff.biggest.diff(weights)
f <- as.simple.formula(better, "class") # formula for the model
print(f)

# Building the logistic regression model with 94 selected features
logreg_94 <- glm(f, data = trainset, family = binomial(link = "logit"))
summary(logreg_94)

# prediction accuracy
predicted <- predict(logreg_94, newdata = testset, type = "response")
y_pred_ac = ifelse(predicted >= 0.5, 1, 0)
cm <- table(testset[, 1], y_pred_ac)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9952239

# checking AUCROC
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj) 
PRROC_obj$auc # 0.8010779 

# ---- 1c. with features from random forest -----
# formula for 30 selected features
rff <- class ~ am_0+al_000+ag_002+bj_000+aq_000+ag_001+ag_003+dn_000+cn_000+bu_000+
  ee_005+ck_000+bb_000+bv_000+cj_000+ap_000+cq_000+ay_006+cn_001+bg_000+bh_000+
  ai_000+ah_000+ci_000+dx_000+an_000+bt_000+ee_007+dg_000+bi_000

# Building the logistic regression model with 30 selected features
logreg_rf <- glm(rff, data = trainset, family = binomial(link = "logit"))
summary(logreg_rf)

# prediction accuracy
predicted <- predict(logreg_rf, newdata = testset, type = "response")
y_pred_ac = ifelse(predicted >= 0.5, 1, 0)
cm <- table(testset[, 1], y_pred_ac)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9951493

# checking AUCROC
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj) 
PRROC_obj$auc # 0.9659864 This is superb

# ---- 1d. with features from lasso regression -----
# formula for 68 selected features

lrf <- class ~ aa_000+af_000+ag_003+ag_008+ai_000+ak_000+am_0+aq_000+ar_000+as_000+at_000+
  au_000+av_000+ax_000+ay_000+ay_002+ay_003+ay_007+ay_008+ay_009+az_001+az_003+az_006+
  az_007+ba_001+ba_002+ba_008+bd_000+be_000+bf_000+bh_000+bi_000+bj_000+bq_000+bs_000+
  bt_000+by_000+cb_000+cc_000+cg_000+cj_000+ck_000+cm_000+cn_001+cn_006+cn_009+cp_000+
  cr_000+cy_000+da_000+dd_000+de_000+df_000+dg_000+dk_000+do_000+dr_000+ds_000+du_000+
  dx_000+dy_000+dz_000+eb_000+ec_00+ee_003+ee_005+ee_007+ee_009

# Building the logistic regression model with 64 selected features
logreg_lr <- glm(lrf, data = trainset, family = binomial(link = "logit"))
summary(logreg_lr)

# prediction accuracy
predicted <- predict(logreg_lr, newdata = testset, type = "response")
y_pred_ac = ifelse(predicted >= 0.5, 1, 0)
cm <- table(testset[, 1], y_pred_ac)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9962687

# checking AUCROC
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj) 
PRROC_obj$auc # 0.9443954

# ================ 2. Naive Bayes classifiers ==========
# ---- 1a. with all the features in the data -----

View(trainset)
# naive bayes classifier model using all the features in trainset
#install.packages("e1071")
#Loading the library
library(e1071)
nb_all <- naiveBayes(factor(class) ~ ., data = trainset)
nb_all

# prediction accuracy
predicted <- predict(nb_all,testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.969403

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc

# ---- 1b. with information gain features in the data -----

igf <- factor(class) ~ am_0 + al_000 + bj_000 + ag_002 + aq_000 + dn_000 + ap_000 + 
  cn_000 + bh_000 + bb_000 + bu_000 + bv_000 + cq_000 + ck_000 + 
  cj_000 + ah_000 + bg_000 + ee_005 + an_000 + ci_000 + ag_001 + 
  ao_000 + ag_003 + aa_000 + bt_000 + cn_001 + cc_000 + bx_000 + 
  ay_008 + az_001 + cs_002 + bi_000 + az_002 + cs_004 + ba_008 + 
  az_005 + by_000 + ag_004 + ee_000 + dd_000 + az_000 + cv_000 + 
  cn_004 + cm_000 + ba_009 + ba_000 + dc_000 + ce_000 + ds_000 + 
  ag_005 + ee_001 + ee_002 + cn_002 + ba_004 + de_000 + cl_000 + 
  dt_000 + az_007 + cn_003 + ba_002 + ba_003 + cs_003 + ay_009 + 
  ba_001 + ba_005 + cs_005 + cx_000 + ee_004 + ba_006 + dg_000 + 
  ee_006 + cs_001 + ed_000 + ec_00 + ba_007 + ee_003 + bc_000 + 
  cg_000 + di_000 + cn_008 + cs_000 + cn_007 + df_000 + ay_007 + 
  cf_000 + cn_009 + bd_000 + ar_000 + eb_000 + ag_000 + do_000 + 
  ai_000 + cn_005 + az_004

# Building the naive bayes classifier with 94 selected features
nb_94 <- naiveBayes(igf, data = trainset)
nb_94

# prediction accuracy
predicted <- predict(nb_94,testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9735821

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc

# ---- 1c. with random forest features in the data -----

rff <- factor(class) ~ am_0+al_000+ag_002+bj_000+aq_000+ag_001+ag_003+dn_000+cn_000+bu_000+
  ee_005+ck_000+bb_000+bv_000+cj_000+ap_000+cq_000+ay_006+cn_001+bg_000+bh_000+
  ai_000+ah_000+ci_000+dx_000+an_000+bt_000+ee_007+dg_000+bi_000

# Building the logistic regression model with 30 selected features
nb_rf <- naiveBayes(rff, data = trainset)
nb_rf

# prediction accuracy
predicted <- predict(nb_rf,testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9811194

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc

# ---- 1d. with lasso regression features in the data -----

lrf <- factor(class) ~ aa_000+af_000+ag_003+ag_008+ai_000+ak_000+am_0+aq_000+ar_000+as_000+at_000+
  au_000+av_000+ax_000+ay_000+ay_002+ay_003+ay_007+ay_008+ay_009+az_001+az_003+az_006+
  az_007+ba_001+ba_002+ba_008+bd_000+be_000+bf_000+bh_000+bi_000+bj_000+bq_000+bs_000+
  bt_000+by_000+cb_000+cc_000+cg_000+cj_000+ck_000+cm_000+cn_001+cn_006+cn_009+cp_000+
  cr_000+cy_000+da_000+dd_000+de_000+df_000+dg_000+dk_000+do_000+dr_000+ds_000+du_000+
  dx_000+dy_000+dz_000+eb_000+ec_00+ee_003+ee_005+ee_007+ee_009

# Building the naive bayes classifier with 64 selected features
nb_lr <- naiveBayes(lrf, data = trainset)
nb_lr

# prediction accuracy
predicted <- predict(nb_lr,testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9775373

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc

# ================ 3. KNN classifiers ==========
# ---- 3a. with all the features in the data -----

dim(trainset)
dim(testset)
library(caret)

# setting trainControl()
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# knn model
knn_all <- train(factor(class) ~ ., data =  trainset, method = "knn",
                 trControl = trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

knn_all
plot(knn_all)

# prediction accuracy
predicted <- predict(knn_all, testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc

# ---- 3b. with the I.G. features in the data -----

# knn model, igf= information gain forest, data = trainset
knn_ig <- train(igf, data =  trainset, method = "knn",
                 trControl = trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)


knn_ig
plot(knn_ig)

# prediction accuracy
predicted <- predict(knn_ig, testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9957463

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc

# ---- 3c. with the R.F. features in the data -----

# knn model, rrf= random forest features, data = trainset
knn_rf <- train(rff, data =  trainset, method = "knn",
                trControl = trctrl,
                preProcess = c("center", "scale"),
                tuneLength = 10)


knn_rf
plot(knn_rf)

# prediction accuracy
predicted <- predict(knn_rf, testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9967164

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc # 0.8165685

# ---- 3d. with the L.R. features in the data -----

# knn model, lrf= lasso regression features, data = trainset
knn_lr <- train(lrf, data =  trainset, method = "knn",
                trControl = trctrl,
                preProcess = c("center", "scale"),
                tuneLength = 10)

# tuneGrid = data.frame(k = seq(1,49,2))

knn_lr
plot(knn_lr)

# prediction accuracy
predicted <- predict(knn_lr, testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9963433

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj) 
PRROC_obj$auc # 0.8016418

# ================ 4. SVM ==========
# with the assumption that the dataset is fully or partially 
# separable
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))


# ---- 4a. with all the features in the data
# with single value of C
svm_all <- train(factor(class) ~., data = trainset, method = "svmLinear",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

# with different values of C
svm_grid_all <- train(factor(class) ~., data = trainset, method = "svmLinear",
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneGrid = grid,
                      tuneLength = 10)

plot(svm_grid_all)

# prediction accuracy
predicted <- predict(svm_grid_all, testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9962687

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc


# ---- 4b. with the I.G. features in the data -----
# with single value of C
svm_igf <- train(igf, data = trainset, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

# with different values of C
svm_grid_igf <- train(igf, data = trainset, method = "svmLinear",
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid,
                           tuneLength = 10)

plot(svm_grid_igf)

# prediction accuracy
predicted <- predict(svm_grid_igf, testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9959701

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc


# ---- 4c. with the R.F. features in the data -----
# with single value of C
svm_rff <- train(rff, data = trainset, method = "svmLinear",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

# with different values of C
svm_grid_rff <- train(rff, data = trainset, method = "svmLinear",
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneGrid = grid,
                      tuneLength = 10)

plot(svm_grid_rff)

# prediction accuracy
predicted <- predict(svm_grid_rff, testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9959701

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc


# ---- 4d. with the L.R. features in the data -----
# with single value of C
svm_lrf <- train(lrf, data = trainset, method = "svmLinear",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

# with different values of C
svm_grid_lrf <- train(lrf, data = trainset, method = "svmLinear",
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneGrid = grid,
                      tuneLength = 10)

plot(svm_grid_lrf)

# prediction accuracy
predicted <- predict(svm_grid_lrf, testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9965672

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc

# ================ 5. Bagging method of Ensemble learning ==========
library(ipred)

# ---- 5a. with all the features in the data
bagging_all <- bagging(factor(class) ~., data = trainset,
                  nbagg = 25)

# prediction accuracy
predicted <- predict(bagging_all, testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9969403|0.9970896

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc

#---Testing
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
trainModel<-train(factor(class) ~., data = trainset, method="treebag",trControl=trctrl)
# prediction accuracy
predicted <- predict(trainModel, testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9969403|0.9970896|0.9970896
# same result but this one is slow, hence no need because same result
#---Testing


# ---- 5b. with the information gain featuree
bagging_igf <- bagging(igf, data = trainset,
                       nbagg = 25)

#plot(bagging_all)

# prediction accuracy
predicted <- predict(bagging_igf, testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9973134

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc

# ---- 5c. with the random forest features
bagging_rff <- bagging(rff, data = trainset,
                       nbagg = 25)

# plot(bagging_all)

# prediction accuracy
predicted <- predict(bagging_rff, testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9969403

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc

# ---- 5d. with the lasso regression features
bagging_lrf <- bagging(lrf, data = trainset,
                       nbagg = 25)

# plot(bagging_all)

# prediction accuracy
predicted <- predict(bagging_lrf, testset)
cm <- table(testset[, 1], predicted)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9969403

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc



# ================ 6. Gradient Boosting method of Ensemble learning ==========
#install.packages('gbm')
library(gbm)
# ---- 6a. with all the features in the data
# 1. try the gbm method
boosting_all <- gbm(class ~igf, data = trainset,
                    distribution = "bernoulli",
                    n.trees = 200,
                    interaction.depth = 4,
                    shrinkage = 0.01,
                    cv.folds = 10)
print(boosting_all)

ntree_opt_cv <- gbm.perf(boosting_all, method = "cv")
print(ntree_opt_cv)

# prediction accuracy
predicted <- predict(boosting_all, newdata = testset,n.trees = ntree_opt_cv,  type = "response")
y_pred_ac = ifelse(predicted >= 0.5, 1, 0)
cm <- table(testset[, 1], y_pred_ac)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9957463

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc
# 0.9712552 best auc so far

# 2. try train control method and compare the result with the 1.
# trctrl <- trainControl(method = "repeatedcv", number = 10)
# boosting_all_tc <- train(factor(class) ~., data = trainset, method="gbm",
#                          trControl=trctrl)
# 
# predicted2 <- predict(boosting_all_tc, testset)
# cm2 <- table(testset[, 1], predicted2)
# accuracy2 <- sum(diag(cm2))/sum(cm2)
# # accuracy = 0.9962687
# 
# PRROC_obj <- roc.curve(scores.class0 = predicted2, weights.class0=testset$class,
#                        curve=TRUE)
# plot(PRROC_obj)
# PRROC_obj$auc
# 0.8310816, good accuracy but not good auc, Hence gbm method is better.


# ---- 6b. with information gain features in the data

igf <- class ~ am_0 + al_000 + bj_000 + ag_002 + aq_000 + dn_000 + ap_000 + 
  cn_000 + bh_000 + bb_000 + bu_000 + bv_000 + cq_000 + ck_000 + 
  cj_000 + ah_000 + bg_000 + ee_005 + an_000 + ci_000 + ag_001 + 
  ao_000 + ag_003 + aa_000 + bt_000 + cn_001 + cc_000 + bx_000 + 
  ay_008 + az_001 + cs_002 + bi_000 + az_002 + cs_004 + ba_008 + 
  az_005 + by_000 + ag_004 + ee_000 + dd_000 + az_000 + cv_000 + 
  cn_004 + cm_000 + ba_009 + ba_000 + dc_000 + ce_000 + ds_000 + 
  ag_005 + ee_001 + ee_002 + cn_002 + ba_004 + de_000 + cl_000 + 
  dt_000 + az_007 + cn_003 + ba_002 + ba_003 + cs_003 + ay_009 + 
  ba_001 + ba_005 + cs_005 + cx_000 + ee_004 + ba_006 + dg_000 + 
  ee_006 + cs_001 + ed_000 + ec_00 + ba_007 + ee_003 + bc_000 + 
  cg_000 + di_000 + cn_008 + cs_000 + cn_007 + df_000 + ay_007 + 
  cf_000 + cn_009 + bd_000 + ar_000 + eb_000 + ag_000 + do_000 + 
  ai_000 + cn_005 + az_004

boosting_igf <- gbm(igf, data = trainset,
                    distribution = "bernoulli",
                    n.trees = 200,
                    interaction.depth = 4,
                    shrinkage = 0.01,
                    cv.folds = 10)
print(boosting_igf)

ntree_opt_cv <- gbm.perf(boosting_igf, method = "cv")
print(ntree_opt_cv)

# prediction accuracy
predicted <- predict(boosting_igf, newdata = testset,n.trees = ntree_opt_cv,  type = "response")
y_pred_ac = ifelse(predicted >= 0.5, 1, 0)
cm <- table(testset[, 1], y_pred_ac)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9956716

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc

# ---- 6c. with random forest features in the data
rff <- class ~ am_0+al_000+ag_002+bj_000+aq_000+ag_001+ag_003+dn_000+cn_000+bu_000+
  ee_005+ck_000+bb_000+bv_000+cj_000+ap_000+cq_000+ay_006+cn_001+bg_000+bh_000+
  ai_000+ah_000+ci_000+dx_000+an_000+bt_000+ee_007+dg_000+bi_000

boosting_rff <- gbm(rff, data = trainset,
                    distribution = "bernoulli",
                    n.trees = 200,
                    interaction.depth = 4,
                    shrinkage = 0.01,
                    cv.folds = 10)
print(boosting_rff)

ntree_opt_cv <- gbm.perf(boosting_rff, method = "cv")
print(ntree_opt_cv)

# prediction accuracy
predicted <- predict(boosting_rff, newdata = testset,n.trees = ntree_opt_cv,  type = "response")
y_pred_ac = ifelse(predicted >= 0.5, 1, 0)
cm <- table(testset[, 1], y_pred_ac)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9957463

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc

# ---- 6d. with random forest features in the data
lrf <- class ~ aa_000+af_000+ag_003+ag_008+ai_000+ak_000+am_0+aq_000+ar_000+as_000+at_000+
  au_000+av_000+ax_000+ay_000+ay_002+ay_003+ay_007+ay_008+ay_009+az_001+az_003+az_006+
  az_007+ba_001+ba_002+ba_008+bd_000+be_000+bf_000+bh_000+bi_000+bj_000+bq_000+bs_000+
  bt_000+by_000+cb_000+cc_000+cg_000+cj_000+ck_000+cm_000+cn_001+cn_006+cn_009+cp_000+
  cr_000+cy_000+da_000+dd_000+de_000+df_000+dg_000+dk_000+do_000+dr_000+ds_000+du_000+
  dx_000+dy_000+dz_000+eb_000+ec_00+ee_003+ee_005+ee_007+ee_009

boosting_lrf <- gbm(lrf, data = trainset,
                    distribution = "bernoulli",
                    n.trees = 200,
                    interaction.depth = 4,
                    shrinkage = 0.01,
                    cv.folds = 10)
print(boosting_lrf)

ntree_opt_cv <- gbm.perf(boosting_lrf, method = "cv")
print(ntree_opt_cv)

# prediction accuracy
predicted <- predict(boosting_lrf, newdata = testset,n.trees = ntree_opt_cv,  type = "response")
y_pred_ac = ifelse(predicted >= 0.5, 1, 0)
cm <- table(testset[, 1], y_pred_ac)
accuracy <- sum(diag(cm))/sum(cm)
# accuracy = 0.9954478

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = predicted, weights.class0=testset$class,
                       curve=TRUE)
plot(PRROC_obj)
PRROC_obj$auc

# ================ 7. Stacking method of Ensemble learning ==========

# ---- 7a. with all the features in the data
# base leaners are: gbm, bagging, SVM, and RF. I have gbm, bagging and SVM models
# already, just remain only RF model that I need to build


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model_rf_all <- train(factor(class) ~., data = trainset, method="rf",
                         trControl=trctrl)

predicted_rf <- predict(model_rf_all, testset)
cm_rf <- table(testset[, 1], predicted_rf)
accuracy_rf <- sum(diag(cm_rf))/sum(cm_rf)
# accuracy = 0.9962687


# keep it simple
# check the correlation of the base models
models_cor <- resamples(list(mod1 = bagging_all, mod2=svm_grid_all, mod3 = boosting_all))

methods <- c("treebag", "gbm", "svmLinear")



