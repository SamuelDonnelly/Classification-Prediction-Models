
-----------------------
  title: "Assignment 1"
author: "Samuel Donnelly"
date: "1/21/2021"
-----------------------

  
  
 
library(MASS)
library(tidyverse)
library(class)
library(kableExtra)




## (2) Create "value" variable & add to df

data <- Boston 
data <- mutate(data, value = if_else(data$medv > median(data$medv),"High",
                                     "Low"))

data$value <- as.factor(data$value)


## (1) Create test and train data set (50/50 split)

set.seed(2021)
dt <- sort(sample(nrow(data), nrow(data)*.5))#random sample w/out replacement 
train <- data[dt, ]
test <- data[-dt, ]


## (3) Train & Test Log Model 


  
  ### (3b) Fit logistic model predicting value
  
log_train <- glm(value ~ rm + ptratio + indus + age, family = "binomial",
                 data = train)
summary(log_train)


### (3c) Test logistic model

log_test <- predict(log_train, test, type = "response")
#plot(log_test, main = "Plot of distribution of predicted probability values")


## (4) Log confusion matrix

log_pred <- rep("High", length(log_test))
log_pred[log_test > .5] <- "Low"

log_results <- table(test$value, log_pred)
log_results


## (5) Log Metrics
### (5a) Log overall accuracy

log_accuracy <- mean(log_pred == test$value)
log_accuracy


### (5b) Log true positive rate

log_tp <- log_results[1,1]/(log_results[1,1] + log_results[2,1])
log_tp


### (5c) Log false positive rate

log_fp <- log_results[1,2]/(log_results[1,2] + log_results[2,2])
log_fp


### (5d) Log true negative rate

log_tn <- 1-log_fp
log_tn


### (5e) Log false negative rate

log_fn <- 1-log_tp
log_fn


## (6a) KNN
### Train & Test KNN

##Create train and test df and train value vector using the train and test dfs 
##(see lines 30:32) created by randomly sampling without replacement to split 
##original df 50/50.
knn_train_df <- train %>% select(indus, rm, age, ptratio)
knn_test_df <- test %>% select(indus, rm, age, ptratio)
knn_value <- train$value

## train KNN model
knn_train <- knn(knn_train_df, knn_test_df, knn_value, k = 1)


### KNN confusion matrix

knn_results <- table(test$value, knn_train)
knn_results


### KNN metrics

## knn overall accuracy
knn_accuracy <- mean(knn_train == test$value)

## knn true positive rate
knn_tp <- knn_results[1,1]/(knn_results[1,1] + knn_results[2,1])

## knn false positive rate
knn_fp <- knn_results[1,2]/(knn_results[1,2] + knn_results[2,2])

## knn true negative rate
knn_tn <- 1-knn_fp

## knn false negative rate
knn_fn <- 1-knn_tp

tibble(Metric = c("Accuracy", "True Positive", "False Positive", 
                  "True Negative", "False Negative"), Percentage = 
         c(knn_accuracy, knn_tp, knn_fp, knn_tn, knn_fn)) %>% 
  kable(caption = "KNN Metrics", digits = 3) %>%
  kable_classic(html_font = "Cambria", full_width = F)


## (6b) LDA
### Train & Test LDA

## Using same train and test data sets (from lines 30:32)

## Train model
lda_train <- lda(value ~ rm + indus + age + ptratio, data = train)
lda_train

## Test LDA model
lda_test <- predict(lda_train, test)


### LDA confusion matrix

lda_results <- table(test$value, lda_test$class)
lda_results


### LDA metrics

## qda overall accuracy 
lda_accuracy <- mean(lda_test$class == test$value)

## lda true positive rate
lda_tp <- lda_results[1,1]/(lda_results[1,1] + lda_results[2,1])

## lda false positive rate
lda_fp <- lda_results[1,2]/(lda_results[1,2] + lda_results[2,2])

## lda true negative rate
lda_tn <- 1-lda_fp

## lda false negative rate
lda_fn <- 1-lda_tp

tibble(Metric = c("Accuracy", "True Positive", "False Positive",
                  "True Negative", "False Negative"), Percentage = 
         c(lda_accuracy, lda_tp, lda_fp, lda_tn, lda_fn)) %>% 
  kable(caption = "LDA Metrics", digits = 3) %>%
  kable_classic(html_font = "Cambria", full_width = F)


## (6c) QDA
### Train & Test QDA

## Using same train and test data sets (from lines 30:32)

## Train QDA model
qda_train <- qda(value ~ rm + indus + age + ptratio, data = train)
qda_train

## Test QDA model
qda_test <- predict(qda_train, test)


### QDA confusion matrix

qda_results <- table(test$value, qda_test$class)
qda_results


### QDA metrics

## Overall qda accuracy 
qda_accuracy <- mean(qda_test$class == test$value)

## qda true positive rate
qda_tp <- qda_results[1,1]/(qda_results[1,1] + qda_results[2,1])

## qda false positive rate
qda_fp <- qda_results[1,2]/(qda_results[1,2] + qda_results[2,2])

## qda true negative rate
qda_tn <- 1-qda_fp

## qda false negative rate
qda_fn <- 1-qda_tp

tibble(Metric = c("Accuracy", "True Positive", "False Positive", 
                  "True Negative", "False Negative"), Percentage = 
         c(qda_accuracy, qda_tp, qda_fp, qda_tn, qda_fn)) %>% 
  kable(caption = "QDA Metrics", digits = 3) %>%
  kable_classic(html_font = "Cambria", full_width = F)


## (7) Summary of results

results <- tibble(Method = c("Logistic", "KNN", "LDA", "QDA"), Total.Accuracy
                  = c(log_accuracy, knn_accuracy, lda_accuracy, qda_accuracy),
                  True.Positive.Rate = c(log_tp, knn_tp, lda_tp, qda_tp), 
                  False.Positive.Rate = c(log_fp, knn_fp, lda_fp, qda_fp), 
                  True.Negative.Rate = c(log_tn, knn_tn, lda_tn, qda_tn), 
                  False.Negative.Rate = c(log_fn, knn_fn, lda_fn, qda_fn))
results %>%
  kable(caption = "Results Table", digits = 3) %>%
  kable_classic(full_width = T, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = "hover")


## (8) Relative performance of modeling
#**Overall accuracy had a range of ~16% with performance being the highest in
#the logistic (87%) and lowest in KNN (70%), with LDA and QDA falling in the 
#middle (both around 84%). This trend generally continued across true positive, 
#false, positive, true negative, and false negative rates. More specifically,
#KNN is discernibly worse than the next best model (QDA) across all metrics, 
#while the top three have negligible differences, with the best model (logistic)
#being only slightly better than the subsequent two models (LDA & QDA).**
  
## (9) Model of choice and rational
#**Overall, I would feel most comfortable optimizing to the logistic model to
#minimize loss for it has the smallest average false ratings (average across 
#false positive and false negative). In other words, the logistic model would
#make the least amount of type 1 and type 2 errors, which prioritizes reducing
#inaccuracy (contrasted with maximizing accuracy). I averaged over both false
#positive and false negative because real estate investors may be using the 
#model to buy real estate (right now) that is anticipated to go up, as well as 
#decide to sell currently owned real estate (e.g., sell now because model 
#predicts value to go down).**  
  