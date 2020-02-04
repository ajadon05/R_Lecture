library(tidyverse)
library(ISLR)

ml_data <- College
ml_data %>%
  glimpse()

library(caret)

# Partition into training and test data
set.seed(42)
index <- createDataPartition(ml_data$Private, p = 0.7, list = FALSE)
train_data <- ml_data[index, ]
test_data  <- ml_data[-index, ]

# Train model with preprocessing & repeated cv
model_gbm <- caret::train(Private ~ .,
                          data = train_data,
                          method = "gbm",
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 5, 
                                                   repeats = 3, 
                                                   verboseIter = FALSE),
                          verbose = 0)
model_gbm

caret::confusionMatrix(
  data = predict(model_gbm, test_data),
  reference = test_data$Private
)


#The xgboost library
#We can also directly work with the xgboost package in R. 
library(xgboost)

xgboost_model <- xgboost(data = as.matrix(train_data[, -1]), 
                         label = as.numeric(train_data$Private)-1,
                         max_depth = 3, 
                         objective = "binary:logistic", 
                         nrounds = 10, 
                         verbose = FALSE,
                         prediction = TRUE)
xgboost_model

# We can again use predict(); because here, we will get prediction probabilities, 
# we need to convert them into labels to compare them with the true class:
predict(xgboost_model, 
        as.matrix(test_data[, -1])) %>%
  as.tibble() %>%
  mutate(prediction = round(value),
         label = as.numeric(test_data$Private)-1) %>%
  count(prediction, label)



