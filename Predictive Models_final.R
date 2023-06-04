library(tidyverse)
library(readxl)
library(tableone)
library(caret)
library(party)  
library(ggplot2)
library(pROC)

rm(list = ls())

data <- read_excel("data_en.xlsx")
varsToFactor <- c("Operation","Operation_name","Gender")
data[varsToFactor] <- lapply(data[varsToFactor], factor)
data$Occluder_size[data$Occluder_size == "-"] <- NA

#Split the raw data into training and testing sets#####
set.seed(123)
trainIndex <- createDataPartition(data$Operation, p = 0.8, list = FALSE, times = 1, groups = 2)
data_train <- data[trainIndex, c(7:12,16)]
data_test <- data[-trainIndex,  c(7:12,16)]
table(data_train$Operation)
table(data_test$Operation)

#Set Capping Edge
data_train[,1:6] <- lapply(data_train[,1:6], function(x) ifelse(x >= 10, 10, x))
data_test[,1:6] <- lapply(data_test[,1:6], function(x) ifelse(x >= 10, 10, x))


# logistic regression model#####
set.seed(123)
# Train logistic regression model
model_logistic <- train(Operation ~ ., data = data_train, method = "glm", family = "binomial")
summary(model_logistic)

#ROC curve#####
# Use the model to predict the probability of the positive class (class 1) on the training set
positive_logistic_train <- predict(model_logistic, newdata = data_train, type = "prob")[, 1]
roc_obj_logistic_train <- roc(response = data_train$Operation, predictor = positive_logistic_train)
# Create a data frame with sensitivity, specificity, and AUC
roc_logistic_train <- data.frame(
  sensitivity = roc_obj_logistic_train$sensitivities,
  specificity = roc_obj_logistic_train$specificities
)

# Plot the ROC curve
ggplot() +
  geom_segment(data = roc_logistic_train, aes(x = 1 - specificity, y = sensitivity, xend = c(tail(1 - specificity, -1), 0), yend = c(tail(sensitivity, -1), 0)), linewidth = 1, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  theme_classic() +
  labs(
    title = "ROC Curve for Logistic Regression Model",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  annotate("text", x = 0.4, y = 0.6, label = paste("AUC Train =", round(roc_obj_logistic_train$auc, 3)), color = "black") 

#prediction accuracy
predictions_logistic_train <- predict(model_logistic, newdata = data_train)
confusionMatrix(predictions_logistic_train, data_train$Operation)
predictions_logistic_test <- predict(model_logistic, newdata = data_test)
confusionMatrix(predictions_logistic_test, data_test$Operation)


# Train cforest model#####
library(party)
model_forest <- cforest(Operation~.,data=data_train)

#ROC curve#####
predforest_train_prob <- predict(model_forest, newdata=data_train, type="prob")
positive_forest_train <- sapply(predforest_train_prob, function(x) x[2])
# Create a ROC object
roc_obj_forest_train <- roc(response = data_train$Operation, predictor = positive_forest_train)
# Create a data frame with sensitivity, specificity, and AUC
roc_forest_train <- data.frame(
  sensitivity = roc_obj_forest_train$sensitivities,
  specificity = roc_obj_forest_train$specificities
)
# Plot the ROC curve
ggplot() +
  geom_segment(data = roc_forest_train, aes(x = 1 - specificity, y = sensitivity, xend = c(tail(1 - specificity, -1), 0), yend = c(tail(sensitivity, -1), 0)), linewidth = 1, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  theme_classic() +
  labs(
    title = "ROC Curve for Logistic Regression Model",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  annotate("text", x = 0.4, y = 0.6, label = paste("AUC Train =", round(roc_obj_forest_train$auc, 3)), color = "black") 

#prediction accuracy
predforest_train <- predict(model_forest,newdata=data_train)
confusionMatrix(predforest_train, data_train$Operation)
predforest_test <- predict(model_forest,newdata=data_test)
confusionMatrix(predforest_test, data_test$Operation)


#multiple linear regression model#####
#Evaluating the Importance of Predicting Sealing on Each Edge Using ROC Curves
data_train <- as.data.frame(data_train)
response <- as.vector(data_train[, 7])


roc_list <- list()
auc_list <- numeric(length = 6)

for (i in 1:6) {
  predictor <- as.numeric(data_train[, i])
  roc_obj <- roc(response, predictor, levels = c(0, 1))
  

  roc_list[[i]] <- roc_obj
  auc_list[i] <- auc(roc_obj)
}


print(auc_list)


plot(roc_list[[1]], col = "blue", main = "ROC Curves")
for (i in 1:6) {
  plot(roc_list[[i]], col = i + 1, add = TRUE)
}
legend("bottomright", legend = paste(colnames(data_train)[1:6], "AUC:", round(auc_list, 3)), col = 2:7, lwd = 2)
legend

#Create a sample data box and reorder the 6 features
auc_sort <- c(6,3,2,4,5,1)
data_frame_train <- data_train[,auc_sort]
data_frame_test <- data_test[,auc_sort]

# Generate all possible combinations models
models <- data.frame(a = integer(),
                     b = integer(),
                     c = integer(),
                     d = integer(),
                     e = integer(),
                     f = integer())

for (a in seq(100, 0, -5)) {
  for (b in seq(a, 0, -5)) {
    for (c in seq(b, 0, -5)) {
      for (d in seq(c, 0, -5)) {
        for (e in seq(d, 0, -5)) {
          for (f in seq(e, 0, -5)) {
            if (a + b + c + d + e + f == 100) {
              result <- data.frame(a = a,
                                   b = b,
                                   c = c,
                                   d = d,
                                   e = e,
                                   f = f)
              models <- rbind(models, result)
            }
          }
        }
      }
    }
  }
}

# defined function generative model
generate_models <- function(data_frame, models) {

  result_data_frame <- NULL

  for (i in 1:nrow(models)) {

    model_coefficients <- models[i, ]
    
    matrix1 <- as.matrix(data_frame)
    matrix2 <- as.matrix(t(model_coefficients))
    
    result_matrix <- matrix1 %*% matrix2/100
    
    model_data_frame <- data.frame(result_matrix[,1])
    colnames(model_data_frame) <- paste("model", i, sep = "")
    
    if (is.null(result_data_frame)) {
      result_data_frame <- model_data_frame
    } else {
      result_data_frame <- cbind(result_data_frame, model_data_frame)
    }
  }
  
  return(result_data_frame)
}

data_model_train <- generate_models(data_frame_train, models)
data_model_train$Operation <- data_train$Operation

auc_results_train <- data.frame(model = character(),
                                auc = numeric())

Operation <- as.factor(data_model_train$Operation)

# Draw ROC curve
plot(NULL, type = "n", xlim = c(1, 0), ylim = c(0, 1), xlab = "1 - Specificity", ylab = "Sensitivity", main = "ROC Curves for Models")

# Calculate the AUC value for each model and add it to the result data box
for (i in 1:282) {
  

  model_name <- paste("model", i, sep = "")
  
  roc_obj <- roc(Operation, data_model_train[[model_name]], plot = TRUE, col = rainbow(282)[i], add = TRUE, legacy.axes = TRUE)
  
  auc_results_train <- rbind(auc_results_train, data.frame(model = model_name, auc = auc(roc_obj)))
}

# Display AUC result data box
head(auc_results_train[order(auc_results_train$auc, decreasing = TRUE), ], 10)
models_top10 <- models[c(249,196,237,215,223,205,167,204,207,158),]
models_top10

#Check the accuracy of model249 in the training set
roc_train_model249 <- roc(Operation, data_model_train$model249)
#Calculate the threshold for model249
best_threshold_model249 <- coords(roc_train_model249, "best", best.method = "youden", ret = "threshold")["threshold"] %>% 
  as.numeric()
best_threshold_model249

#Prediction accuracy of multiple linear regression models
# Convert predicted values to binary categories using optimal thresholds
model249_train_predictions <- ifelse(data_model_train$model249 >= best_threshold_model249, 0, 1)

# Calculate confusion matrix and statistical information of training set
confusionMatrix(factor(model249_train_predictions), factor(data_model_train$Operation))

data_model_test <- generate_models(data_frame_test, models)
data_model_test$Operation <- data_test$Operation
model249_test_predictions <- ifelse(data_model_test$model249 >= best_threshold_model249, 0, 1)
# Calculate confusion matrix and statistics of test set
confusionMatrix(factor(model249_test_predictions), factor(data_model_test$Operation))

#Accuracy of predicting adults and children separately
data_adult <- data[data$Age_year>=18, c(7:12,16)]
data_young <- data[data$Age_year<18, c(7:12,16)]

predictions_logistic_adult <- predict(model_logistic, newdata = data_adult)
confusionMatrix(predictions_logistic_adult, data_adult$Operation)
predictions_logistic_young <- predict(model_logistic, newdata = data_young)
confusionMatrix(predictions_logistic_young, data_young$Operation)

predforest_adult <- predict(model_forest,newdata=data_adult)
confusionMatrix(predforest_adult, data_adult$Operation)
predforest_young <- predict(model_forest,newdata=data_young)
confusionMatrix(predforest_young, data_young$Operation)

auc_sort <- c(6,3,2,4,5,1)
data_frame_adult <- data_adult[,auc_sort]
data_frame_young <- data_young[,auc_sort]

data_model_adult <- generate_models(data_frame_adult, models)
data_model_young <- generate_models(data_frame_young, models)
data_model_adult$Operation <- data_adult$Operation
data_model_young$Operation <- data_young$Operation

model249_adult_predictions <- ifelse(data_model_adult$model249 >= best_threshold_model249, 0, 1)
model249_young_predictions <- ifelse(data_model_young$model249 >= best_threshold_model249, 0, 1)

#Calculate confusion matrix and statistics of test set
confusionMatrix(factor(model249_adult_predictions), factor(data_model_adult$Operation))
confusionMatrix(factor(model249_young_predictions), factor(data_model_young$Operation))