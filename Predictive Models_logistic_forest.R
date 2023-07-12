library(tidyverse)
library(readxl)
library(tableone)
library(caret)
library(party)  
library(ggplot2)
library(pROC)
library(patchwork)

rm(list = ls())
#导入数据，整理数据#####
data <- read_excel("data_en.xlsx")
varsToFactor <- c("Operation","Operation_name","Gender")
data[varsToFactor] <- lapply(data[varsToFactor], factor)
data$Occluder_size[data$Occluder_size == "-"] <- NA

# 使用apply()和shapiro.test()函数检查正态性#####
normality_tests <- apply(data[,c(4:12,14,18)], 2, shapiro.test)
p_values <- sapply(normality_tests, function(x) x$p.value)
# 根据p值判断正态性
normality_result <- p_values > 0.05
# 显示正态性结果
print(normality_result)

# 用秩和检验计算p值#####
vars_nonnorm <- names(data[c(4:12,14,18)])
# 使用lapply函数对所有非正态变量进行Wilcoxon检验 （可以省略）
wilcox_results <- lapply(vars_nonnorm, function(vars_nonnorm) {
  if(!shapiro.test(data[[vars_nonnorm]])$p.value > 0.05) {
    wilcox.test(data[[vars_nonnorm]] ~ Operation_name, data = data)
  }
})
print(wilcox_results)

#生成基线三线表#####
vars_summary <- colnames(data[,c(4:14,18)])
table_summary <- print(CreateTableOne(vars = vars_summary, strata = c("Operation_name"), 
                                      data = data, addOverall = TRUE), nonnormal = vars_nonnorm) 
kableone(table_summary)  #P值为秩和检验的P值

#生成封堵组summary，获取Occluder_size中位数#####
data_device <- data[data$Operation == 0, ] %>%
  mutate(Occluder_size = as.numeric(Occluder_size))
summary(data_device$Occluder_size)

#将原始数据分割成训练集和测试集#####
# Load required libraries
library(pROC)
library(caret)

set.seed(123)
trainIndex <- createDataPartition(data$Operation, p = 0.8, list = FALSE, times = 1, groups = 2)
data_train <- data[trainIndex, c(7:12,16)]
data_test <- data[-trainIndex,  c(7:12,16)]

# logistic regression model#####
set.seed(123)
# Train logistic regression model
model_logistic <- train(Operation ~ ., data = data_train, method = "glm", family = "binomial")

#ROC curve for Training Data#####
# Use the model to predict the probability of the positive class (class 1) on the training set
positive_logistic_train <- predict(model_logistic, newdata = data_train, type = "prob")[, 1]
roc_obj_logistic_train <- roc(response = data_train$Operation, predictor = positive_logistic_train)

roc_logistic_train <- data.frame(
  sensitivity = roc_obj_logistic_train$sensitivities,
  specificity = roc_obj_logistic_train$specificities
)

#ROC curve for Test Data#####
# Use the model to predict the probability of the positive class (class 1) on the test set
positive_logistic_test <- predict(model_logistic, newdata = data_test, type = "prob")[, 1]
roc_obj_logistic_test <- roc(response = data_test$Operation, predictor = positive_logistic_test)

roc_logistic_test <- data.frame(
  sensitivity = roc_obj_logistic_test$sensitivities,
  specificity = roc_obj_logistic_test$specificities
)

# Plot the ROC curve
plot_logistic <- ggplot() +
  geom_segment(data = roc_logistic_train, aes(x = 1 - specificity, y = sensitivity, xend = c(tail(1 - specificity, -1), 0), yend = c(tail(sensitivity, -1), 0)), color = "red") +
  geom_segment(data = roc_logistic_test, aes(x = 1 - specificity, y = sensitivity, xend = c(tail(1 - specificity, -1), 0), yend = c(tail(sensitivity, -1), 0)), color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  theme_classic() +
  labs(
    title = "ROC Curve for Logistic Regression Model",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  annotate("text", x = 0.4, y = 0.6, label = paste("AUC Train =", round(auc(roc_obj_logistic_train), 3)), color = "red") +
  annotate("text", x = 0.4, y = 0.5, label = paste("AUC Test =", round(auc(roc_obj_logistic_test), 3)), color = "blue") 


#预测准确率
predictions_logistic_train <- predict(model_logistic, newdata = data_train)
confusionMatrix(predictions_logistic_train, data_train$Operation)
predictions_logistic_test <- predict(model_logistic, newdata = data_test)
confusionMatrix(predictions_logistic_test, data_test$Operation)



# Train cforest model#####
library(party)
model_forest <- cforest(Operation~.,data=data_train)

#ROC curve for Training Data#####
predforest_train_prob <- predict(model_forest, newdata=data_train, type="prob")
positive_forest_train <- sapply(predforest_train_prob, function(x) x[2])
# Create a ROC object
roc_obj_forest_train <- roc(response = data_train$Operation, predictor = positive_forest_train)

roc_forest_train <- data.frame(
  sensitivity = roc_obj_forest_train$sensitivities,
  specificity = roc_obj_forest_train$specificities
)

#ROC curve for Test Data#####
predforest_test_prob <- predict(model_forest, newdata=data_test, type="prob")
positive_forest_test <- sapply(predforest_test_prob, function(x) x[2])
# Create a ROC object
roc_obj_forest_test <- roc(response = data_test$Operation, predictor = positive_forest_test)

roc_forest_test <- data.frame(
  sensitivity = roc_obj_forest_test$sensitivities,
  specificity = roc_obj_forest_test$specificities
)

# Plot the ROC curve
plot_forest <- ggplot() +
  geom_segment(data = roc_forest_train, aes(x = 1 - specificity, y = sensitivity, xend = c(tail(1 - specificity, -1), 0), yend = c(tail(sensitivity, -1), 0)), color = "red") +
  geom_segment(data = roc_forest_test, aes(x = 1 - specificity, y = sensitivity, xend = c(tail(1 - specificity, -1), 0), yend = c(tail(sensitivity, -1), 0)), color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  theme_classic() +
  labs(
    title = "ROC Curve for Random Forest Model",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  annotate("text", x = 0.4, y = 0.6, label = paste("AUC Train =", round(auc(roc_obj_forest_train), 3)), color = "red") +
  annotate("text", x = 0.4, y = 0.5, label = paste("AUC Test =", round(auc(roc_obj_forest_test), 3)), color = "blue") 

#随机森林预测准确性
predforest_train <- predict(model_forest,newdata=data_train)
confusionMatrix(predforest_train, data_train$Operation)
predforest_test <- predict(model_forest,newdata=data_test)
confusionMatrix(predforest_test, data_test$Operation)

plot_logistic/plot_forest





#分别预测成人和儿童的准确度
data_adult <- data[data$Age_year>=18, c(7:12,16)]
data_minor <- data[data$Age_year<18, c(7:12,16)]

#ROC curve for adulting Data#####
# Use the model to predict the probability of the positive class (class 1) on the adulting set
positive_logistic_adult <- predict(model_logistic, newdata = data_adult, type = "prob")[, 1]
roc_obj_logistic_adult <- roc(response = data_adult$Operation, predictor = positive_logistic_adult)

roc_logistic_adult <- data.frame(
  sensitivity = roc_obj_logistic_adult$sensitivities,
  specificity = roc_obj_logistic_adult$specificities
)

#ROC curve for minor Data#####
# Use the model to predict the probability of the positive class (class 1) on the minor set
positive_logistic_minor <- predict(model_logistic, newdata = data_minor, type = "prob")[, 1]
roc_obj_logistic_minor <- roc(response = data_minor$Operation, predictor = positive_logistic_minor)

roc_logistic_minor <- data.frame(
  sensitivity = roc_obj_logistic_minor$sensitivities,
  specificity = roc_obj_logistic_minor$specificities
)

# Plot the ROC curve
plot_logistic_adult_minor <- ggplot() +
  geom_segment(data = roc_logistic_adult, aes(x = 1 - specificity, y = sensitivity, xend = c(tail(1 - specificity, -1), 0), yend = c(tail(sensitivity, -1), 0)), color = "red") +
  geom_segment(data = roc_logistic_minor, aes(x = 1 - specificity, y = sensitivity, xend = c(tail(1 - specificity, -1), 0), yend = c(tail(sensitivity, -1), 0)), color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  theme_classic() +
  labs(
    title = "ROC Curve for Logistic Regression Model",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  annotate("text", x = 0.4, y = 0.6, label = paste("AUC adult =", round(auc(roc_obj_logistic_adult), 3)), color = "red") +
  annotate("text", x = 0.4, y = 0.5, label = paste("AUC minor =", round(auc(roc_obj_logistic_minor), 3)), color = "blue") 

save(plot_logistic_adult_minor,file = "plot_logistic_adult_minor.Robj")

predictions_logistic_adult <- predict(model_logistic, newdata = data_adult)
confusionMatrix(predictions_logistic_adult, data_adult$Operation)
predictions_logistic_minor <- predict(model_logistic, newdata = data_minor)
confusionMatrix(predictions_logistic_minor, data_minor$Operation)



#ROC curve for adulting Data#####
predforest_adult_prob <- predict(model_forest, newdata=data_adult, type="prob")
positive_forest_adult <- sapply(predforest_adult_prob, function(x) x[2])
# Create a ROC object
roc_obj_forest_adult <- roc(response = data_adult$Operation, predictor = positive_forest_adult)

roc_forest_adult <- data.frame(
  sensitivity = roc_obj_forest_adult$sensitivities,
  specificity = roc_obj_forest_adult$specificities
)

#ROC curve for minor Data#####
predforest_minor_prob <- predict(model_forest, newdata=data_minor, type="prob")
positive_forest_minor <- sapply(predforest_minor_prob, function(x) x[2])
# Create a ROC object
roc_obj_forest_minor <- roc(response = data_minor$Operation, predictor = positive_forest_minor)

roc_forest_minor <- data.frame(
  sensitivity = roc_obj_forest_minor$sensitivities,
  specificity = roc_obj_forest_minor$specificities
)

# Plot the ROC curve
plot_forest_adult_minor <- ggplot() +
  geom_segment(data = roc_forest_adult, aes(x = 1 - specificity, y = sensitivity, xend = c(tail(1 - specificity, -1), 0), yend = c(tail(sensitivity, -1), 0)), color = "red") +
  geom_segment(data = roc_forest_minor, aes(x = 1 - specificity, y = sensitivity, xend = c(tail(1 - specificity, -1), 0), yend = c(tail(sensitivity, -1), 0)), color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  theme_classic() +
  labs(
    title = "ROC Curve for Random Forest Model",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  annotate("text", x = 0.4, y = 0.6, label = paste("AUC adult =", round(auc(roc_obj_forest_adult), 3)), color = "red") +
  annotate("text", x = 0.4, y = 0.5, label = paste("AUC minor =", round(auc(roc_obj_forest_minor), 3)), color = "blue") 

save(plot_forest_adult_minor,file = "plot_forest_adult_minor.Robj")

predforest_adult <- predict(model_forest,newdata=data_adult)
confusionMatrix(predforest_adult, data_adult$Operation)
predforest_minor <- predict(model_forest,newdata=data_minor)
confusionMatrix(predforest_minor, data_minor$Operation)


