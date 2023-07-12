library(tidyverse)
library(readxl)
library(tableone)
library(caret)
library(party)  
library(ggplot2)
library(pROC)

# rm(list = ls())
#导入数据，整理数据#####
data <- read_excel("data_en.xlsx")
varsToFactor <- c("Operation","Operation_name","Gender")
data[varsToFactor] <- lapply(data[varsToFactor], factor)
data$Occluder_size[data$Occluder_size == "-"] <- NA

#将原始数据分割成训练集和测试集#####
set.seed(123)
trainIndex <- createDataPartition(data$Operation, p = 0.8, list = FALSE, times = 1, groups = 2)
data_train_capping <- data[trainIndex, c(7:12,16)]
data_test_capping <- data[-trainIndex,  c(7:12,16)]

#多元线性回归模型#####
#利用ROC曲线评估各个边预测封堵的重要性
# 提取分类变量，保留为数值向量
data_train_capping <- as.data.frame(data_train_capping)
response <- as.vector(data_train_capping[, 7])

# 初始化一个列表来存储每个变量的ROC曲线和AUC
roc_list <- list()
auc_list <- numeric(length = 6)

# 遍历每个连续变量（1-6列）
for (i in 1:6) {
  predictor <- as.numeric(data_train_capping[, i])
  roc_obj <- roc(response, predictor, levels = c(0, 1))
  
  # 存储ROC对象和AUC
  roc_list[[i]] <- roc_obj
  auc_list[i] <- auc(roc_obj)
}

# 打印AUC值
print(auc_list)

# 可视化ROC曲线
# plot(roc_list[[1]], col = "blue", main = "ROC Curves")
plot(NULL, type = "n", xlim = c(1, 0), ylim = c(0, 1), xlab = "1 - Specificity", ylab = "Sensitivity", main = "ROC Curves for original data")
for (i in 1:6) {
  plot(roc_list[[i]], col = i + 1, add = TRUE)
}
legend("bottomright", legend = paste(colnames(data_train_capping)[1:6], "AUC:", round(auc_list, 3)), col = 2:7, lwd = 2)
legend
#捕获图形
captured_plot_origin_ROC <- recordPlot()
save(captured_plot_origin_ROC,file = "captured_plot_origin_ROC.Robj")
# 创建示例数据框,对6个特征重新排序
auc_sort <- c(6,3,2,4,5,1)
data_frame_train <- data_train_capping[,auc_sort]
data_frame_test <- data_test_capping[,auc_sort]

# 生成所有可能的组合
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

# 自定义函数生成模型
generate_models <- function(data_frame, models) {
  # 初始化结果数据框
  result_data_frame <- NULL
  
  # 为第二个数据框的每一行生成一个模型
  for (i in 1:nrow(models)) {
    
    # 提取第2个数据框的第i行
    model_coefficients <- models[i, ]
    
    # 将数据框转换为矩阵
    matrix1 <- as.matrix(data_frame)
    matrix2 <- as.matrix(t(model_coefficients))
    
    # 计算矩阵乘法
    result_matrix <- matrix1 %*% matrix2/100
    
    # 将结果矩阵转换回数据框，并设置列名为 "model" + 行的序号
    model_data_frame <- data.frame(result_matrix[,1])
    colnames(model_data_frame) <- paste("model", i, sep = "")
    
    # 将模型数据框添加到结果数据框中
    if (is.null(result_data_frame)) {
      result_data_frame <- model_data_frame
    } else {
      result_data_frame <- cbind(result_data_frame, model_data_frame)
    }
  }
  
  return(result_data_frame)
}

data_model_train <- generate_models(data_frame_train, models)
data_model_train$Operation <- data_train_capping$Operation

# 初始化AUC结果数据框
auc_results_train <- data.frame(model = character(),
                                auc = numeric())

# 提取手术方式列
Operation <- as.factor(data_model_train$Operation)

# 绘制ROC曲线
plot(NULL, type = "n", xlim = c(1, 0), ylim = c(0, 1), xlab = "1 - Specificity", ylab = "Sensitivity", main = "ROC Curves for Models of original data")

# 计算每个模型的AUC值并将其添加到结果数据框中
for (i in 1:282) {
  
  # 提取模型名称
  model_name <- paste("model", i, sep = "")
  
  # 计算ROC曲线和AUC值
  roc_obj <- roc(Operation, data_model_train[[model_name]], plot = TRUE, col = rainbow(282)[i], add = TRUE, legacy.axes = TRUE)
  
  # 将AUC值添加到结果数据框中
  auc_results_train <- rbind(auc_results_train, data.frame(model = model_name, auc = auc(roc_obj)))
}


captured_models_plot_origin_ROC <- recordPlot()
save(captured_models_plot_origin_ROC,file = "captured_models_plot_origin_ROC.Robj")


# 显示AUC结果数据框
head(auc_results_train[order(auc_results_train$auc, decreasing = TRUE), ], 10)
models_top10 <- models[c(158,196,167,204,128,151,193,121,194,174),]
models_top10
#查看model158在训练集的准确度
roc_train_model158 <- roc(Operation, data_model_train$model158)
#计算model158的阈值
best_threshold_model158 <- coords(roc_train_model158, "best", best.method = "youden", ret = "threshold")["threshold"] %>% 
  as.numeric()
best_threshold_model158

#多元线性回归模型预测准确性
# 使用最佳阈值将预测值转换为二进制类别（0或1）
# model158_train_predictions <- ifelse(data_model_train$model158 >= best_threshold_model158, 0, 1)
model158_train_predictions <- ifelse(data_model_train$model158 >= best_threshold_model158, 0, 1)
# 计算训练集混淆矩阵和统计信息
confusionMatrix(factor(model158_train_predictions), factor(data_model_train$Operation))

data_model_test <- generate_models(data_frame_test, models)
data_model_test$Operation <- data_test_capping$Operation
model158_test_predictions <- ifelse(data_model_test$model158 >= best_threshold_model158, 0, 1)
# 计算测试集集混淆矩阵和统计信息
confusionMatrix(factor(model158_test_predictions), factor(data_model_test$Operation))





# #分别预测成人和儿童的准确度
# data_adult <- data[data$Age_year>=18, c(7:12,16)]
# data_minor <- data[data$Age_year<18, c(7:12,16)]
# 
# auc_sort <- c(6,3,2,4,5,1)
# data_frame_adult <- data_adult[,auc_sort]
# data_frame_minor <- data_minor[,auc_sort]
# 
# data_model_adult <- generate_models(data_frame_adult, models)
# data_model_minor <- generate_models(data_frame_minor, models)
# data_model_adult$Operation <- data_adult$Operation
# data_model_minor$Operation <- data_minor$Operation
# 
# model158_adult_predictions <- ifelse(data_model_adult$model158 >= best_threshold_model158, 0, 1)
# model158_minor_predictions <- ifelse(data_model_minor$model158 >= best_threshold_model158, 0, 1)
# # 计算测试集集混淆矩阵和统计信息
# confusionMatrix(factor(model158_adult_predictions), factor(data_model_adult$Operation))
# confusionMatrix(factor(model158_minor_predictions), factor(data_model_minor$Operation))







# Calculate ROC curve for model 158 on training data
roc_obj <- roc(Operation, data_model_train$model158)
roc_train_df <- data.frame(
  sensitivity = rev(roc_obj$sensitivities),
  specificity = rev(roc_obj$specificities)
)

# Calculate ROC curve for model 158 on test data
roc_obj <- roc(as.factor(data_model_test$Operation), data_model_test$model158)
roc_test_df <- data.frame(
  sensitivity = rev(roc_obj$sensitivities),
  specificity = rev(roc_obj$specificities)
)

# Plot ROC curve
plot_model158 <- ggplot() +
  geom_segment(data = roc_train_df, aes(x = 1 - specificity, y = sensitivity, xend = c(tail(1 - specificity, -1), 0), yend = c(tail(sensitivity, -1), 0)), color = "red") +
  geom_segment(data = roc_test_df, aes(x = 1 - specificity, y = sensitivity, xend = c(tail(1 - specificity, -1), 0), yend = c(tail(sensitivity, -1), 0)), color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  theme_classic() +
  labs(
    title = "ROC Curve for Model 158",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  annotate("text", x = 0.4, y = 0.6, label = paste("AUC Train =", round(auc(roc(Operation, data_model_train$model158)), 3)), color = "red") +
  annotate("text", x = 0.4, y = 0.5, label = paste("AUC Test =", round(auc(roc(as.factor(data_model_test$Operation), data_model_test$model158)), 3)), color = "blue")


save(plot_model158,file = "plot_model158.robj")



