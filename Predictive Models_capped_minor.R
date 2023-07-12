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

#提取未成年数据
data<- data[data$Age_year<18, ]


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
set.seed(123)
trainIndex <- createDataPartition(data$Operation, p = 0.8, list = FALSE, times = 1, groups = 2)
data_train_capped <- data[trainIndex, c(7:12,16)]
data_test_capped <- data[-trainIndex,  c(7:12,16)]
#设置封顶边缘，优化数据
data_train_capped[,1:6] <- lapply(data_train_capped[,1:6], function(x) ifelse(x >= 10, 10, x))
data_test_capped[,1:6] <- lapply(data_test_capped[,1:6], function(x) ifelse(x >= 10, 10, x))



#多元线性回归模型#####
#利用ROC曲线评估各个边预测封堵的重要性
# 提取分类变量，保留为数值向量
data_train_capped <- as.data.frame(data_train_capped)
response <- as.vector(data_train_capped[, 7])

# 初始化一个列表来存储每个变量的ROC曲线和AUC
roc_list <- list()
auc_list <- numeric(length = 6)

# 遍历每个连续变量（1-6列）
for (i in 1:6) {
  predictor <- as.numeric(data_train_capped[, i])
  roc_obj <- roc(response, predictor, levels = c(0, 1))
  
  # 存储ROC对象和AUC
  roc_list[[i]] <- roc_obj
  auc_list[i] <- auc(roc_obj)
}

# 打印AUC值
print(auc_list)

# 可视化ROC曲线
# plot(roc_list[[1]], col = "blue", main = "ROC Curves")
plot(NULL, type = "n", xlim = c(1, 0), ylim = c(0, 1), xlab = "1 - Specificity", ylab = "Sensitivity", main = "ROC Curves for minor capped data")
for (i in 1:6) {
  plot(roc_list[[i]], col = i + 1, add = TRUE)
}
legend("bottomright", legend = paste(colnames(data_train_capped)[1:6], "AUC:", round(auc_list, 3)), col = 2:7, lwd = 2)
legend

#捕获图形
captured_plot_capped_ROC <- recordPlot()
save(captured_plot_capped_ROC,file = "captured_plot_capped_ROC.Robj")


# 创建示例数据框,对6个特征重新排序
auc_sort <- c(6,3,2,4,5,1)
data_frame_train_capped <- data_train_capped[,auc_sort]
data_frame_test_capped <- data_test_capped[,auc_sort]

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

data_model_train_capped <- generate_models(data_frame_train_capped, models)
data_model_train_capped$Operation <- data_train_capped$Operation

# 初始化AUC结果数据框
auc_results_train_capped <- data.frame(model = character(),
                                auc = numeric())

# # 提取手术方式列
# Operation <- as.factor(data_model_train_capped$Operation)

# 绘制ROC曲线
plot(NULL, type = "n", xlim = c(1, 0), ylim = c(0, 1), xlab = "1 - Specificity", ylab = "Sensitivity", main = "ROC Curves for Models of capped data")

# 计算每个模型的AUC值并将其添加到结果数据框中
for (i in 1:282) {
  
  # 提取模型名称
  model_name <- paste("model", i, sep = "")
  
  # 计算ROC曲线和AUC值
  roc_obj <- roc(data_model_train_capped$Operation, data_model_train_capped[[model_name]], plot = TRUE, col = rainbow(282)[i], add = TRUE, legacy.axes = TRUE)
  
  # 将AUC值添加到结果数据框中
  auc_results_train_capped <- rbind(auc_results_train_capped, data.frame(model = model_name, auc = auc(roc_obj)))
}


captured_models_plot_capped_ROC <- recordPlot()
save(captured_models_plot_capped_ROC,file = "captured_models_plot_capped_ROC.Robj")

# 显示AUC结果数据框
head(auc_results_train_capped[order(auc_results_train_capped$auc, decreasing = TRUE), ], 20)
models_top10 <- models[c(249,196,237,215,223,205,167,204,207,158),]
models_top10
#查看model249在训练集的准确度
roc_train_model249 <- roc(data_model_train_capped$Operation, data_model_train_capped$model249)
#计算model249的阈值
best_threshold_model249 <- coords(roc_train_model249, "best", best.method = "youden", ret = "threshold")["threshold"] %>% 
  as.numeric()
best_threshold_model249

#多元线性回归模型预测准确性
# 使用最佳阈值将预测值转换为二进制类别（0或1）
# model249_train_predictions <- ifelse(data_model_train_capped$model249 >= best_threshold_model249, 0, 1)
model249_train_predictions <- ifelse(data_model_train_capped$model249 >= best_threshold_model249, 0, 1)
# 计算训练集混淆矩阵和统计信息
confusionMatrix(factor(model249_train_predictions), factor(data_model_train_capped$Operation))

data_model_test_capped <- generate_models(data_frame_test_capped, models)
data_model_test_capped$Operation <- data_test_capped$Operation
model249_test_predictions <- ifelse(data_model_test_capped$model249 >= best_threshold_model249, 0, 1)
# 计算测试集集混淆矩阵和统计信息
confusionMatrix(factor(model249_test_predictions), factor(data_model_test_capped$Operation))


# Calculate ROC curve for model 249 on training data
roc_obj_train <- roc(data_model_train_capped$Operation, data_model_train_capped$model249)
roc_train_df <- data.frame(
  sensitivity = rev(roc_obj_train$sensitivities),
  specificity = rev(roc_obj_train$specificities)
)

# Calculate ROC curve for model 249 on test data
roc_obj_nimor <- roc(as.factor(data_model_test_capped$Operation), data_model_test_capped$model249)
roc_test_df <- data.frame(
  sensitivity = rev(roc_obj_nimor$sensitivities),
  specificity = rev(roc_obj_nimor$specificities)
)

# Plot ROC curve
plot_model249_train_test <- ggplot() +
  geom_segment(data = roc_train_df, aes(x = 1 - specificity, y = sensitivity, xend = c(tail(1 - specificity, -1), 0), yend = c(tail(sensitivity, -1), 0)), color = "red") +
  geom_segment(data = roc_test_df, aes(x = 1 - specificity, y = sensitivity, xend = c(tail(1 - specificity, -1), 0), yend = c(tail(sensitivity, -1), 0)), color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  theme_classic() +
  labs(
    title = "ROC Curve for Model 249",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  annotate("text", x = 0.4, y = 0.6, label = paste("AUC train =", round(auc(roc(data_model_train_capped$Operation, data_model_train_capped$model249)), 3)), color = "red") +
  annotate("text", x = 0.4, y = 0.5, label = paste("AUC test =", round(auc(roc(as.factor(data_model_test_capped$Operation), data_model_test_capped$model249)), 3)), color = "blue")

plot_model249_train_test
save(plot_model249_train_test,file = "plot_model249_train_test.robj")
load("plot_model158.robj")

plot_model158/plot_model249_train_test











#分别预测成人和儿童的准确度
data_adult_capped  <- data[data$Age_year>=18, c(7:12,16)]
data_minor_capped <- data[data$Age_year<18, c(7:12,16)]

#设置封顶边缘，优化数据
data_adult_capped[,1:6] <- lapply(data_adult_capped[,1:6], function(x) ifelse(x >= 10, 10, x))
data_minor_capped[,1:6] <- lapply(data_minor_capped[,1:6], function(x) ifelse(x >= 10, 10, x))


auc_sort <- c(6,3,2,4,5,1)
data_frame_adult_capped <- data_adult_capped[,auc_sort]
data_frame_minor_capped <- data_minor_capped[,auc_sort]

data_model_adult_capped <- generate_models(data_frame_adult_capped, models)
data_model_minor_capped <- generate_models(data_frame_minor_capped, models)
data_model_adult_capped$Operation <- data_adult_capped$Operation
data_model_minor_capped$Operation <- data_minor_capped$Operation

model249_adult_predictions <- ifelse(data_model_adult_capped$model249 >= best_threshold_model249, 0, 1)
model249_minor_predictions <- ifelse(data_model_minor_capped$model249 >= best_threshold_model249, 0, 1)
# 计算测试集集混淆矩阵和统计信息
confusionMatrix(factor(model249_adult_predictions), factor(data_model_adult_capped$Operation))
confusionMatrix(factor(model249_minor_predictions), factor(data_model_minor_capped$Operation))





# Calculate ROC curve for model 249 on adulting data
roc_obj_adult <- roc(data_model_adult_capped$Operation, data_model_adult_capped$model249)
roc_adult_df <- data.frame(
  sensitivity = rev(roc_obj_adult$sensitivities),
  specificity = rev(roc_obj_adult$specificities)
)

# Calculate ROC curve for model 249 on minor data
roc_obj_nimor <- roc(as.factor(data_model_minor_capped$Operation), data_model_minor_capped$model249)
roc_minor_df <- data.frame(
  sensitivity = rev(roc_obj_nimor$sensitivities),
  specificity = rev(roc_obj_nimor$specificities)
)

# Plot ROC curve
plot_model249_adult_minor <- ggplot() +
  geom_segment(data = roc_adult_df, aes(x = 1 - specificity, y = sensitivity, xend = c(tail(1 - specificity, -1), 0), yend = c(tail(sensitivity, -1), 0)), color = "red") +
  geom_segment(data = roc_minor_df, aes(x = 1 - specificity, y = sensitivity, xend = c(tail(1 - specificity, -1), 0), yend = c(tail(sensitivity, -1), 0)), color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  theme_classic() +
  labs(
    title = "ROC Curve for Model 249",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  annotate("text", x = 0.4, y = 0.6, label = paste("AUC adult =", round(auc(roc(data_model_adult_capped$Operation, data_model_adult_capped$model249)), 3)), color = "red") +
  annotate("text", x = 0.4, y = 0.5, label = paste("AUC minor =", round(auc(roc(as.factor(data_model_minor_capped$Operation), data_model_minor_capped$model249)), 3)), color = "blue")

plot_model249_adult_minor
save(plot_model249_adult_minor,file = "plot_model249_adult_minor.robj")




load("plot_model249_adult_minor.robj")
load("plot_forest_adult_minor.Robj")
load("plot_logistic_adult_minor.Robj")

plot_logistic_adult_minor/plot_forest_adult_minor/plot_model249_adult_minor




adult_predict_249_data <- data_model_adult_capped[,c("model249","Operation")]
minor_predict_249_data <- data_model_minor_capped[,c("model249","Operation")]


# Create bins
adult_predict_249_data <- adult_predict_249_data %>%
  mutate(binned_model = cut(model249, breaks = seq(0, 10, by = 1), include.lowest = TRUE, labels = FALSE)) %>%
  group_by(binned_model, Operation) %>%
  summarise(count = n(), .groups = 'drop') %>%
  spread(Operation, count, fill = 0)

# Check the output
print(adult_predict_249_data)

# Write the output to a CSV file
write.csv(adult_predict_249_data, file = "adult_predict_249_data_output.csv")


# Create bins
minor_predict_249_data <- minor_predict_249_data %>%
  mutate(binned_model = cut(model249, breaks = seq(0, 10, by = 1), include.lowest = TRUE, labels = FALSE)) %>%
  group_by(binned_model, Operation) %>%
  summarise(count = n(), .groups = 'drop') %>%
  spread(Operation, count, fill = 0)

# Check the output
print(minor_predict_249_data)

# Write the output to a CSV file
write.csv(minor_predict_249_data, file = "minor_predict_249_data_output.csv")
table(adult_predict_249_data$Operation)
