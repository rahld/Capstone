library(tidyverse)
library(randomForest)
library(corrplot)
library(caret)
library(glmnet)
library(xgboost)
library(class)
library(ggplot2)




data <- read.csv("C:/Users/rahld/Desktop/sw/data/online_shoppers_intention.csv")

# 범주형 => 수치형
data <- data[, -c(12:15)]
data$Month <- as.numeric(factor(data$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "June", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
data$VisitorType <- ifelse(data$VisitorType == "New_Visitor", 0, 1)
data$Weekend <- ifelse(data$Weekend == FALSE, 0, 1)

# 결측값
table(is.na(data))

# 이상값
boxplot(data$ProductRelated_Duration)
Q1 <- quantile(data$ProductRelated_Duration, 0.25)
Q3 <- quantile(data$ProductRelated_Duration, 0.75)
IQR_value <- IQR(data$ProductRelated_Duration)
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
data <- data[data$ProductRelated_Duration >= lower_bound & data$ProductRelated_Duration <= upper_bound, ]

unique_values <- unique(data$Revenue)
as.factor(data$Revenue)

data$Revenue <- ifelse(data$Revenue == FALSE, 0, 1)




# 상관분석
correlation_matrix <- cor(data, use = "complete.obs")

# 상관 행렬 시각화
corrplot(correlation_matrix, method = "circle", 
         type = "upper", 
         order = "hclust", 
         tl.col = "black", 
         tl.srt = 45,
         tl.cex = 0.5,
         number.cex = 0.5,
         addCoef.col = "black")


# RandomForest
set.seed(123)
train_index <- sample(1:nrow(data), size = round(nrow(data) * 0.8))
train_set <- data[train_index, ]
test_set <- data[-train_index, ]
rf <- randomForest(as.factor(Revenue) ~., data=train_set, ntree = 500)
print(rf)

# 변수중요도
importance(rf)
varImpPlot(rf)

# 2차 RF
rf2 <- randomForest(as.factor(Revenue) ~ PageValues + 
                      ExitRates + 
                      ProductRelated_Duration, data=train_set, ntree = 500)
print(rf2)

# 4 RF
train_index2 <- sample(1:nrow(data), size = round(nrow(data) * 0.8))
train_set2 <- data[train_index2, ]  # train_index2 사용
test_set2 <- data[-train_index2, ]  # train_index2 사용
rf4 <- randomForest(as.factor(Revenue) ~ PageValues + 
                      ExitRates + 
                      ProductRelated_Duration + 
                      Administrative_Duration + 
                      BounceRates + 
                      Month + 
                      VisitorType, data = train_set2, ntree = 500)
print(rf4)

################## 3차 RF (최최종) ###################
set.seed(123)
train_index <- sample(1:nrow(data), size = round(nrow(data) * 0.8))
train_set <- data[train_index, ]
test_set <- data[-train_index, ]
rf3 <- randomForest(as.factor(Revenue) ~ PageValues + 
                      ExitRates + 
                      ProductRelated_Duration + 
                      Administrative_Duration + 
                      BounceRates + 
                      Month + 
                      VisitorType, data=train_set, ntree = 500)
print(rf3)
predicted_rf3 <- predict(rf3, newdata = test_set)

# 혼동 행렬 생성
actual_rf3 <- test_set$Revenue
confusion_matrix_rf3 <- table(Predicted = predicted_rf3, Actual = actual_rf3)
print(confusion_matrix_rf3)

# 변수 중요도 확인 및 시각화
importance(rf3)
varImpPlot(rf3, main = "Variable Importance Plot", col = "steelblue")



########
# 변수 중요도 데이터 정리
importance_rf <- data.frame(Variable = rownames(importance(rf3)), Importance = importance(rf3)[, 1])
importance_rf <- importance_rf[order(-importance_rf$Importance), ]

# 그래프 그리기
ggplot(importance_rf, aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  coord_flip() +
  scale_fill_gradient(low = "skyblue", high = "steelblue") +
  labs(title = "Variable Importance (Random Forest)", 
       y = "Importance", 
       x = "Variable") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
########


# 정밀도 (Precision) 계산: TP / (TP + FP)
precision_rf3 <- confusion_matrix_rf3[2, 2] / sum(confusion_matrix_rf3[2, ])

# 재현율 (Recall) 계산: TP / (TP + FN)
recall_rf3 <- confusion_matrix_rf3[2, 2] / sum(confusion_matrix_rf3[, 2])

# 정확도 (Accuracy) 계산: (TP + TN) / (TP + TN + FP + FN)
accuracy_rf3 <- sum(diag(confusion_matrix_rf3)) / sum(confusion_matrix_rf3)


# 결과 출력
cat("Precision:", round(precision_rf3, 4), "\n")
cat("Recall:", round(recall_rf3, 4), "\n")
cat("Accuracy:", round(accuracy_rf3, 4), "\n")


# F1-스코어 계산
f1_score_rf3 <- 2 * (precision_rf3 * recall_rf3) / (precision_rf3 + recall_rf3)
cat("F1-Score:", round(f1_score_rf3, 4), "\n")




############### XGBOOST #################

# 데이터 준비
train_set_xgb <- train_set
test_set_xgb <- test_set

# 데이터셋을 DMatrix 형식으로 변환
dtrain <- xgb.DMatrix(data = as.matrix(train_set_xgb[, -which(names(train_set_xgb) == "Revenue")]), label = train_set_xgb$Revenue)
dtest <- xgb.DMatrix(data = as.matrix(test_set_xgb[, -which(names(test_set_xgb) == "Revenue")]), label = test_set_xgb$Revenue)

# XGBoost 모델 학습
params <- list(
  objective = "binary:logistic",  # 이진 분류
  eval_metric = "logloss",        # 평가 지표로 logloss 사용
  scale_pos_weight = sum(train_set_xgb$Revenue == 0) / sum(train_set_xgb$Revenue == 1)  # 클래스 불균형 조정
)

# 학습
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 500)

# 예측
predictions_xgb <- predict(xgb_model, newdata = dtest)

# 예측 결과 이진화
predictions_xgb_binary <- ifelse(predictions_xgb > 0.5, 1, 0)

# 혼동 행렬
confusion_matrix_xgb <- table(Predicted = predictions_xgb_binary, Actual = test_set_xgb$Revenue)
print(confusion_matrix_xgb)

# 정밀도, 재현율, 정확도 계산
precision_xgb <- confusion_matrix_xgb[2, 2] / sum(confusion_matrix_xgb[2, ])
recall_xgb <- confusion_matrix_xgb[2, 2] / sum(confusion_matrix_xgb[, 2])
accuracy_xgb <- sum(diag(confusion_matrix_xgb)) / sum(confusion_matrix_xgb)

# F1-스코어 계산
f1_score_xgb <- 2 * (precision_xgb * recall_xgb) / (precision_xgb + recall_xgb)

cat("Precision:", round(precision_xgb, 4), "\n")
cat("Recall:", round(recall_xgb, 4), "\n")
cat("Accuracy:", round(accuracy_xgb, 4), "\n")
cat("F1-Score:", round(f1_score_xgb, 4), "\n")




########## 로지스틱 ##########

# 로지스틱 회귀 모델 학습
logistic_model <- glm(Revenue ~ PageValues + ExitRates + ProductRelated_Duration + 
                        Administrative_Duration + BounceRates + Month + VisitorType,
                      data = train_set, family = "binomial")

# 모델 요약
summary(logistic_model)

# 테스트 데이터에서 예측
predicted_logistic <- predict(logistic_model, newdata = test_set, type = "response")

# 예측값을 0.5 임계값을 기준으로 이진화
predicted_logistic_binary <- ifelse(predicted_logistic > 0.5, 1, 0)

# 혼동 행렬 생성
confusion_matrix_logistic <- table(Predicted = predicted_logistic_binary, Actual = test_set$Revenue)
print(confusion_matrix_logistic)

# 정밀도, 재현율, 정확도 계산
precision_logistic <- confusion_matrix_logistic[2, 2] / sum(confusion_matrix_logistic[2, ])
recall_logistic <- confusion_matrix_logistic[2, 2] / sum(confusion_matrix_logistic[, 2])
accuracy_logistic <- sum(diag(confusion_matrix_logistic)) / sum(confusion_matrix_logistic)


# F1-스코어 계산
f1_score_logistic <- 2 * (precision_logistic * recall_logistic) / (precision_logistic + recall_logistic)

cat("Precision:", round(precision_logistic, 4), "\n")
cat("Recall:", round(recall_logistic, 4), "\n")
cat("Accuracy:", round(accuracy_logistic, 4), "\n")
cat("F1-Score:", round(f1_score_logistic, 4), "\n")





############## knn ##############

# KNN 모델 학습 (k = 5로 설정)
k <- 5
knn_model <- knn(train = train_set[, -which(names(train_set) == "Revenue")], 
                 test = test_set[, -which(names(test_set) == "Revenue")], 
                 cl = train_set$Revenue, k = k)

# 혼동 행렬 생성
confusion_matrix_knn <- table(Predicted = knn_model, Actual = test_set$Revenue)
print(confusion_matrix_knn)

# 정밀도, 재현율, 정확도 계산
precision_knn <- confusion_matrix_knn[2, 2] / sum(confusion_matrix_knn[2, ])
recall_knn <- confusion_matrix_knn[2, 2] / sum(confusion_matrix_knn[, 2])
accuracy_knn <- sum(diag(confusion_matrix_knn)) / sum(confusion_matrix_knn)


# F1-스코어 계산
f1_score_knn <- 2 * (precision_knn * recall_knn) / (precision_knn + recall_knn)

cat("Precision:", round(precision_knn, 4), "\n")
cat("Recall:", round(recall_knn, 4), "\n")
cat("Accuracy:", round(accuracy_knn, 4), "\n")
cat("F1-Score:", round(f1_score_knn, 4), "\n")


results <- data.frame(
  Model = c("Random Forest", "XGBoost", "Logistic Regression", "KNN"),
  Accuracy = c(accuracy_rf3, accuracy_xgb, accuracy_logistic, accuracy_knn),
  Precision = c(precision_rf3, precision_xgb, precision_logistic, precision_knn),
  Recall = c(recall_rf3, recall_xgb, recall_logistic, recall_knn),
  F1_Score = c(f1_score_rf3, f1_score_xgb, f1_score_logistic, f1_score_knn)
)
print(results)

results_long <- pivot_longer(results, cols = -Model, names_to = "Metric", values_to = "Value")

##########

# 결과 데이터를 장기 형식으로 변환
results_long <- pivot_longer(results, cols = -Model, names_to = "Metric", values_to = "Value")

# 그래프 그리기
ggplot(results_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6, color = "black") +
  scale_fill_manual(values = c("Accuracy" = "#FFB5B5", 
                    "Precision" = "#8FBBD9", 
                    "Recall" = "#A3D9A5", 
                    "F1_Score" = "#F5C26B")) +
  labs(title = "Model Performance Comparison", 
       y = "Score", 
       x = "Model") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))


