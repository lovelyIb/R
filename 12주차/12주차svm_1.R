library(e1071) # SVM 모델
library(caret)
# 데이터 불러오기
df <- read.csv("C:\\Users\\USER\\Downloads\\telecom_churn.csv")
# 종속변수에 대한 데이터 요소로 변환
df$Churn <- as.factor(df$Churn)
#데이터 나누기 학습 80%, 테스트 20%
trainIndex <- createDataPartition(df$Churn, p = 0.8, list = FALSE)
train_data <- df[trainIndex,]
test_data <- df[-trainIndex,]
cost_value <- 1 # Cost(C)
gamma_value <- 0.1 # Gamma
# SVM모델 생성
svm_model <- svm(train_data$Churn ~ ., data = train_data, 
              type = 'C-classification', #Soft margin
              kernel = 'polynomial', #다항커널
              cost = cost_value,
              gamma = gamma_value,
              probability = TRUE) #확률값으로 도출

summary(svm_model)

library(pROC)
predicted_probs <- predict(svm_model, test_data, probability = TRUE)
predicted_probs
#행 하나하나가 테스트 데이터의 수
#22번 기준으로 No 96% yes 3.4%
#이건 수치값이 아님. 그래서 yes개체의 속성을 추출 후 작업.
predicted_probs <- attr(predicted_probs, "probabilities")[, "Yes"] #객체의 속성을 추출하는 함수

predicted_classes <- ifelse(predicted_probs > 0.5, "Yes", "No")
predicted_classes <- as.factor(predicted_classes)
predicted_classes
#확률적으로 있던게 yes, no로 바뀌게 됨.

conf_matrix <- confusionMatrix(predicted_classes, test_data$Churn)
print(conf_matrix)
roc_curve <- roc(test_data$Churn, predicted_probs)
plot(roc_curve) #roc 커브 그림
auc(roc_curve) #정확도

#검정에 대한 것도 없고, 이상치 제거도 없고 변수제거도 안하고