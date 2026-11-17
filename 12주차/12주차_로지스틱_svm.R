library(GGally) 
library(skimr)
library(tidyverse)
library(tidymodels)
library(caret)
df <- read.csv("C:\\Users\\USER\\Downloads\\03.로지스틱회귀분석\\UniversalBank.csv", header = TRUE, na = ".")

skim(df)

#0과 1만 있는거 제거
df <- select(df, -c(Securities.Account, CD.Account, Online, CreditCard))
df$Personal.Loan <- ifelse(df$Personal.Loan == 1, "yes", "no") #1이면 yes 아니면 no
df$Personal.Loan <- as.factor(df$Personal.Loan)

#전체데이터를 학습과 테스트로 나눔
trainIndex <- createDataPartition(df$Personal.Loan, p = 0.8, list = FALSE)
train_data <- df[trainIndex,]
test_data <- df[-trainIndex,]

#분석
result1=glm(train_data$Personal.Loan ~1, data= train_data, family = "binomial")
result2=glm(train_data$Personal.Loan ~., data= train_data, family = "binomial") 
anova(result1, result2, test = "Chisq") #p < 0.05 모델 전체가 유의한가?
summary(result2)

library(tidyverse)
library(tidymodels)

tidy_result <- tidy(result2)
tidy_result <- mutate_if(tidy_result, is.numeric, round, 3)
print(tidy_result)

summary(result2) #실제로 종속변수에 영향을 끼치는것들 확인
#0.05보다 큰것들 제거 (영향을 끼치지 않는거 제거)
df_result2 <- select(train_data, -c(ID, Age, Experience, ZIP.Code, Mortgage))
#다시 회귀분석

result2=glm(train_data$Personal.Loan ~., data= df_result2, family = "binomial") 
summary(result2)

#summary했으니 ROC하고 AUC
library(pROC)
library(caret)
library(e1071)
# Test 데이터셋 결과
test_predictions <- predict(result2, newdata = test_data, type = "response")
print(test_predictions)
predicted_class <- ifelse(test_predictions > 0.5, "1", "0")
predicted_class <- as.factor(predicted_class)
print(predicted_class)
# 모델의 Confusion matrix
confusion_matrix <- confusionMatrix(predicted_class, test_data$Personal.Loan)
confusion_matrix

# ROC curve and AUC
roc_curve <- roc(test_data$Personal.Loan, test_predictions)
plot(roc_curve)
auc(roc_curve)

#==== SVM 모델로 분석 해보기#
library(e1071) # SVM 모델
library(caret)
# 데이터 불러오기
df <- read.csv("C:\\Users\\USER\\Downloads\\03.로지스틱회귀분석\\UniversalBank.csv", header = TRUE, na = ".")
# 종속변수에 대한 데이터 요소로 변환
df$Personal.Loan <- as.factor(df$Personal.Loan)
#데이터 나누기 학습 80%, 테스트 20%
trainIndex <- createDataPartition(df$Personal.Loan, p = 0.8, list = FALSE)
train_data <- df[trainIndex,]
test_data <- df[-trainIndex,]
cost_value <- 1 # Cost(C)
gamma_value <- 0.1 # Gamma
# SVM모델 생성
svm_model <- svm(train_data$Personal.Loan ~ ., data = train_data, 
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
predicted_probs <- attr(predicted_probs, "probabilities")[, "1"] #객체의 속성을 추출하는 함수

predicted_classes <- ifelse(predicted_probs > 0.5, "1", "0")
predicted_classes <- as.factor(predicted_classes)
predicted_classes
#확률적으로 있던게 1, 0로 바뀌게 됨.

conf_matrix <- confusionMatrix(predicted_classes, test_data$Personal.Loan)
print(conf_matrix)
roc_curve <- roc(test_data$Personal.Loan, predicted_probs)
plot(roc_curve) #roc 커브 그림
auc(roc_curve) #정확도

#검정에 대한 것도 없고, 이상치 제거도 없고 변수제거도 안하고