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

install.packages("tidyverse")
install.packages("tidymodels")
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

#=====#
library(pROC)
library(caret)
library(e1071)
# Test 데이터셋 결과
test_predictions <- predict(result2, newdata = test_data, type = "response")
print(test_predictions)
predicted_class <- ifelse(test_predictions > 0.5, "yes", "no")
predicted_class <- as.factor(predicted_class)
print(predicted_class)
# 모델의 Confusion matrix
confusion_matrix <- confusionMatrix(predicted_class, test_data$Personal.Loan)
confusion_matrix

# ROC curve and AUC
roc_curve <- roc(test_data$Personal.Loan, test_predictions)
plot(roc_curve)
auc(roc_curve)
