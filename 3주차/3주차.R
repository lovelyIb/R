install.packages("psych")

#키와 몸무게 데이터 생성
heights <- c(160, 162, 155, 180, 170, 175, 165, 171, 177, 172)
weights <- c(55, 60, 53, 72, 70, 73, 62, 64, 69, 65)

library(psych)
#피어슨상관계수 도출 및 p-value
result_pearson=corr.test(heights, weights, method="pearson")

result_pearson$p
result_pearson$r

#데이터 불러오기
train_data <- read.csv("C:\\Users\\USER\\Downloads\\단순회귀분석_데이터셋\\01.simple_train_data.csv") #먼저 학습
test_data <- read.csv("C:\\Users\\USER\\Downloads\\단순회귀분석_데이터셋\\01.simple_test_data.csv") #검증

#회귀분석 모델
model <- lm(upper_arm ~ thigh, data = train_data)
summary(model)

#모델 저장
saveRDS(model, "regression_model.rds")

#모델 불러오기
loaded_model <- readRDS("regression_model.rds")

#불러온 모델을 통한 예측
predicted <- predict(loaded_model, newdata = test_data)
head(predicted)

#예측 결과와 실제값 비교
results <- data.frame(Actual = test_data$upper_arm, Predicted = predicted)

#RMSE 계산
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))
print(paste("RMSE:", round(rmse,3)))

install.packages("ggplot2")
library(ggplot2)

ggplot(train_data, aes(x = thigh, y = upper_arm)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) + #회귀선 (표준오차 리본 제거)
  labs(title = "대퇴부 vs 상박부 회귀분석",
       x = "대퇴부 둘레 (cm)",
       y = "상박부 둘레 (cm)")