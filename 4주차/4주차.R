install.packages("rstatix") #통계 텍스트
install.packages("skimr") #데이터의 흐름 요약

library(rstatix)
library(skimr)

#데이터 불러오기
df <- read.csv("C:\\Users\\USER\\Downloads\\02.다중회귀분석_실습\\02.diabetes.csv", header = TRUE, stringsAsFactors =  TRUE)
skim(df)


#결측값 제거
df <- na.omit(df)

#Outcome < 범주형 변수인데 연속적으로 인식해서 불러왔음.

#불필요 변수 제거
df <- select(df, -c(Outcome)) #하나만 제거(순수한 열 이름만 적기)
df <- select(df, -c(Pregnancies, BloodPressure, BMI, Age)) #여러 개 제거

#다중회귀분석
result <- lm(df$Diabetes ~ ., data = df)
result1 <- lm(df$Diabetes ~ 1, data = df) #다른변수 고려 x 평균으로만 예측

#ANOVA분석을 활용해 두 집단의 의미있는 차이가 있는지 확인(잔차제곱합 차이의 검증)
anova_result <- anova(result1, result)
print(anova_result)

#회귀모델
summary(result)
# 상수는 0.05보다 작고 p-value가 0.05보다 작은 3개만 사용할 수 있다.
#때문에 -c 한거

#모델 저장
saveRDS(result2, "C:\\Users\\USER\\Downloads\\02.다중회귀분석_실습\\regression_model.rds")

#모델 불러오기
loaded_model <- readRDS("C:\\Users\\USER\\Downloads\\02.다중회귀분석_실습\\regression_model.rds")

#데이터를 csv에서 불러오기
new_data <- read.csv("C:\\Users\\USER\\Downloads\\02.다중회귀분석_실습\\02.diabetes_test.csv")

#모델에 새로운 데이터 추가
predicted <- predict(loaded_model, newdata = new_data)
head(predicted)

#예측 결과 출력
results <- data.frame(new_data, Predicted = predicted)


#RMSE 계산
rmse <- sqrt(mean((results$Diabetes - results$Predicted)^2))
print(paste("RMSE:", round(rmse, 3)))


#<등분산성>
StdResid <- sqrt(abs(df$Residuals)) # 표준화된 잔차의 크기 근사치

ggplot(df, aes(x = Fitted, y = StdResid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Scale-Location Plot (등분산성 확인)",
       x = "적합값 (Fitted Values)",
       y = "Standardized Residuals") +
  theme_minimal()

#등분산성 : p>0.05보다 크면, 등분산성 가정을 유지 반대는 이분산성
library(lmtest)
bptest(result)

#정규성 p> 0.05 이면 귀무가설 채택
library(car)
durbinWatsonTest(result)
shapiro_test(result$residuals) #0.05보다 작으니 정규성이 없다.

#4개의 조건에 대한 그래프
opar <- par(no.readonly = TRUE) #그래프가 들어갈 window
par(mfrow = c(2,2)) # 2x2공간
plot(result) #그래프 결과
par(opar) #그래프 매개변수

#이상치검정 : cook's D 값이 높으면 문제
#세 기준에서(StudRes, Hat CookD) 상위 3개를 각각 뽑아 합친 것
library(ggplot2)
cand <- influencePlot(result, id = list(n = 3))

mf <- model.frame(result) # 모형에 실제로 쓰인 데이터 (결측 제외된 상태)
z <- round(scale(mf[sapply(mf, is.numeric)]), 1)

rownames(z) <- rownames(mf) # 행 이름을 다시 붙임
cbind(round(cand, 2), z[rownames(cand), ])

#CookD값이 0.5미만이면 이상치없다.
#일단 229와 446제거해보자

#이상치 제거
out_rows <- c("229", "446") #확인 후 직접 입력
df2 <- df[!rownames(df) %in% out_rows, ]

#이상치가 제거된 데이터로 다시 분석 및 그래프 추출
result2 <- lm(Diabetes ~ ., data = df2)
cand <- influencePlot(result2, id = list(n = 3))

#max Cook's D < 0.5 → 영향점 없음, 0.5~1 → 확인, > 1 → 영향점
max(cooks.distance(result2))
