library(rstatix)
library(skimr)
#데이터 불러오기
df <- read.csv("C:\\Users\\USER\\Downloads\\archive (1)\\car_sales_data.csv", header = TRUE, stringsAsFactors = TRUE)
skim(df)
#결측값 제거
df <- na.omit(df)

#전체 넣고 돌려보기
#다중회귀분석
result <- lm(Price ~ ., data = df)
result1 <- lm(Price ~ 1, data = df) # 다른 변수 고려 X(단순 평균으로 예측)
#ANOVA분석을 활용해 두 집단의 의미있는 차이가 있는지 확인(잔차제곱합 차이의 검증)
anova_result <- anova(result1, result)  
print(anova_result)
#회귀모델
summary(result)
#result result1 차이는 result1이 더 작음

#범주형 제거
df <- select(df, -c("ModelMondeo", "ModelPolo", "ModelYaris", "ModelZ4"))
#levels(df$Fuel.type)
result <- lm(Price ~ ., data = df)
result1 <- lm(Price ~ 1, data = df) # 다른 변수 고려 X(단순 평균으로 예측)
summary(result)
#p-value < 0.05 R^2 71

#모델 저장
saveRDS(result, "C:\\Users\\USER\\Downloads\\archive (1)\\car_regression_model.rds")

#모델 불러오기
loaded_model <- readRDS("C:\\Users\\USER\\Downloads\\archive (1)\\car_regression_model.rds")

#데이터를 csv에서 불러오기
new_data <- read.csv("C:\\Users\\USER\\Downloads\\archive (1)\\car_sales_data.csv")

#모델에 새로운 데이터 추가
predicted <- predict(loaded_model, newdata = new_data)
head(predicted)

#예측 결과 출력
results <- data.frame(new_data, Predicted = predicted)

#RMSE 계산
rmse <- sqrt(mean((results$Price - results$Predicted)^2))
print(paste("RMSE:", round(rmse, 3)))

#1) 선형성 → 예측하고자 하는 종속변수 y와 독립변수 x간의 선형성을 이루어야 함
Fitted <- fitted(result) # 적합값
Residuals <- resid(result) # 잔차


#등분산성 : p>0.05보다 크면, 등분산성 가정을 유지 반대는 이분산성
library(lmtest)
bptest(result)

#독립성 : p>0.05보다 크면 잔차의 독립성을 가짐
library(car)
durbinWatsonTest(result)


#정규성 : p>0.05보다 크면 귀무가설 채택(정규성은 알 수 없다)
ks.test(result$residuals)

#이상치검정 : cook's D 값이 높으면 문제
#세 기준에서(StudRes, Hat CookD) 상위 3개를 각각 뽑아 합친 것
library(ggplot2)
cand <- influencePlot(result, id = list(n = 3))

mf <- model.frame(result) # 모형에 실제로 쓰인 데이터 (결측 제외된 상태)
z <- round(scale(mf[sapply(mf, is.numeric)]), 1)

rownames(z) <- rownames(mf) # 행 이름을 다시 붙임
cbind(round(cand, 2), z[rownames(cand), ])

#CookD값이 0.5미만이면 이상치없다.
#max Cook's D < 0.5 → 영향점 없음, 0.5~1 → 확인, > 1 → 영향점
max(cooks.distance(result))


#다중공선성(변수의 중복성) : 10이상 GVIF^(1/(2*DF))
vif_values <- vif(result)

#변수선택
#AIC(Akaike information criterion) AIC값이 줄어들면 그 변수는 의미가 있음
library("MASS")
#후진소거법
result_bk = lm(charges ~., data=df)
Df_fit_bk =stepAIC(result_bk, direction= "backward", trace = T) #trace 하나씩 빼는 과정을 보여줌
#단계적선택법
Df_fit_st = stepAIC(result_bk, direction = "both", trace = T)
#전진선택법
result_fw = lm(charges ~ 1, data=df)
Df_fit_fk =stepAIC(result_fw, direction= "forward",scope = list(lower = result_fw, upper = result_bk) ,trace = T)


#https://www.kaggle.com/datasets/msnbehdani/mock-dataset-of-second-hand-car-sales?resource=download
