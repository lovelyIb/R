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
library(ggplot2)
library("MASS")

#후진소거법
result_bk = lm(df$Diabetes ~., data=df)
Df_fit_bk =stepAIC(result_bk, direction= "backward", trace = T) #trace 하나씩 빼는 과정을 보여줌

#전진선택법
result_fw = lm(df$Diabetes ~ 1, data=df)
Df_fit_fk = stepAIC(result_fw, direction= "forward",scope = (df$Diabetes ~ Pregnancies + SkinThickness + Insulin + Age + Outcome),trace = T)
                   