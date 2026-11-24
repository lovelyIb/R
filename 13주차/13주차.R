library(caret)
library(rpart) #CART모델
library(rpart.plot)
library(skimr)
df=read.csv("C:\\Users\\USER\\Desktop\\의사결정나무\\2class.csv")
skim(df)
df$class <- as.factor(df$class)
train_indices <- createDataPartition(df$class, p = 0.8, list = FALSE)
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

#Hyper parameters 설정
control_params <- rpart.control(cp = 0.01, maxdepth = 5, minsplit = 10) 
#Complexity Parameter(분할될 때 마다 모델이 최소 1%는 개선이 되어야 함)
#maxdepth(트리의 깊이가 최대값), minsplit(각각의 노드에 최소 10개의 관측치가 있도록 분할)
#모델 생성
tree_model <- rpart(class ~ ., data = train_data, method = "class", control = control_params)
#모델 결과
summary(tree_model)
rpart.plot(tree_model)
predicted_probs <- predict(tree_model, test_data, type = "prob")[,1]
predicted_probs
predicted_classes <- ifelse(predicted_probs > 0.5, "A", "B")
predicted_classes <- as.factor(predicted_classes)
predicted_classes

library(pROC)
#Confusion Matrix 도출
confusion_matrix <- confusionMatrix(predicted_classes, test_data$class)
print(confusion_matrix)
#ROC Curveroc_curve <- roc(test_data$class, predicted_probs)
plot(roc_curve)
auc(roc_curve)

#====다중분류 코드====
library(caret)
library(rpart) #CART모델
library(rpart.plot)
library(skimr)
df=read.csv("C:\\Users\\USER\\Desktop\\의사결정나무\\3class.csv")
skim(df)
df$class <- as.factor(df$class)
train_indices <- createDataPartition(df$class, p = 0.8, list = FALSE)
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

#Hyper parameters 설정
control_params <- rpart.control(cp = 0.01, maxdepth = 5, minsplit = 10) 
#Complexity Parameter(분할될 때 마다 모델이 최소 1%는 개선이 되어야 함)
#maxdepth(트리의 깊이가 최대값), minsplit(각각의 노드에 최소 10개의 관측치가 있도록 분할)
#모델 생성
tree_model <- rpart(class ~ ., data = train_data, method = "class", control = control_params)
#모델 결과
summary(tree_model)
rpart.plot(tree_model)
predicted_probs <- predict(tree_model, test_data, type = "prob") #모든 확률에 대해 도출
predicted_probs
predicted_classes <- colnames(predicted_probs)[max.col(predicted_probs, ties.method = "first")]
predicted_classes <- factor(predicted_classes, levels = levels(test_data$class))
predicted_classes

library(pROC)
#Confusion Matrix 도출
confusion_matrix <- confusionMatrix(predicted_classes, test_data$class)
print(confusion_matrix)
#ROC Curve
roc_curve <- roc(test_data$class, predicted_probs[, 2])
plot(roc_curve)
auc(roc_curve)
