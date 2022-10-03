# 데이터 가져오기
wine <- read.csv("whitewines.csv", stringsAsFactors = TRUE)
str(wine)

# 데이터 탐색
# 종속변수 분포 확인
hist(wine$quality)

# 훈련 & 테스트 데이터 결정
wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

# 회귀 트리 모델 생성 및 확인
# install.packages("rpart")
library(rpart)

m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart

summary(m.rpart)

# 회귀 트리 시각화
# install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

# 모델 성능 평가
p.rpart <- predict(m.rpart, wine_test)

summary(p.rpart)
summary(wine_test$quality)

cor(p.rpart, wine_test$quality)

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(p.rpart, wine_test$quality)
MAE(mean(wine_train$quality), wine_test$quality)

# 모델 성능 개선
# install.packages("Cubist")
library(Cubist)

m.cubist <- cubist(x = wine_train[-12], y = wine_train$quality)
m.cubist

summary(m.cubist)

p.cubist <- predict(m.cubist, wine_test)
summary(p.cubist)

cor(p.cubist, wine_test$quality)

MAE(p.cubist, wine_test$quality)
