# 데이터 불러오기 & 데이터 구조확인
concrete <- read.csv('concrete.csv')
str(concrete)

# 데이터 정규화
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm)

# 훈련 데이터, 테스트 데이터 생성
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

# 모델 훈련
# install.packages("neuralnet")
library(neuralnet)

concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train)
plot(concrete_model)

# 훈련 성능 평가
model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$strength)

# 성능 개선
concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train, hidden = 5)
plot(concrete_model2)

model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

softplus <- function(x) { log(1 + exp(x)) }
concrete_model3 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train, hidden = c(5, 5), act.fct = softplus, stepmax = 1e+06)
plot(concrete_model3)

model_result3 <- compute(concrete_model3, concrete_test[1:8])
predicted_strength3 <- model_result3$net.result
cor(predicted_strength3, concrete_test$strength)

# 원래 값으로 복구
strengths <- data.frame(
  actual = concrete$strength[774:1030],
  pred = predicted_strength3
)
head(strengths, 3)

cor(predicted_strength3, concrete_test$strength)

unnormalize <- function(x) {
  return ((x * (max(concrete$strength)) - min(concrete$strength)) + min(concrete$strength))
}

strengths$pred_new <- unnormalize(strengths$pred)
strengths$error <- strengths$pred_new - strengths$actual
head(strengths, 3)

cor(predicted_strength3, concrete_test$strength)