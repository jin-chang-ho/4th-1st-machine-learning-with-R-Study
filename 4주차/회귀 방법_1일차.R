# 데이터 가져오기
insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)

# 데이터 탐색
# 종속변수 정규성 확인
summary(insurance$charges)
hist(insurance$charges)

# factor 확인
table(insurance$region)

# 상관 행렬로 관계 파악
cor(insurance[c("age", "bmi", "children", "charges")])

# 산포도 행렬로 관계 시각화
pairs(insurance[c("age", "bmi", "children", "charges")])

# 계산된 산포도 행렬
# install.packages("psych")
library(psych)

pairs.panels(insurance[c("age", "bmi", "children", "charges")])

# 선형 회귀 모델 생성
ins_model <- lm(charges ~ age + children + bmi + sex + smoker + region, data = insurance)
ins_model

# 모델 성능 평가
summary(ins_model)

# 모델 성능 개선
insurance$age2 <- insurance$age^2
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)
summary(ins_model2)

insurance$pred <- predict(ins_model2, insurance)
cor(insurance$pred, insurance$charges)
plot(insurance$pred, insurance$charges)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)

# 예측
predict(ins_model2, data.frame(age = 30, age2 = 30^2, children = 2, bmi = 30, sex = "male", bmi30 = 1, smoker = "no", region = "northeast"))
predict(ins_model2, data.frame(age = 30, age2 = 30^2, children = 2, bmi = 30, sex = "female", bmi30 = 1, smoker = "no", region = "northeast"))
predict(ins_model2, data.frame(age = 30, age2 = 30^2, children = 0, bmi = 30, sex = "female", bmi30 = 1, smoker = "no", region = "northeast"))