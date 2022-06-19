# 데이터 가져오기 & 데이터 탐색
credit <- read.csv("credit.csv", stringsAsFactors = TRUE)
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)

# 데이터 전처리
credit <- credit[1:17]
credit$default <- factor(credit$default)

# train data와 test data 결정
set.seed(124)

train_sample <- sample(1000, 900)
str(train_sample)

credit_train <- credit[train_sample, ] 
credit_test <- credit[-train_sample, ]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# 모델 생성 & 훈련
# install.packages("C50")
library(C50)

credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model
summary(credit_model)

# 모델 성능 평가
library(gmodels)

credit_pred <- predict(credit_model, credit_test)
CrossTable(credit_test$default, credit_pred, prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('actual default','predicted default'))

# 모델 성능 개선
# 부스팅 사용
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)
credit_boost10
summary(credit_boost10)

credit_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_pred10, prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('actual default','predicted default'))

# 현재 모델은 Yes임에도 No로 분류하는 경우가 많고 해당 오류는 다른 오류보다 비싸므로 해당 오류를 줄이는 방법
matrix_dimensions <- list(c("1", "2"), c("1", "2"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

credit_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred, prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('actual default','predicted default'))