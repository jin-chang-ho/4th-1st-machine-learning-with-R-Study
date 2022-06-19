# 데이터 불러오기 & 데이터 구조확인
letters <- read.csv('letterdata.csv')
str(letters)

# 데이터 전처리 (자동으로 정규화해주므로 정규화 필요 x)
letters$letter <- factor(letters$letter)
str(letters)

# 훈련 데이터와 테스트 데이터 결정
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]

# 모델 생성 & 훈련
# install.packages('kernlab')
library(kernlab)

letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")
letter_classifier

# 훈련 성능 평가
letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)
table(letter_predictions, letters_test$letter)

agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

# 성능 개선
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)
agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))

cost_values <- c(1, seq(from = 5, to = 40, by = 5))

accuracy_values <- sapply(cost_values, function(x) {
  m <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot", C = x)
  pred <- predict(m, letters_test)
  agree <- ifelse(pred == letters_test$letter, 1, 0)
  accuracy <- sum(agree) / nrow(letters_test)
  return (accuracy)
})
plot(cost_values, accuracy_values, type = "b")