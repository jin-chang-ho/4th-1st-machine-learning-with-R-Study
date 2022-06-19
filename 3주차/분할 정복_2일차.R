# 데이터 가져오기 & 데이터 탐색
mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)
str(mushrooms)
table(mushrooms$type)

# 데이터 전처리
mushrooms$veil_type <- NULL

# 모델 생성 & 훈련
# install.packages("OneR")
library(OneR)

mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R

mushroom_1R_pred <- predict(mushroom_1R, mushrooms)
table(actual = mushrooms$type, predicted = mushroom_1R_pred)

# 모델 성능 개선
# install.packages("RWeka")
library(RWeka)

mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip

mushroom_JRip_pred <- predict(mushroom_JRip, mushrooms)
table(actual = mushrooms$type, predicted = mushroom_JRip_pred)