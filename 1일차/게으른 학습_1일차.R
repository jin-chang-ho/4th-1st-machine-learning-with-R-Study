# 데이터를 불러오고 데이터의 구조 파악
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)

# 데이터 전처리 및 데이터 파악하기
wbcd <- wbcd[-1] # id 정보 없애기
table(wbcd$diagnosis) # B는 양성, M은 악성
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant")) # labels는 levels에 이름을 부여
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1) # prop.table : table을 전체 합계가 1이 되게 비율화
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# 수치 데이터 정규화
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

# 트레이닝, 테스트 데이터셋, 라벨 생성
wbcd_train <- wbcd_n[1:469,] # 트레이닝 데이터
wbcd_test <- wbcd_n[470:569,] # 테스트 데이터
wbcd_train_labels <- wbcd[1:469, 1] # 트레이닝 라벨
wbcd_test_labels <- wbcd[470:569, 1] # 테스트 라벨

# 모델 훈련
# install.packages("class")
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21) # knn 알고리즘 적용

# 모델 성능 평가
# install.packages("gmodels")
library(gmodels)

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

# 모델 성능 개선
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)
wbcd_train <- wbcd_z[1:469,] # 트레이닝 데이터
wbcd_test <- wbcd_z[470:569,] # 테스트 데이터
wbcd_train_labels <- wbcd[1:469, 1] # 트레이닝 라벨
wbcd_test_labels <- wbcd[470:569, 1] # 테스트 라벨
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21) # knn 알고리즘 적용
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)