# Sys.setlocale(category = "LC_CTYPE", locale = "C")

# 데이터를 불러오고 데이터의 구조 파악
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)

# sms_raw$type 전처리
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

# sms_raw$text 전처리 첫번째 방법
# install.packages("tm")
library(tm)

# 1. 문자열들을 코퍼스로 바꾸기
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)
# 2. 대문자를 소문자로 치환
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
# 3. 숫자 제거
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) # removeNumbers는 tm 함수
# 4. 불용어 제거
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) # stopwords 대신에 제거하고자 하는 단어 벡터로 치환 가능
# 5. 구두점을 빈칸으로 치환
replacePunctuation <- function(x) {
  gsub("[[:punct:]]+", " ", x)
}
sms_corpus_clean <- tm_map(sms_corpus_clean, content_transformer(replacePunctuation))
# 6. 접미사를 제거하여 어근만 남기기
# install.packages("SnowballC")
library(SnowballC)

sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
# 7. 추가여백 제거
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])
# 8. 메시지를 토큰화해 개별 용어로 나누기
sms_dtm <- DocumentTermMatrix(sms_corpus_clean) # 행은 SMS 메시지, 열은 단어

# sms_raw$text 시각화
# install.packages("wordcloud")
library(wordcloud)

wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

# sms_raw$text 전처리 두번째 방법
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE, # function(x) {removeWords(x, stopwords())}을 사용하면 앞 전처리 결과와 동일
  removePunctuation = TRUE,
  stemming = TRUE
))

# sms_raw$text의 두가지 전처리 방법 비교
sms_dtm
sms_dtm2

# 트레이닝, 테스트 데이터셋, 라벨 생성
sms_dtm_train <- sms_dtm[1:4182,]
sms_dtm_test <- sms_dtm[4183:5574,]
sms_train_labels <- sms_raw[1:4182,]$type
sms_test_labels <- sms_raw[4183:5574,]$type
# 대표성 확인
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# 트레이닝, 테스트 데이터셋에서 최소한 5회 이상 나온 단어만 고르기
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)
sms_dtm_freq_train <- sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[,sms_freq_words]

# 출현 빈도로 측정된 값을 단어가 출현했냐에 따라 yes or no로 바꾸기
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

# 데이터로 모델 훈련
# install.packages("e1071")
library(e1071)

sms_classifier <- naiveBayes(sms_train, sms_train_labels)

# 모델 성능 평가
# install.packages("gmodels")
library(gmodels)

sms_test_pred <- predict(sms_classifier, sms_test)
CrossTable(sms_test_pred, sms_test_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))

# 모델 성능 개선
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual')) # 오히려 성능이 더 안 좋아짐