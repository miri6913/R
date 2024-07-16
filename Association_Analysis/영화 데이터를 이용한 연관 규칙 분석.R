##사용할 라이브러리 설치 및 로드
install.packages('arules')
install.packages('arulesViz')

library(arules) #연관분석규칙
library(arulesViz) #연관분석규칙시각화
library(tidyverse)

#데이터 불러와서 확인
movies <- read.csv('movies.csv')
ratings <- read.csv('ratings.csv')

head(movies)
tail(movies)

str(movies)


head(ratings)
tail(ratings)

str(ratings)

summary(ratings)

##결측치 확인
colSums(is.na(movies))

colSums(is.na(ratings))


##중복 확인
movies %>%
  filter(duplicated(movies))

movies %>%
  filter(duplicated(title)) %>%
  arrange(title)

movies %>%
  filter(title == 'Aladdin (1992)')

ratings %>%
  filter(duplicated(ratings))


dataset <- ratings %>%
  inner_join(movies, by = 'movieId')

new_dataset <- dataset %>%
  select(-genres, -timestamp) %>%
  group_by(title) %>%
  mutate(movieId = min(movieId),
         rating = mean(rating))

head(new_dataset)

summary(new_dataset)


##EDA 및 이상치 확인
#영화별 펼과된 수
new_dataset %>%
  group_by(movieId) %>%
  summarize(cnt = n()) %>%
  summary()

#유저별 평과 개수
new_dataset %>%
  group_by(userId) %>%
  summarise(cnt = n()) %>%
  summary()

new_dataset %>%
  group_by(userId) %>%
  summarise(cnt = n()) %>%
  ggplot(aes(y = cnt)) +
  geom_boxplot()

new_dataset %>%
  group_by(userId) %>%
  summarise(cnt = n()) %>%
  summarise(avg = mean(cnt),
            qt90 = quantile(cnt, 0.9),
            qt99 = quantile(cnt, 0.99),
            qt999 = quantile(cnt, 0.999),
            max = max(cnt))

#평점이 너무 많은 유저를 outlier로 저장
ol <- new_dataset %>%
  group_by(userId) %>%
  summarise(cnt = n()) %>%
  filter(cnt > 1128)

#데이터셋에서 제외
new_dataset <- new_dataset %>%
  anti_join(ol, by = 'userId')

#낮은 평점 제외
new_dataset <- new_dataset %>%
  select(userId, rating, title) %>%
  filter(rating >= 3.5)

#데이터 포멧 변경
movie_list <- split(new_dataset$title, new_dataset$userId)

transaction <- as(movie_list, 'transactions') 

summary(transaction)


##시각화
itemFrequencyPlot(transaction, topN=10, type = 'absolute')

itemFrequencyPlot(transaction, topN=10, type = 'relative')


##규칙 생성
rules <- apriori(transaction, parameter = list(supp = 0.1, conf = 0.6, maxlen = 2, target = 'rules'))

#상위 6개 신뢰도별
rules_conf <- sort(rules, by = 'confidence', decreasing = T)

inspect(head(rules_conf))



#상위 6개 리프트별
rules_lift <- sort(rules, by = 'lift', decreasing = T)

inspect(head(rules_lift))


#상위 6개 지지도별
rules_supp <- sort(rules, by = 'support', decreasing = T)

inspect(head(rules_supp))

#특정 아이템이 있는 규칙
rule_nemo <- subset(rules, items %in% ('Finding Nemo (2003)'))
inspect(sort(rule_nemo))

