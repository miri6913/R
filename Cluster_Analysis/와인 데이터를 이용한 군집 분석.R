#사용할 라이브러리 설치 및 로드
install.packages('NbClust') #k 개수 선택
install.packages('cluster') #클러스터 시각화

library(tidyverse)
library(NbClust)
library(cluster)

wine <- read.csv('wine-clustering.csv')

head(wine)

tail(wine)

str(wine)

summary(wine)

names(wine)

##결측치 체크
colSums(is.na(wine))

##중복 확인
wine %>%
  filter(duplicated(wine)) %>%
  head()

##데이터 포멧 변경
wine_long <- wine %>%
  pivot_longer(cols = names(wine),
               names_to = 'value_groups',
               values_to = 'value')

#히스토그램
ggplot(wine_long, aes(x = value)) +
  geom_histogram(bins = 20) +
  geom_density() +
  facet_wrap(.~value_groups, scales = 'free_y')

#박스플랏으로 이상치 체크
ggplot(wine_long, aes(y = value, fill = value_groups)) +
  geom_boxplot() +
  facet_wrap(.~value_groups, scales = 'free_y')

#스케일링
wine_scaled <- scale(wine)


#k 개수
nb <- NbClust(wine_scaled, distance = 'euclidean', method = 'kmeans',
              min.nc = 2, max.nc = 10)

#씨드설정
set.seed(999)

#클러스터링
wine_km <- kmeans(wine_scaled, centers = 3, nstart = 25)

wine_km$cluster

wine_km$size

wine_km$centers

aggregate(wine, by = list(cluster = wine_km$cluster), mean)

#시각화
clusplot(wine, clus = wine_km$cluster, color = T, shade = T, lables = 2, lines = 0)
