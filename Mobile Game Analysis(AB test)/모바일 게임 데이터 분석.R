#사용할 라이브러리 로드
library(tidyverse)

game <- read.csv('cookie_cats.csv')

head(game)

tail(game)

str(game)

summary(game)

#결측치 확인
colSums(is.na(game))

#중복 확인
duplicated(game)

game %>%
  filter(duplicated(game)) %>%
  head()

#이상치 체크
summary(game)

#버전별 유저 확인
game %>%
  group_by(version) %>%
  count()

game %>%
  ggplot(aes(x = version, y = sum_gamerounds)) +
  geom_boxplot()

game %>%
  group_by(version) %>%
  summarize(avg = mean(sum_gamerounds),
            qt90 = quantile(sum_gamerounds, 0.9),
            qt99 = quantile(sum_gamerounds, 0.99),
            qt999 = quantile(sum_gamerounds, 0.999),
            max = max(sum_gamerounds))

#이상치 제거
new_game <- game %>%
  filter(sum_gamerounds < 49854) %>%
  arrange(desc(sum_gamerounds))


new_game %>%
  ggplot(aes(x = version, y = sum_gamerounds)) +
  geom_boxplot()


##EDA
#버전별 플레이수 히스토그램
game %>%
  ggplot(aes(x = sum_gamerounds, fill = version)) +
  geom_histogram(position = 'identity', alpha = 0.4) +
  xlim(0, 100)


#버전별 총 인원 수, 평균 게임 플레이 횟수
new_game %>%
  group_by(version) %>%
  summarize(players = n(),
            avg = mean(sum_gamerounds),
            med = median(sum_gamerounds),
            max = max(sum_gamerounds))

#버전별 설치후 게임 한번도 안한 유저
new_game %>%
  group_by(version, retention_1, retention_7) %>%
  filter(sum_gamerounds == 0) %>%
  summarize(players = n())

#전체 데이터 게임 설치후 게임 한번도 안한 유저 55%
game %>%
  mutate(total_players = n()) %>%
  group_by(retention_1, total_players) %>%
  summarize(players = n(),
            avg = mean(sum_gamerounds)) %>%
  mutate(pct = players / total_players)

#설치 일주일 후 게임하지 않은 플레이어 81%
game %>%
  mutate(total_players = n()) %>%
  group_by(retention_7, total_players) %>%
  summarize(players = n(),
            avg = mean(sum_gamerounds)) %>%
  mutate(pct = players / total_players)

#버전별 확인
game %>%
  group_by(version) %>%
  mutate(total_players = n()) %>%
  group_by(version, retention_1, retention_7, total_players) %>%
  summarize(players = n(),
            avg = mean(sum_gamerounds)) %>%
  mutate(pct = players / total_players)


##AB TEST
gate30 <- new_game %>%
  filter(version == 'gate_30')

gate40 <- new_game %>%
  filter(version == 'gate_40')

#정규성 가정 - 샤피로
shapiro.test(gate30$sum_gamerounds)

#데이터가 너무 많아 qqplot으로 확인
ggplot(gate30, aes(sample = sum_gamerounds)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = 'Gate 30: QQ Plot for Normality Check')


ggplot(gate40, aes(sample = sum_gamerounds)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = 'Gate 40: QQ Plot for Normality Check')

#비모수검정 - 윌콕슨
wilcox.test(gate30$sum_gamerounds, gate40$sum_gamerounds)

#통계적으로 유의미한 차이가 없음...