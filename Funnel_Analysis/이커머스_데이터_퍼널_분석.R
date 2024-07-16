library(tidyverse)

#데이터 불러와서 확인
mql <- read.csv('olist_marketing_qualified_leads_dataset.csv')
cd <- read.csv('olist_closed_deals_dataset.csv')

#판매자로 가입한 고객 데이터 확인
head(mql)

tail(mql)

str(mql)

summary(mql)

#데이터타입 변경
mql <- mql %>%
  mutate(first_contact_date = ymd(first_contact_date))

summary(mql)

#결측치 확인
colSums(is.na(mql))

#중복데이터 확인
mql %>%
  filter(duplicated(mql))

#이상치 확인 및 EDA
mql %>%
  group_by(origin) %>%
  summarise(cnt = n(),
            pct = cnt / 8000) %>%
  arrange(desc(cnt))


#결측치 제거
mql <- mql %>%
  filter(origin != "")

#판매자로 전환된 고객 데이터 확인
head(cd)
tail(cd)
summary(cd)


#데이터 타입 변경
cd <- cd %>%
  mutate(won_date = as.Date(ymd_hms(won_date)))

#리드타입별
cd %>%
  group_by(lead_type) %>%
  summarise(cnt = n()) %>%
  arrange(desc(cnt)) %>%
  ggplot(aes(x = lead_type, y = cnt)) +
  geom_col()


#비즈니스 타입별
# ""가 들어가 있음
cd %>%
  group_by(business_type) %>%
  summarise(cnt = n()) %>%
  arrange(desc(cnt))

#비즈니스 세그맨트별
cd %>%
  group_by(business_segment) %>%
  summarise(cnt = n()) %>%
  mutate(pct = cnt / sum(cnt)) %>%
  arrange(desc(cnt))

#비어 있는 데이터 확인 및 삭제
cd %>%
  filter(business_segment == '' | business_type == '' | lead_type == '')


cd <- cd %>%
  filter(business_segment != '' | business_type != '' | lead_type != '')

#데이터 병합, left join을 사용하여 판매자로 전환되지 않는 사용자도 같이 표시
mql_cd <- mql %>%
  left_join(cd, by = 'mql_id')

head(mql_cd)


#유입별 판매자 전환율
mql_cd %>%
  group_by(origin) %>%
  summarize(mqls = n(),
            sellers = sum(ifelse(is.na(seller_id), 0, 1)),
            conversion = sellers/mqls) %>%
  arrange(desc(conversion))


#랜딩 페이지별 전환율
mql_cd %>%
  group_by(landing_page_id) %>%
  summarize(sellers = sum(ifelse(is.na(seller_id), 0, 1))) %>%
  mutate(conversion = sellers/sum(sellers)) %>%
  arrange(desc(conversion))

#영업담당자별 전환시키닌데 걸리는 시간
sr_info <- mql_cd %>%
  filter(!is.na(seller_id)) %>%
  mutate(duration = won_date - first_contact_date) %>%
  group_by(sr_id) %>%
  mutate(avg_duration = mean(duration, na.rm = T),
         sellers = n()) %>%
  distinct(sr_id, avg_duration, sellers) %>%
  arrange(avg_duration)

sr_info

sr_info %>%
  mutate(avg_duration = as.integer(avg_duration)) %>%
  summary()
