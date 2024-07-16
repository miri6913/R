#사용할 라이브러리 로드
library(tidyverse)


#데이터 불러와서 확인
ecom <- read.csv('ecommerce_data.csv')

head(ecom)
tail(ecom)

str(ecom)

#결측치 처리
colSums(is.na(ecom)) / nrow(ecom)

new_ecom <- ecom %>%
  filter(!is.na(CustomerID))

nrow(new_ecom)

colSums(is.na(new_ecom)) / nrow(new_ecom)

#중복 확인
new_ecom %>%
  filter(duplicated(new_ecom)) %>%
  head()

new_ecom %>%
  filter(InvoiceNo == 536409 & StockCode == 21866 & CustomerID == 17908)


new_ecom <- new_ecom %>%
  distinct()

nrow(new_ecom)


#데이터 포맷 수정 및 데이터 추가
new_ecom <- new_ecom %>%
  mutate(InvoiceDate = as.Date(mdy_hm(InvoiceDate)),
         total = Quantity * UnitPrice)

head(new_ecom)

str(new_ecom)

summary(new_ecom)

#이상치 확인 및 EDA
new_ecom %>%
  filter(Quantity == 80995 | Quantity == -80995)

new_ecom %>%
  filter(Quantity <80995 & Quantity > -80995) %>%
  summary()

new_ecom %>%
  filter(Quantity < 74215 & Quantity > -74215) %>%
  summary()


summary(new_ecom)

ggplot(new_ecom, aes(x = Quantity)) +
  geom_histogram() +
  xlim(-10, 130)

new_ecom %>%
  summarize(q90 = quantile(Quantity, 0.9),
            q95 = quantile(Quantity, 0.99),
            q999 = quantile(Quantity, 0.999) )

#500개 이상 주문한 상품 목록
new_ecom %>%
  filter(Quantity > 500) %>%
  group_by(Description) %>%
  summarize(cnt = n()) %>%
  arrange(desc(cnt))

#금액대
new_ecom %>%
  ggplot(aes(x = UnitPrice)) +
  geom_histogram() +
  xlim(0, 100)

new_ecom %>%
  summarize(q90 = quantile(UnitPrice, 0.9),
            q95 = quantile(UnitPrice, 0.99),
            q999 = quantile(UnitPrice, 0.999) )

new_ecom %>%
  filter(UnitPrice > 50) %>%
  group_by(Description) %>%
  summarize(cnt = n()) %>%
  arrange(desc(cnt))
  

nrow(new_ecom %>% filter(UnitPrice > 50)) / nrow(new_ecom)

new_ecom %>%
  filter(Description == 'Manual') %>%
  summary()

new_ecom <- new_ecom %>%
  filter(Description != 'Manual')


#환불의 건수
new_ecom %>%
  filter(str_sub(InvoiceNo, 1, 1) == 'C') %>%
  summarize(cnt = n())

#환불 제의
new_ecom <- new_ecom %>%
  filter(str_sub(InvoiceNo, 1, 1) != 'C')

new_ecom %>%
  filter(UnitPrice == 0) %>%
  summarize(cnt = n())

new_ecom <- new_ecom %>%
  filter(UnitPrice > 0)

#날짜별 주문 확인
new_ecom %>%
  mutate(yearmonth = format(InvoiceDate, '%Y-%m')) %>%
  group_by(yearmonth) %>%
  summarize(cnt = n()) %>%
  ggplot(aes(x = yearmonth, y = cnt)) +
  geom_col()


#국가별 주문 확인
new_ecom %>%
  group_by(Country) %>%
  summarize(cnt = n()) %>%
  mutate(pct = cnt / sum(cnt)) %>%
  arrange(desc(pct))


## RFM 생성
max(new_ecom$InvoiceDate)
today = as.Date('2011-12-09')

#recency
recency <- new_ecom %>%
  group_by(CustomerID) %>%
  summarize(last_order = max(InvoiceDate)) %>%
  mutate(r = today - last_order) %>%
  arrange(CustomerID)

head(recency)

#frequency
frequency <- new_ecom %>%
  distinct(CustomerID, InvoiceNo) %>%
  group_by(CustomerID) %>%
  summarise(f = n()) %>%
  arrange(CustomerID)

head(frequency)

#monetary
monetary <- new_ecom %>%
  group_by(CustomerID) %>%
  summarize(m = sum(total)) %>%
  arrange(CustomerID)

head(monetary)

rfm <- cbind(recency, frequency['f'], monetary['m'])
head(rfm)


#전체 점수, recency는 최근일수록 좋아 -를 붙임
rfm <- rfm %>%
  mutate(r_score = ntile(-r, 3),
         f_score = ntile(f, 3),
         m_score = ntile(m, 3),
         total_score = r_score + f_score + m_score)

ggplot(rfm, aes(x = total_score)) +
  geom_bar()


#rfm 상관관계
rfm %>%
  select(r, f, m) %>%
  mutate(r = as.numeric(r)) %>%
  cor(.)

#fm 스코어
rfm <- rfm %>%
  mutate(fm_score = f_score + m_score)

ggplot(rfm, aes(x = fm_score)) +
  geom_bar()

#각각의 그룹 비교
rfm %>%
  group_by(fm_score) %>%
  summarise(n = n(),
            avg_r = mean(r),
            avg_f = mean(f),
            avg_m = mean(m)) %>%
  mutate(ptg = n / sum(n))

#rfm 수정
rfm1 <- rfm %>%
  mutate(r_score = ntile(-r, 4),
         f_score = ntile(f, 4),
         m_score = ntile(m, 4),
         fm_score = f_score + m_score)

ggplot(rfm1, aes(x = fm_score)) +
  geom_bar()

rfm1 %>%
  group_by(fm_score) %>%
  summarise(n = n(),
            avg_r = mean(r),
            avg_f = mean(f),
            avg_m = mean(m)) %>%
  mutate(ptg = n / sum(n))

rfm1 %>%
  write.csv('fm_score.csv')
