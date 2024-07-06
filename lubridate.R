library(tidyverse)


#내장형 함수
as.Date('2024-06-27')
as.Date('2024/06/27')


#내장형 함수가 제대로 작동하지 않는 경우 - 위를 제외한 모든 경우
as.Date(20240627)

as.Date('24-06-27')

as.Date('06-27-2024')

as.Date('20240627')

as.Date('2024.06.27')

#lubridate 함수
#년(y) 월(m) 일(d) 순서에 따라 함수를 적으면 됨

ymd('2024-06-27')

dmy('27-06-2024')

mdy('06272024')

ymd('240627')

mdy('06,27,2024')

mdy('June 27th 2024')

mdy('6월 27일 2024')

ymd(240627)


#날짜의 년/월/일/주 추출
dt1 <- ymd(20240627)
dt2 <- ymd(20230514)

year(dt1)

month(dt1)

day(dt1)

week(dt1)

wday(dt1)

wday(dt1, label = TRUE)

yday(dt1)
