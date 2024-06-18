library(dplyr)
library(tidyr)

stu1 <- data.frame(name = c('Hwayoung', NA, 'Sua', 'Sojin', 'Minjeong', 'Gaeun', 'Janghoon', 'Sunghoon', 'Jaeyoung', 'Yeseung', 'Jungjae', 'Kyunghwan'),
                   class = c(1, 2, 3, 2, 3, 1, 1, 2, 3, 3, 2, 2),
                   math = c(74, 56, NA, 57, 42, 39, 55, 68, NA, 83, 92, 71),
                   eng = c(76, 70, 77, 68, NA, 63, 70, NA, 95, NA, 100, 65))

anyNA(stu1)

is.na(stu1)

table(stu1)

table(is.na(stu1))

table(is.na(stu1$math))


stu1 %>%
  filter(is.na(math))


stu1 %>%
  filter(is.na(eng))


ed <- stu1 %>%
  filter(!is.na(math))

ed <- stu1 %>%
  filter(!is.na(math) & !is.na(eng))

ed

stu1 %>%
  drop_na(math, eng)

stu1 %>%
  drop_na()

na.omit(stu1)


stu2 <- data.frame(stu1, 
                   gender = c('f', NA, NA, NA, NA, NA, 'm', NA, NA, NA, NA, NA))

stu2

stu2 %>%
  drop_na()


stu2 %>%
  drop_na(math, eng) %>%
  group_by(class) %>%
  summarize(mean(math), mean(eng))

stu2 %>%
  summarize(mean(math))

stu2 %>%
  summarize(mean(math, na.rm = T))


stu2 %>%
  fill(gender, .direction = 'down')

stu2 %>%
  fill(math)

stu2 %>%
  replace_na(list(math = 0, eng = 10))


stu2 %>%
  mutate(math = ifelse(is.na(math), mean(math, na.rm = T), math))

stu2 %>%
  replace_na(list(math = mean(stu2$math, na.rm = T)))








