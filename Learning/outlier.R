library(tidyverse)

hs <- data.frame(id = 1:10,
                 gender = c('f', 'f', 'f', 'f', 'm', 'm', 'm', 'mm', 'mm', 'm'),
                 age = c(17, 18, 18, 17, 17, 18, 18, 19, 19, 16),
                 math = c(65, 70, 50, 60, 1, 60, 90, 70, 110, 0))

hs

table(hs$gender)
table(hs$age)

new_hs <- hs %>%
  mutate(gender = ifelse(gender == 'mm', 'm', gender),
         age = ifelse(age == 16, 17, age),
         math = ifelse(math >= 0 & math <= 100 & math %% 5 == 0, math, NA))

new_hs %>%
  group_by(gender, age) %>%
  summarize(avg = mean(math, na.rm = T))


head(mpg)

?mpg


summary(mpg)

boxplot(mpg$hwy)$stat

mpg %>%
  mutate(hwy = ifelse(hwy < 12 & hwy > 37, NA, hwy)) %>%
  arrange(desc(hwy))
