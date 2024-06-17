install.packages("tidyverse")
library(tidyverse)
tidyverse_packages()

stu1 <- data.frame(name = c('Jimin', 'Hyunsoo', 'Sangho', 'Yerim'),
                   year1 = c(100, 70, 80, 60),
                   year2 = c(77, 49, 53, 82))

stu1_long <- pivot_longer(stu1, cols = c('year1', 'year2'),
             names_to = 'year',
             values_to = 'math')

stu1_long


pivot_wider(stu1_long,
            names_from = year,
            values_from = math)


stu2 <- data.frame(id = rep(1:3, each=4),
                   name = rep(c('Jimin', 'Hyunsoo', 'Sanghoo'), each = 4),
                   year = rep(2020:2021, each = 2),
                   term = rep(1:2, rep = 2),
                   math = c(86, 66, 67, 93, 97, 63, 58, 89, 95, 65, 64, 60),
                   eng = c(79, 84, 92, 73, 82, 89, 90, 75, 83, 74, 95, 71))

pivot_wider(stu2,
            names_from = c('year', 'term'),
            values_from = c('math', 'eng'))

pivot_wider(stu2,
            names_from = 'year',
            values_from = c('math', 'eng'))

pivot_wider(stu2[1:5],
            names_from = c('year', 'term'),
            values_from = 'math')


stu2 %>%
  select(-math) %>%
  pivot_wider(names_from = c('year', 'term'),
              values_from = 'eng')


stu2 %>%
  group_by(id, name, year) %>%
  summarize(avg_math = mean(math)) %>%
  pivot_wider(names_from = 'year', values_from = 'avg_math')
