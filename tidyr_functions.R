library(tidyverse)


stu1 <- data.frame(name = c('Jimin', 'Hyunsoo', 'Sangho', 'Yerim'),
                   year1 = c(100, 70, 80, 60),
                   year2 = c(77, 49, 53, 82))

stu2 <- pivot_longer(stu1,
                     cols = c('year1', 'year2'),
                     names_to = 'year',
                     values_to = 'grade')

gather(stu1, key = 'year', value = 'grade', 'year1', 'year2')


spread(stu2, key = year, value = grade)


df1 <- data.frame(id= 1:4,
                  x = c('m-014', 'f-1524', 'f-2534', 'm-65'),
                  bday = c(920134, 941025, 960409, 970228))

df1 %>%
  separate(x, into = c('gender', 'age'), sep = '-')


df1 %>%
  separate_wider_delim(x, names = c('gender', 'age'), delim = '-')


df1 %>%
  separate_wider_position(bday, widths = c(year = 2, month = 2, day = 2))

df1 %>%
  separate_wider_position(bday, widths = c(year = 2, 3, day = 1))

df2 <- data.frame(id=1:4,
                  color = c('red, orange', 'orange, yellow, purple', 'green, blue', 'blue, pink, green'))

df2 %>%
  separate_longer_delim(color, delim = ',') %>%
  group_by(id) %>%
  summarize(cnt = n())




df3 <- data.frame(id = 1:3,
                  year = c(2000, 2001, 2000),
                  month = c(5, 3, 11),
                  day = c(24, 30, 15))

df3 %>%
  unite(date, year, month, day, sep = '')


df4 <- data.frame(
  type = c('apple', 'orange', 'apple', 'orange', 'orange', 'orange'),
  year = c(2010, 2010, 2012, 2010, 2011, 2012),
  size = c('S', 'S', 'S', 'S', 'L', 'L')
)


df4 %>%
  expand(year, type, size)





