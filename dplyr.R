install.packages("dplyr")
library(dplyr)

install.packages("installr")

version


csv1 <- read.csv("C:/Users/Jarvis/Documents/Programming/Fast_Campus(데이터_분석)/R/Part 1/emp.csv")
head(csv1)

emp <- csv1

head(emp)

emp

emp %>% filter(dept_no == 10)
emp %>% filter(job_level == 3)
emp %>% filter(job_level != 5)

emp %>% filter(join_date < "2022-01-01")
emp %>% filter(base > 5000)
emp %>% filter(job_level >= 4)
emp %>% filter(ename <= "d")
emp %>% filter(ename <= "e")

emp %>% filter(dept_no == 10 & gender == "M")
emp %>% filter(dept_no == 10 & gender == "M" & bonus >= 400)
emp %>% filter(base >= 5000 | bonus >= 500)
emp %>% filter(base >= 5000 | bonus >= 500 | dept_no == 40)

emp %>% filter(dept_no == 20 | dept_no == 30)
emp %>% filter(dept_no %in% c(20, 30))

emp_jr <- emp %>% filter(job_level %in% c(1, 2))
emp_sr <- emp %>% filter(job_level %in% c(3, 4, 5))

emp_jr
emp_sr

mean(emp_jr$base)
mean(emp_sr$base)


emp %>% select(ename)
emp %>% select(gender)
emp %>% select(ename, gender)

emp %>% select(-base)
emp %>% select(-bonus)
emp %>% select(-base, -bonus)

emp %>% filter(dept_no == 30) %>% select(ename)

emp %>% 
  filter(dept_no == 30) %>% 
  select(ename)


emp %>%
  filter(dept_no == 10) %>%
  head(2)


emp %>%
  arrange(id)

emp %>%
  arrange(base)

emp %>%
  arrange(desc(base))

emp %>%
  arrange(dept_no, join_date)

emp %>%
  arrange(join_date, dept_no)

emp %>%
  arrange(desc(job_level), desc(base), join_date)


emp %>%
  filter(dept_no %in% c(10, 40)) %>%
  select(id, ename) %>%
  arrange(desc(id))


emp %>%
  filter((dept_no == 10 & job_level <= 2) | (dept_no %in% c(20, 30) & job_level >= 4)) %>%
  select(ename, dept_no, job_level, join_date) %>%
  arrange(desc(job_level), join_date)

emp %>%
  mutate(total = base + bonus) %>%
  head

emp %>%
  mutate(special_bonus = base * 0.1) %>%
  head

head(emp)

emp$total <- emp$base + emp$bonus

head(emp)

emp$special_bonus <- emp$base * 0.1

head(emp)

emp$total <- NULL
head(emp)


emp$special_bonus <- NA

head(emp)

emp$special_bonus <- NULL
head(emp)

emp <- emp %>%
  select(-special_bonus)

emp <- emp %>%
  mutate(total = base + bonus,
         special_bonus = total * 0.1)

head(emp)

emp %>%
  mutate(is_new = ifelse(join_date > "2022-03-01", "Y", "N")) %>%
  arrange(desc(join_date)) 

emp %>%
  mutate(is_new = ifelse(join_date > "2022-03-01", "Y", "N")) %>%
  arrange(desc(is_new)) 

emp %>%
  mutate(is_new = ifelse(join_date > "2022-03-01", "Y", "N")) %>%
  select(ename, is_new)  


head(emp)

emp$total <- NULL

emp %>%
  mutate(job_level_name = ifelse(job_level <= 2, "junior", "senior"),
         signing_bonus = ifelse(join_date >= "2023-01-01" & join_date <= "2023-12-21", 200, 0),
         comission = ifelse(dept_no == 10 & job_level_name == "senior", 400, 0),
         total = base + bonus + signing_bonus + comission) %>%
  filter(total >6000) %>%
  select(ename, total) %>%
  arrange(desc(total))


emp %>%
  summarise(avg_base = mean(base))

emp %>%
  group_by(dept_no) %>%
  summarise(avg_base = mean(base))

emp %>%
  group_by(dept_no) %>%
  summarise(avg_base = mean(base),
            med_base = median(base),
            sum_bonus = sum(bonus),
            no_ppl = n()) %>%
  arrange(avg_base)


emp %>%
  group_by(dept_no, gender, job_level) %>%
  summarize(avg_base = mean(base))


emp %>%
  mutate(job_level_name = ifelse(job_level <= 2, "junior", "senior")) %>%
  group_by(dept_no, job_level_name, gender) %>%
  summarize(avg_base = mean(base))


emp %>%
  filter(dept_no != 30) %>%
  group_by(dept_no) %>%
  summarize(avg_salary = mean(base + bonus)) %>%
  arrange(desc(avg_salary)) %>%
  select(dept_no) %>%
  head(1)