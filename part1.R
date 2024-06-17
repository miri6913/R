

csv1 <- read.csv("C:/Users/Jarvis/Documents/Programming/Fast_Campus(데이터_분석)/R/Part 1/emp.csv")
head(csv1)

csv2 <- read.csv("C:/Users/Jarvis/Documents/Programming/Fast_Campus(데이터_분석)/R/Part 1/emp2.csv", header = F)
head(csv2)


names(csv2)
names(csv2) <- c("id", "ename", "dept_no", "level", "join_date", "gender", "base", "bonus")


head(csv2)
names(csv2)[4]
names(csv2)[4] <- "job_level"


emp <- csv1
emp

emp$total <- emp$base + emp$bonus

head(emp)

emp$country <- "Korea"

2+1
2-4