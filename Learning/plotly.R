install.packages('plotly')
 

library(plotly)
library(tidyverse)


p <- ggplot(mpg, aes(x=displ, y=hwy, col = drv))+
  geom_point()

plot(p)
ggplotly(p)


#plotly에서 파이프 연산자 사용
plot_ly(mpg, x=~displ, y=~hwy, type='scatter', mode='markers')

#markers의 설정정
plot_ly(mpg, x=~displ, y=~hwy, type='scatter', mode='markers',
        marker=list(size=6, color='green', line=list(color='red', width=2)))

#구동 방식별 색을 정하기
plot_ly(mpg, x=~displ, y=~hwy, type='scatter', mode='markers',color = ~drv)

#점의 모양 바꾸기
plot_ly(mpg, x=~displ, y=~hwy, type='scatter', mode='markers', symbol = ~drv)

#plotly에서 파이프 연산자 사용
plot_ly(mpg, x=~displ, y=~hwy, type='scatter', mode='markers', symbol = ~drv) %>%
  layout(title = 'Interactive scatter plot',
         xaxis = list(title = 'Highway MGP'),
         yaxis = list(title = 'displacement'))


#막대그래프

new_mpg <- mpg %>%
  group_by(manufacturer, drv) %>%
  summarise(cnt = n())

plot_ly(new_mpg, x=~manufacturer, y=~cnt, type='bar') %>%
  layout(title = 'Interactive bargraph',
         xaxis = list(title = 'Manufacturer'),
         yaxis = list(title = 'Count'))

#구동 방식별 막대그래프프
plot_ly(new_mpg, x=~manufacturer, y=~cnt, type='bar', color = ~drv) %>%
  layout(title = 'Interactive bargraph',
         xaxis = list(title = 'Manufacturer'),
         yaxis = list(title = 'Count'),
         barmode = 'stack')

plot_ly(new_mpg, x=~manufacturer, y=~cnt, type='bar', color = ~drv) %>%
  layout(title = 'Interactive bargraph',
         xaxis = list(title = 'Manufacturer'),
         yaxis = list(title = 'Count'),
         barmode = 'group')

#평균 제조사별 고속도로 연비, 도시 연비 구하기
new_mpg_2 <- mpg %>%
  group_by(manufacturer) %>%
  summarise(avg_hwy = mean(hwy),
            avg_cty = mean(cty))


#subplot
#name은 legend 이름
g1 <- plot_ly(new_mpg_2, x = ~avg_hwy, y = ~manufacturer, type = 'bar', name = 'Highway MPG') %>%
  layout(title = 'Highway MPG',
         yaxis = list(categoryorder = 'total ascending'))


g2 <- plot_ly(new_mpg_2, x = ~avg_cty, y = ~manufacturer, type = 'bar', name = 'City MPG') %>%
  layout(title = 'City MPG',
         yaxis = list(categoryorder = 'total ascending'))

subplot(g1, g2, nrows = 1) %>%
  layout(title = 'MPG by manufacturers',
         legend = list(orientation = 'h'))







  