library(tidyverse)

ans_tidy <- anscombe %>%
  mutate(obs = row_number()) %>%
  pivot_longer(cols = -obs, names_to = 'key', values_to = 'value') %>%
  separate_wider_position(cols = key, widths = c(variable = 1, set =1)) %>%
  pivot_wider(names_from = variable, values_from = value)


ans_tidy %>%
  group_by(set) %>%
  summarize(mean(x), mean(y), var(x), var(y), sd(x), sd(y), cor(x, y))


ggplot(ans_tidy, aes(x, y)) +
  geom_point() +
  facet_wrap( ~ set) +
  geom_smooth(method = 'lm', se = F)


#ggplot - graphic grammar plot

#자리수가 많은 숫자도 그대로 보여 줌
options(scipen = 999)

head(midwest)

ggplot(midwest, aes(x=area, y=poptotal))+
  geom_point()+
  geom_smooth(method = 'lm')

#이상치 제거해서 보여 줌
ggplot(midwest, aes(x=area, y=poptotal))+
  geom_point()+
  geom_smooth(method = 'lm')+
  xlim(c(0, 0.1))+
  ylim(c(0, 1000000))

#보는 영역만 다름, 확대해서 보여 줌
ggplot(midwest, aes(x=area, y=poptotal))+
  geom_point()+
  geom_smooth(method = 'lm')+
  coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 1000000))+
  labs(title = 'area vs population',
       subtitle = 'from midwest dataset',
       y = 'population',
       x = 'area')


#색 추가
ggplot(midwest, aes(x=area, y=poptotal))+
  geom_point(col = 'green', size = 4)+
  geom_smooth(method = 'lm', col = 'firebrick', size = 2)+
  coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 1000000))+
  labs(title = 'area vs population',
       subtitle = 'from midwest dataset',
       y = 'population',
       x = 'area')

# aes - aesthetic mappings
ggplot(midwest, aes(x=area, y=poptotal))+
  geom_point(aes(colour = state))+
  geom_smooth(method = 'lm', col = 'firebrick', size = 2)+
  coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 1000000))+
  labs(title = 'area vs population',
       subtitle = 'from midwest dataset',
       y = 'population',
       x = 'area')

ggplot(midwest, aes(x=area, y=poptotal))+
  geom_point(aes(colour = state))+
  geom_smooth(method = 'lm', col = 'firebrick', size = 2)+
  coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 1000000))+
  labs(title = 'area vs population',
       subtitle = 'from midwest dataset',
       y = 'population',
       x = 'area')+
  theme(legend.position = 'None')+
  scale_color_brewer(palette = 'Set1')

# x축의 단위 바꾸기
ggplot(midwest, aes(x=area, y=poptotal))+
  geom_point(aes(colour = state))+
  geom_smooth(method = 'lm', col = 'firebrick', size = 2)+
  coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 1000000))+
  labs(title = 'area vs population',
       subtitle = 'from midwest dataset',
       y = 'population',
       x = 'area')+
  theme(legend.position = 'None')+
  scale_color_brewer(palette = 'Set1')+
  scale_x_continuous(breaks = seq(0, 0.1, 0.01))

#histogram
head(mpg)

ggplot(mpg, aes(x=hwy))+
  geom_histogram()+
  labs(title = 'Histogram of Highway MPG',
       x = 'Highway MPG')



ggplot(mpg, aes(x=hwy))+
  geom_histogram(bins = 16,
                 col = 'black',
                 fill = 'blue')+
  labs(title = 'Histogram of Highway MPG',
       x = 'Highway MPG')

#폭의 넓이 설정
ggplot(mpg, aes(x=hwy))+
  geom_histogram(binwidth = 5,
                 col = 'black',
                 fill = 'blue')+
  labs(title = 'Histogram of Highway MPG',
       x = 'Highway MPG')


#자동차 구동 방식별 히스토그램 확인
ggplot(mpg, aes(x=hwy, fill=factor(drv)))+
  geom_histogram(binwidth = 5,
                 position = 'identity',
                 alpha = 0.5)+
  labs(title = 'Histogram of Highway MPG',
       x = 'Highway MPG',
       fill = 'drive train')+
  scale_fill_manual(values = c('4' = 'blue', 'f' = 'green', 'r' = 'red'),
                    breaks = c('4', 'f', 'r'),
                    labels = c('front-wheel drive', 'rear-wheel drive', '4wd'))



#barchart
ggplot(mpg, aes(x = manufacturer))+
  geom_bar(color = 'black', fill = 'green') +
  labs(x='Manufacturer',
       title = 'Barchart of Manufacturer')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#x축과 y축을 바꿔서 그리기
ggplot(mpg, aes(x = manufacturer))+
  geom_bar(color = 'black', fill = 'green') +
  labs(x='Manufacturer',
       title = 'Barchart of Manufacturer')+
  coord_flip()

#제조사별 구동방식에 따라 막대그래프
ggplot(mpg, aes(x = manufacturer, fill = drv))+
  geom_bar(position = 'stack') +
  labs(x='Manufacturer',
       title = 'Barchart of Manufacturer')+
  coord_flip()

ggplot(mpg, aes(x = manufacturer, fill = drv))+
  geom_bar(position = 'fill') +
  labs(x='Manufacturer',
       title = 'Barchart of Manufacturer')+
  coord_flip()


#자동차 종류별 평균 고속도로 연비 막대그래프
head(mpg)

class_hwy <- mpg %>%
  group_by(class) %>%
  summarize(avg_hwy = mean(hwy))

class_hwy

ggplot(class_hwy, aes(x=class, y=avg_hwy))+
  geom_col()+
  labs(title = 'Average Highway MPG by Type of Car',
       x = 'Type of Car',
       y = 'Average Highway MPG')
  
#연비가 높은 순으로 x축 정렬
ggplot(class_hwy, aes(x=reorder(class, -avg_hwy), y=avg_hwy))+
  geom_col()+
  labs(title = 'Average Highway MPG by Type of Car',
       x = 'Type of Car',
       y = 'Average Highway MPG')


mpg_long <-mpg %>%
  group_by(class) %>%
  summarize(avg_hwy = mean(hwy), 
            avg_cty = mean(cty)) %>%
  pivot_longer(cols = -class, names_to = 'avg')


ggplot(mpg_long, aes(x=class, y=value,fill=avg))+
  geom_col(position = 'dodge')



#선그래프
head(economics)
head(economics_long)


summary(economics)


ggplot(economics, aes(x=date, y=unemploy))+
  geom_line()

#년도별 실업자 수
ggplot(economics, aes(x=date, y=unemploy))+
  geom_line(color = 'red') +
  labs(title = 'Number of unemployment over time',
       x = 'Date',
       y = 'Unemployment in thousand') +
  geom_smooth(method = 'lm', se = F) +
  theme(panel.grid.major.x = element_line(color = 'black'),
        panel.grid.minor.y = element_blank())


#모든 파라미터를 선그래프로 표시
ggplot(economics_long, aes(x=date, y=value01, color = variable))+
  geom_line()+
  #배경을 하얀색으로 표현
  theme_bw()


#상자그래프
summary(mpg)

ggplot(mpg, aes(x = drv, y=hwy, fill = drv))+
  geom_boxplot() +
  labs(x = 'Type of drive train',
       y = 'Highway MPG',
       title = 'Box Plot of Highway MPG by Type of Drive Train')
 
#값의 빈도수 확인
table(mpg$drv)

ggplot(mpg, aes(x = drv, y=hwy, fill = drv))+
  geom_boxplot(varwidth = T, notch = T) +
  labs(x = 'Type of drive train',
       y = 'Highway MPG',
       title = 'Box Plot of Highway MPG by Type of Drive Train')

#이상치 다르게 표시
ggplot(mpg, aes(x = drv, y=hwy, fill = drv))+
  geom_boxplot(varwidth = T, notch = T, outlier.color = 'red', outlier.shape = 'square', 
               outlier.size = 3) +
  labs(x = 'Type of drive train',
       y = 'Highway MPG',
       title = 'Box Plot of Highway MPG by Type of Drive Train')

#개별 데이트 박스플랏 위에 표시
ggplot(mpg, aes(x = drv, y=hwy, fill = drv))+
  geom_boxplot(varwidth = T, notch = T, outlier.color = 'red', outlier.shape = 'square', 
               outlier.size = 3) +
  labs(x = 'Type of drive train',
       y = 'Highway MPG',
       title = 'Box Plot of Highway MPG by Type of Drive Train') +
  geom_jitter()



#그래프 분할
ggplot(mpg, aes(x=displ, y=hwy, color=class))+
  geom_point()

#글래스별 다른 그래프 그리기
ggplot(mpg, aes(x=displ, y=hwy))+
  geom_point()+
  facet_wrap(~class)

#행에 수를 지정
ggplot(mpg, aes(x=displ, y=hwy))+
  geom_point()+
  facet_wrap(~class, nrow = 2)

#그래프 제목의 위치 변경
ggplot(mpg, aes(x=displ, y=hwy))+
  geom_point()+
  facet_wrap(~class, nrow = 2, strip.position = 'left')

#개별 x축 이용
ggplot(mpg, aes(x=displ, y=hwy))+
  geom_point()+
  facet_wrap(~class, nrow = 2, strip.position = 'left', scale = 'free_x')

ggplot(mpg, aes(x=displ, y=hwy))+
  geom_point()+
  facet_grid(. ~ class)


#박스플랏
plot_ly(mpg, x=~drv, y=~hwy, type='box')

plot_ly(mpg, x=~drv, y=~hwy, type='box', color=~class, boxmean = T, boxpoints = 'all', 
        jitter=0.8) %>%
  layout(boxmode = 'group')


#선그래프
head(economics)

plot_ly(economics, x=~date, y=~unemploy, type='scatter', mode='lines')


#정규화된 long format으로 wide format으로 만들기
new_econ <- economics_long %>%
  select(-value) %>%
  pivot_wider(names_from = variable,
              values_from = value01)



plot_ly(new_econ, x=~date, y=~unemploy, type='scatter', mode='lines')

#여러 축 추가
plot_ly(new_econ, x=~date, y=~unemploy, type='scatter', mode='lines',
        name='unemploy', line=list(color='black', width=7)) %>%
  add_trace(y=~pce, mode='lines',
            name='pce', line=list(color='red', dash='dash')) %>%
  add_trace(y=~psavert, mode='lines',
            name='psavert', line=list(color='blue', dash='dot'))


plot_ly(new_econ, x=~date) %>%
  add_trace(y=~unemploy, type='scatter', mode='lines',
        name='unemploy', line=list(color='black', width=7)) %>%
  add_trace(y=~pce, mode='lines',
            name='pce', line=list(color='red', dash='dash')) %>%
  add_trace(y=~psavert, mode='lines',
            name='psavert', line=list(color='blue', dash='dot'))


#range_slider 추가
plot_ly(new_econ, x=~date) %>%
  add_trace(y=~unemploy, type='scatter', mode='lines',
            name='unemploy', line=list(color='black', width=7)) %>%
  add_trace(y=~pce, mode='lines',
            name='pce', line=list(color='red', dash='dash')) %>%
  add_trace(y=~psavert, mode='lines',
            name='psavert', line=list(color='blue', dash='dot')) %>%
  layout(title='Economics in the US',
         xaxis=list(rangeslider=list(type='date')),
         yaxis=list(title='normalized values'))


