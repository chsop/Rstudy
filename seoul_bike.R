library(tidyverse)
library(lubridate)
library(ggplot2)

########################### 따릉이 데이터 분석 ###########################

train <- read.csv("bike.csv")
head(train)
str(train)

# datetime 컬럼을 lubridate 라이브러리의 ymd_hms형으로 변형
train$datetime <-ymd_hms(train$datetime)

# datetime 컬럼에서 year, month, day, weekday, hour 추출
train$year <- year(train$datetime)
train$month <- month(train$datetime)
train$day <- day(train$datetime)
train$weekday <- weekdays(train$datetime)
train$hour <- hour(train$datetime)

# 범주형 데이터 factor 형태로 변형해주기
factor_vars = c('season', 'weather', 'holiday', 'workingday',
                'year', 'month', 'day', 'weekday', 'hour')
train[factor_vars] <- lapply(train[factor_vars], as.factor)
str(train)


# 가설 검정 : workingday 여부, 시간, 온도에 따라 자전거 수요가 다를까?

# 1. 시각적 접근
# workingday마다 시간에 따른 자전거 수요 시각화하기
# (온도에 대해서 색으로 구분)
train %>% 
  filter(workingday == 0) %>% 
  ggplot()+
  aes(x=hour, y=count, color=temp)+
  geom_point()+
  labs(title="Bike count on non-working days")+
  #ggtitle("Bike count on non-working days")로 해줘도됨
  scale_color_gradient(low="#88d8b0", high="#ff6f69")

train %>% 
  filter(workingday == 1) %>% 
  ggplot()+
  aes(x=hour, y=count, color=temp)+
  geom_point()+
  labs(title="Bike count on working days")+
  scale_color_gradient(low="#88d8b0", high="#ff6f69")

# 두 그래프 같이 보고싶을때
# 각 그래프에 workingday:0, workingday:1로 라벨 붙이기
train %>% 
  ggplot()+
  aes(x=hour, y=count, color=temp)+
  geom_point()+
  facet_grid(~workingday, labeller=label_both)

# 각 그래프에 임의로 지정한 제목 붙이기
train_names <- c('0' = "working day", '1' = "non-working day")
train %>% 
  ggplot()+
  aes(x=hour, y=count, color=temp)+
  geom_point()+
  facet_grid(~workingday, labeller=as_labeller(train_names))

# position_jitter을 사용하면 산점도의 점을 퍼뜨려 표시할 수 있음
train %>%
  filter(workingday == 1) %>%
  ggplot(aes(hour, count, color = temp)) +
  geom_point(position = position_jitter()) +
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") +
  ggtitle("Bike Count on Working Days")


# 2. 통계적 접근

# workingday에 대한 가설 통계적으로 확인
# workingday는 1과 0으로 되어있는 이항분포 데이터이므로 T검정 진행
# 참고 : 출력값을 지수형태로 보고싶지 않을 때는 options(scipen = 3) 설정

# 등분산 검정
# 귀무가설 : 등분산임 / 대립가설 : 등분산이 아님
var.test(count~workingday,data=train)
# p-value < 0.05이므로 귀무가설 기각 : 등분산이 아님

# t검정
# 귀무가설 : 그룹간 평균에 통계적으로 유의미한 차이가 없다
# 대립가설 : 그룹간 평균에 통계적으로 유의미한 차이가 있다
t.test(count~workingday, data=train, var.equal = F)
# 등분산을 만족하지 않는 선에서 workingday = 0일때 평균은 약 188, 1일때 평균은 193
# p-value > 0.05이므로 귀무가설 채택
# workingday일때와 아닐때 그룹간 평균에 통계적으로 유의한 차이 없다


# holiday에 대한 가설 통계적으로 확인
var.test(count~holiday,data=train)
# p-value > 0.05이므로 귀무가설 채택 : 등분산임

t.test(count~holiday,data=train)
# p-value > 0.05이므로 귀무가설 채택
# holiday일때와 아닐때 그룹간 평균에 통계적으로 유의한 차이 없다
