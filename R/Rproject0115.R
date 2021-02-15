#집단별로 요약하기
exam

library(dplyr)
exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math))
#티블스 형태로 나옴, 5개의 그룹이 생긴걸 볼 수 있다.

#각 반별 최고 평균
exam %>% 
  group_by(class) %>% 
  summarise(max_mean = max(mean))

#실습
subway <- read.csv("1-4호선승하차승객수.csv")
subway

subway %>% 
  group_by(line_no) %>% 
  summarise(total_in. = sum(in.), total_out = sum(out))

subway %>% 
  group_by(line_no) %>% 
  summarise(total_in.out = sum(in., out)) #총합구하는 함수

#시간대 별 가장 승차인원수가 높은 값
subway %>% 
  group_by(time) %>% 
  summarise(max_in. = max(in.))

install.packages("hflights")
library(hflights)
head(hflights)

dim(hflights)
str(hflights) #속성

hflights %>% 
  filter(Month == 1 | Month == 2) %>% 
  nrow()

nrow(hflights) 
ncol(hflights) #컬럼수

hflights %>% 
  arrange(AirTime, Month, Year) %>% 
  head(10) %>% 
  select(AirTime, Month, Year)

hflights %>% 
  group_by(TailNum) %>% 
  arrange(desc(Distance)) %>% 
  summarise(Distance_mean = mean(Distance), ArrDelay_mean = mean(ArrDelay))
#summarise는 아얘 새로운 컬럼을 만드는 
hflights %>% 
  group_by(TailNum) %>% 
  mutate(Distance_mean = mean(Distance), ArrDelay_mean = mean(ArrDelay)) %>% 
  arrange(desc(Distance))

hflights %>% 
  group_by(TailNum) %>% 
  summarise(Distance_mean = mean(Distance), ArrDelay_mean = mean(ArrDelay)) %>% 
  arrange(desc(Distance_mean)) #거리 평균 기준으로 정렬... 뭔가 이상하더라...

list.files()
test1 <- read.csv('test1.csv')
test2 <- read.csv('test2.csv')

total <- full_join(test1, test2, by="id")
total

total <-inner_join(test1, test2, by="id")
total

total <-left_join(test1, test2, by="id")
total

total <-right_join(test1, test2, by="id")
total

teacher <- read.csv('teacher.csv')
teacher

exam <- full_join(exam, teacher, by = 'class')
exam

total

test3 <- read.csv("test3.csv")
test3

total <- bind_rows(total, test3)
total
