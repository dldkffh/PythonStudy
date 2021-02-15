#결측치, 이상치 제거하기
#결측치가 존재하면 함수적용이 되지 않아 정제해야 함

list.files()
exam_na <- read.csv("exam_na.csv")
exam_na

is.na(exam_na) #is -> 결과가 boolean값, NA -> TRUE
table(is.na(exam_na))
table(is.na(exam_na$math))
table(is.na(exam_na$english))
table(is.na(exam_na$science))

mean(exam_na$math) #NA값때문에 NA값이 나옴
mean(exam_na$math, na.rm = T) #결측치 제거옵션
#na.rm 옵션이 없으면 필터를 걸어 제거를 한다

library(dplyr)
exam_na %>% 
  filter(!is.na(math))  #!is.na : 비결측치만 보기

#수학시험도 봤고 영어시험도 봤고 과학시험도 본 학생
exam_na %>% 
  filter(!is.na(math) & !is.na(english) & !is.na(science)) #&대신 ,가능
  
#결측치 제거 -> 데이터 손실
#결측치 대체(평균, 중앙값, 최빈값)

exam_na$science <- ifelse(is.na(exam_na$science), mean(exam_na$science, na.rm = T), exam_na$science)
table(is.na(exam_na$science))

exam_na$math <- ifelse(is.na(exam_na$math), mean(exam_na$math, na.rm = T), exam_na$math)
exam_na$english <- ifelse(is.na(exam_na$english), mean(exam_na$english, na.rm = T), exam_na$english)

exam_na %>% 
  group_by(class) %>% 
  summarise(math_mean = mean(math), english_mean = mean(english))

#데이터 관찰하기
delivery <- read.csv("주문 후 배달시간.csv")
delivery

mean(delivery$A, na.rm = T)
mean(delivery$B, na.rm = T)

mean(delivery$B[delivery$B < 600])

median(delivery$A, na.rm = T)
median(delivery$B, na.rm = T)

#사분위수 계산
quantile(delivery$A, na.rm = T) 
quantile(delivery$B, na.rm = T)

boxplot(delivery$A, na.rm = T)
boxplot(delivery$B, na.rm = T)

delivery <- delivery %>% 
  filter(B < 600)
boxplot(delivery$A, delivery$B, names = c('A', 'B'))

#B식당과 C식당중 어디에다 밥을 시킬까?
mean(delivery$B)
mean(delivery$C)

median(delivery$B)
median(delivery$C)

quantile(delivery$B, na.rm = T)
quantile(delivery$C, na.rm = T)

boxplot(delivery)
#이상치 없으니까 똑같은거 같은데... 

#저상태만으로 판단하기 어려움 
#분포정도 확인
hist(delivery$B)
hist(delivery$C)


