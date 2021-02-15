install.packages("dplyr")
library(dplyr) #전처리
library(ggplot2) #시각화

install.packages("foreign") #spss 로딩
library(foreign)

library(RColorBrewer)
display.brewer.all()
pal <- brewer.pal(30, "Set3")

list.files()
korea <- read.spss("Koweps_hpda12_2017_beta1.sav", to.data.frame = T)
korea

korea_copy <- korea
korea <- korea %>% rename(gender = h12_g3, 
                          birth = h12_g4,
                          marriage = h12_g10,
                          religion = h12_g11,
                          income = h12_pers_income1,
                          code_job = h12_eco9,
                          code_region = h12_reg7)
korea_select <- korea %>% select(gender, birth, marriage, religion, income, code_job, code_region)
korea_select 

table(korea_select$gender)
summary(korea_select)

korea_select$gender <- ifelse(korea_select$gender == 1, 'male', 'female')
table(korea_select$gender) #범주형 데이터 확인은 table로

#수입이 0인 사람을 결측치로 바꿈
korea_select$income <- ifelse(korea_select$income == 0, NA, korea_select$income)

gender_income <- korea_select %>% 
  group_by(gender) %>% 
  summarise(mean_income= mean(income, na.rm = T))

ggplot(data = gender_income, aes(x=gender, y=mean_income)) + geom_col()

#나이와 연봉의 관계
summary(korea_select$birth)
korea_select <- korea_select %>%
  mutate(age = 2019-korea_select$birth)
summary(korea_select)

#나이별(20세 이상) 연봉 평균
age_income <- korea_select %>% 
  filter(korea_select$age > 20) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income, na.rm = T))

ggplot(data = age_income, aes(x=age, y = mean_income)) + geom_line()+ xlim(20, 80)
  
#30세 미만 -> young, 60세 미만 -> middle, 60세 이상 -> old
#age_type 속성 생성
korea_select$age_type <- ifelse(korea_select$age < 30, 'young', 
         ifelse(korea_select$age < 60, 'middle', 'old'))
#korea_select <- korea_select %>% mutate(ifelse(korea_select$age < 30, 'young', ifelse(korea_select$age < 60, 'middle', 'old'))) 
table(korea_select$age_type)

#연령대 별 성별 평균연봉
age_type_income <- korea_select %>% 
  group_by(age_type, gender) %>% 
  summarise(mean_income = mean(income, na.rm = T))

ggplot(data = age_type_income, aes(x=age_type, y=mean_income)) +
  geom_col(aes(fill=gender), position = "dodge") +
  scale_x_discrete(limits = c('young', 'middle', 'old'))


age_gender <- korea_select %>% 
  filter(age > 20) %>% 
  group_by(age, gender) %>% 
  summarise(mean_income = mean(income, na.rm = T))

ggplot(data = age_gender, aes(x=age, y=mean_income, col=gender)) + geom_line() + xlim(20, 80)

#돈 제일 많이 받는 직업
table(korea_select$code_job)

list.files()
code_job <- read.csv("code_job.csv")
code_job <- code_job %>% rename(code_job = code)
code_job

korea_select <- left_join(korea_select, code_job, id='code_job')
table(korea_select$job)

korea_select %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income, na.rm = T)) %>% 
  arrange(desc(mean_income)) %>% 
  head(10)




### 상관관계 찾기 ###
#피어슨 상관계수
#상관행렬 히트맵

install.packages('googleVis')
library(googleVis)

mtcars
cor(mtcars) #상관관계
#이걸 시각화 하면 상관행렬 히트맵

install.packages('corrplot') #히트맵 그리는 패키지
library(corrplot)

corrplot(cor(mtcars),
         type = 'lower', #반만 보기
         order = 'hclust', #군집화, 정렬
         method = 'color', # 동그라미 -> 모자이크, 네모모양
         tl.col = 'black', #변수명 색깔
         addCoef.col = 'black') #상관계수 표현

