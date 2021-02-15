name <- c('김지훈', '이유진', '박동현', '김민지')
english <- c(90, 80, 60, 70)
math <- c(50, 60, 100, 20)
score <- data.frame(name, english, math)
score

mean(score$math)
mean(score$english)

score$mean <- (score$math + score$english) / 2
score

max(score$mean)
summary(score$mean)
hist(score$mean) #히스토그램

score$pass <- ifelse(score$mean >= mean(score$mean), "pass", "fail")
score

table(score$pass) #데이터빈도수 계산

install.packages("ggplot2")
library(ggplot2) 

qplot(score$pass) #데이터 빈도수 시각화

score$grade <- ifelse(score$mean>= 80, "A", 
                      ifelse(score$mean>= 60, "B", "C"))
table(score$grade)
qplot(score$grade)

midwest #tibble형태
df_midwest <- as.data.frame(midwest)
df_midwest

head(df_midwest)
tail(df_midwest)
table(df_midwest$county)

df_midwest$perasian <- df_midwest$popasian / df_midwest$poptotal * 100
hist(df_midwest$perasian)

delete.response(df_midwest$asian_per)
head(df_midwest)

exam <- read.csv("csv_exam.csv")
exam

table(exam$class)
install.packages("dplyr")
library(dplyr)

exam %>% filter(class == 1) #ctrl + shift + m, 파이프, 함수 적용할때, 조건에 맞는 자료 추출
exam %>% filter(class != 3)

#수학 점수가 50점 이상인 학생만 출력
exam %>% filter(class == 1 & math >= 50) 
exam %>% 
  filter(class == 1) %>% 
  filter(math >= 50) 

#수학점수가 40점이상 50점미만인 학생
exam %>% filter(math < 50 & math >= 40)

#수학점수나 영어점수가 90점 이상인 학생
exam %>% filter(math >= 90 | english >= 90)

#1반 3반 5반 학생
exam %>% filter(class == 1 | class == 3 | class == 5)
exam %>% filter(class %in% c(1, 3, 5))
exam %>% filter(class %in% 3:5)

#cloumn명 바꾸기
mpg
df_mpg <- as.data.frame(mpg)
df_mpg

df_mpg <- df_mpg %>% rename(highway = hwy)

displ_4 <- df_mpg %>% filter(displ <= 4)
displ_5 <- df_mpg %>% filter(displ >= 5)
mean(displ_4$cty) >= mean(displ_5$cty)

mf_audi <- df_mpg %>% filter(manufacturer == 'audi')
mf_toyota <- df_mpg %>% filter(manufacturer == 'toyota')
mean(mf_audi$cty) >= mean(mf_toyota$cty)

mpg_cfh <- df_mpg %>% filter(manufacturer == 'chevrolet' 
                      | manufacturer == 'ford' 
                      | manufacturer == 'honda')
mpg_cfh <- df_mpg %>% filter(manufacturer %in% c('chevrolet', 'ford', 'honda'))
mean(mpg_cfh$highway)

exam %>% select(class, english) #dataframe형태로 추출
exam %>% select(-math)

#1반 학생들의 class, math 
exam %>% filter(class == 1) %>% select(class, math)

exam %>% arrange(math) #정렬함수
exam %>% arrange(desc(math)) #내림차순
exam %>% arrange(class, desc(math))
                 
exam$mean <- (exam$math + exam$english + exam$science)/3
head(exam)
for (i in 1:5) {
  print(exam %>% filter(class == i) %>% arrange(desc(mean)) %>% head(1)) }

#전체 학생 평균을 구해 평균 이상이면 pass, fail 값을 부여하는 변수 추가
#pass/fail 각각 명몇인지 막대 그래프
exam$pf <- ifelse(exam$mean >= mean(exam$mean), "pass", "false")
exam %>% mutate(pf = ifelse(exam$mean >= mean(exam$mean), "pass", "false")) 
#mutate 파생변수 생성, 변수 추가
qplot(exam$pf)
table(exam$pf)
