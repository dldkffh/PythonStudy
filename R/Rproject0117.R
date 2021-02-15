library(ggplot2)
library(dplyr)
#배경설정(축) -> 그래프추가(점,선,막대) -> 설정추가(축범위,색,표식)

df_mpg
ggplot(data = df_mpg, aes(x=displ, y=highway)) + #배경
  geom_point() + #그래프
  xlim(1,6.5) + ylim(10,40)  #축범위

df_mpg
ggplot(data = df_mpg, aes(x=cty, y=highway)) +
  geom_point()
  
midwest
ggplot(data = df_midwest, aes(x=poptotal, y=popasian)) +
  geom_point() +
  xlim(0,500000) + ylim(0,10000)

df_meanhwy <- df_mpg %>% group_by(manufacturer) %>% summarise(mean_hwy = mean(highway))
ggplot((data = df_meanhwy), aes(x=reorder(manufacturer, -mean_hwy), y=mean_hwy)) +
  geom_col() 

ggplot(data = df_mpg, aes(x=drv)) + 
  geom_bar(fill = "RoyalBlue3")

blood <- read.csv("혈액형현황.csv")
blood

ggplot(data = blood, aes(x=blood)) + 
  geom_bar(fill = c("#bb2222", "#bb2233", "#aa1122", "#aa3355"))

ggplot(data = blood, aes(x=blood)) + 
  geom_bar(aes(fill = gender), position = "dodge") +
  scale_fill_manual(values = c("Black", "RoyalBlue4"))

library(RColorBrewer)
display.brewer.all()
pal <- brewer.pal(12, "Set3")

ggplot(data = blood, aes(x=class)) + #반별 학생수
  geom_bar(aes(fill = blood), position = "dodge") +
  scale_fill_manual(values = rainbow(20)) #무지개색

list.files()
student_kem <- read.csv("학생별과목별성적_국영수_new.csv")
student_kem

ggplot(data = student_kem, aes(x=이름, y=점수)) +
  geom_col(aes(fill=과목)) +
  scale_fill_manual(values = pal) #빈도수를 세고 싶으면 bar, 그 값만큼의 그래프를 그리고 싶으면 cal 

#이름 오름차순, 과목명 내림차순으로 정렬
student_kem <- student_kem %>% arrange(이름, desc(과목))
#텍스트가 찍힐 좌표 
 student_kem <- student_kem %>% group_by(이름) %>% 
  mutate(cumsum = cumsum(점수), position = cumsum - 점수/2)

ggplot(data = student_kem, aes(x=이름, y=점수)) +
  geom_col(aes(fill=과목)) + geom_text(aes(y = position, label = paste(점수, "점"))) +
  scale_fill_manual(values = pal)
