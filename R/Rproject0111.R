Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_192")
library(KoNLP)

text

useNIADic() #단어 가져오기
text_noun <- extractNoun(text) 
text #형태가 2차형 리스트

text_unlist <- unlist(text_noun) #2차형 리스트를 2차형 벡터로 변환
text_unlist

#불용어 처리
text_unlist <- text_unlist[nchar(text_unlist) > 1] # 글자수가 2개 이상인것만 남기기
text_unlist <- gsub("들이", "", text_unlist)
text_unlist

text_table <- table(text_unlist) #단어가 column명

head(sort(text_table, decreasing = T), 10)

install.packages("wordcloud2") #시각화는 좋으나 정확한 수치를 보기는 힘듦
library(wordcloud2)

wordcloud2(text_table, size = 5) 


#실습 영화리뷰 데이터 -> 워드 클라우드 그리기
list.files()
review <- readLines("review.txt")
review

review_noun <- extractNoun(review)
review_noun

review_unlist <- unlist(review_noun) 
review_unlist

#전처리
review_unlist <- review_unlist[nchar(review_unlist) > 1]

newwords <- readLines("newword.txt")
mergeUserDic(data.frame(newwords, "ncn"))

review_unlist <- gsub("들이", "", review_unlist)
review_unlist <- gsub("하지", "", review_unlist)
review_unlist <- gsub("해서", "", review_unlist)
review_unlist <- gsub("같습니", "", review_unlist)
review_unlist <- gsub("하면", "", review_unlist)

review_unlist <- gsub("이영화", "영화", review_unlist)
review_unlist <- gsub("이런영화", "영화", review_unlist)
review_unlist <- gsub("영화의", "영화", review_unlist)

review_unlist <- gsub("^재미없", "재미없다", review_unlist)
review_unlist <- gsub("^재밌", "재미", review_unlist)
review_unlist <- gsub("^재미", "재미", review_unlist)
review_unlist <- gsub("재미게", "재미", review_unlist)
review_unlist <- gsub("재미", "재미", review_unlist)
review_unlist <- gsub("재미어요", "재미", review_unlist)
review_unlist <- gsub("브금", "ost", review_unlist)
review_unlist <- gsub("bgm", "ost", review_unlist)
review_unlist <- gsub("노래", "ost", review_unlist)
review_unlist <- gsub("음ㅇ", "ost", review_unlist)
review_unlist <- gsub("[ㅋ]", "크크크크", review_unlist)
review_unlist <- gsub("[ㅎ]", "하하", review_unlist)
review_unlist <- gsub("[ㄷ]", "덜덜", review_unlist)
review_unlist <- gsub("[ㄱ]", "고고", review_unlist)

review_unlist <- gsub("^후미", "후미코", review_unlist)
review_unlist <- gsub("루즈", "지루", review_unlist)


review_table <- table(review_unlist)
review_table <- review_table[text_table > 1]
head(sort(review_table, decreasing = T), 50)

wordcloud2(review_table, size = 10, minSize = 5)
#우들의 연기력은  좋았으나 재미는 없고 지루했다.


for (i in 1:length()) { #1부터 끝번까지 반복
  text_unlist <- gsub(subwords[i], "", text_unlist)
}