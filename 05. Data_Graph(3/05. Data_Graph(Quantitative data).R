#히스토그램 작성1
hist(survey$Age)

#히스토그램 작성2:확률밀도 그래프
hist(dust$pm25, main="서울시 서대문구 2022년 1월 초미세먼지 측정분포", col=terrain.colors(12), freq = FALSE)

#히스토그램 작성3:확률밀도 선 추가
lines(density(dust$pm25), lwd=2)

#박스플롯 작성1
boxplot(dust$X3pm25, main="야식업의 2022년 1월 미세먼지 발생현황", col="yellow")

#박스플롯 작성2:병렬
boxplot(dust$X3pm25, dust$X7pm25, main="업종별 2022년 1월 미세먼지 발생현황", col="yellow", names = c("야식업","중식"))

#산점도1
plot(x=survey$Grade, y=survey$Age, xlab="Grade", ylab="Age",main="Grade와 Age의 변화")

#산점도2
plot(x=dust$pm10, y=dust$pm25, xlab="미세먼지", ylab="초미세먼지", main="미세먼지와 초미세먼지의 변화", pch=24, col="red", bg="yellow", cex=1.5)

#산점도3
plot(x=dust$pm10, y=dust$pm25, xlab="미세먼지", ylab="초미세먼지", main="미세먼지와 초미세먼지의 변화", type = "h")

#ggplot2 패키지 불러오기
library(ggplot2)

#각 Grade에 대한 Age 상지 그림 만들기
ggplot(survey,aes(x=grade,y=Age))+
  geom_boxplot(fill="lightblue",color="black")+
  labs(title="Age distribution by Grade",x="Grade",y="Age")+
  
  # ggplot2 패키지 설치
  install.packages("ggplot2")

# ggplot2 패키지 로드
library(ggplot2)

# readxl 패키지 설치 및 로드 (엑셀 파일을 읽기 위해 필요)
install.packages("readxl")
library(readxl)

#각 Grade에 대한 Age 상자 그림 만들기 
ggplot(survey,aes(x=Grade,y=Age))+
  geom_boxplot(fill="lightblue",color="black")+
  labs(title="Age Distribution by Grade",x="grade","y=Age")+
  ggtitle("Box Plot of Age by Grade")

#
library(ggplot2)
ggplot(survey, aes(x = Grade, y = Age)) +
  geom_point(color = "blue", size = 3) +  # 산점도 점의 크기와 색상 지정
  labs(title = "Scatter Plot of Grade vs Age", x = "Grade", y = "Age") +  # 제목 및 축 레이블 설정
  theme_minimal()