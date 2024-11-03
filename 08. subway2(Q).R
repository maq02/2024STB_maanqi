#실습에 필요한 packages를 라이브러리에 등록
library(dplyr)
library(ggplot2)

#CSV형식의 파일 불러와서 congestion객체에 입력하고 구조 확인
str(congestion)

#변수의 이상치와 결측치 확인하고 처리
summary(congestion)
#결측치 갯수 확인
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))
#결측치가 있는 행을 제거한 새로운 데이터 프레임 생성
#6시 출발기차의 결측치를 제거
congestion1 <- congestion[!is.na(congestion$s0600),]
colSums(is.na(congestion1))
#23시 30분 출발기차의 결측치를 제거
congestion1 <- congestion1[!is.na(congestion1$s2330),]
colSums(is.na(congestion1))
#남은 결측치를 0으로 대체
congestion1[is.na(congestion1)] <- 0
colSums(is.na(congestion1))

#이상치 확인
ggplot(congestion1, aes(y=s0530))+
  geom_boxplot()
summary(congestion1$s0530)

#파생변수만들기
#1.지하철역의 하루 평균 혼잡도
congestion1$day_mean <- rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])

#데이터분석
#1.  수도권 지하철의 하루 평균 혼잡도
congestion1$day_mean <- rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])
summary(congestion1$day_mean)
# Visualize the distribution of daily congestion
library(ggplot2)
ggplot(congestion1, aes(y = congestion1$day_mean)) +
  geom_boxplot() +
  labs(title = "Daily Average Congestion Distribution", y = "Congestion", x = "")
congestion1$day_mean_congestion <- congestion1$day_mean
head(congestion1)

#2. 호선별 하루평균혼잡도
summary(congestion1$day_mean)

#2. 호선별 출근시간(07:00~09:00)의 혼잡도 평균
congestion1$peak_mean <- 
  rowMeans(congestion1[,c('s0700', 's0730', 's0800', 's0830','s0900')], na.rm = TRUE)
average_peak_congestion <-
  mean(congestion1$peak_mean, na.rm = TRUE)
barplot(congestion1$peak_mean, main="07:00-09:00 Peak Congestion", xlab="Day", ylab="Congestion")
print(average_peak_congestion)
op_4_stations <- 
  head(order(congestion1$peak_mean, decreasing = TRUE), 4)


# 2-1. 호선별 출근시간(07:00~09:00)의 기술통계
congestion_peak_mean <- congestion1 %>%
  group_by(line) %>%
  summarise(mean_congestion_peak = mean(c_across(c('s0700', 's0730', 's0800', 's0830',(s0900))), na.rm = TRUE))
print(congestion_peak_mean)
# 2-2. 평균혼잡도가 가장 높은 시간대를 막대그래프로 그리기
congestion1 <- congestion1 %>%
  group_by(line) %>%
  summarise(peak_mean = mean(c_across(c('s0700', 's0730', 's0800', 's0830', 's0900')), na.rm = TRUE))
barplot(congestion1$peak_mean, 
        names.arg = congestion1$line, 
        main = "Average Congestion by Line (07:00-09:00)", 
        xlab = "Line", 
        ylab = "Average Congestion", 
        col = "lightblue", 
        border = "black",
        las = 2)  # las=2

#2-3. 평균혼잡도가 가장 높은 호선에서 기여도가 높은 역
# Use the 'congestion1' calculated in step 2-1, and sort by average congestion
top_congested_lines <- congestion1 %>%
  arrange(desc(peak_mean)) %>%
  head(5) # Extract the top 5 lines with the highest average congestion

# Print the result
print(top_congested_lines)

#3.08시 지하철 혼잡도 범주화/범주별 빈도분석
congestion1 %>%
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%
  group_by(s80_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  select(s80_grade,n,pct)%>%
  arrange(desc(n))

#3-1. 호선별로 08시 지하철 혼잡도 범주화
congestion1 %>%
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%
  group_by(line, s80_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  filter(s80_grade=="caution")%>%
  select(line, s80_grade,n,pct)%>%
  arrange(desc(pct))%>%
  head(5)

# 4. 호선별 퇴근시간(18:00~20:00)의 혼잡도 평균
congestion1$peak_mean <- 
  rowMeans(congestion1[,c('s1800', 's1830', 's1900', 's1930','s2000')], na.rm = TRUE)
average_peak_congestion <-
  mean(congestion1$peak_mean, na.rm = TRUE)
barplot(congestion1$peak_mean, main="18:00-20:00 Peak Congestion", xlab="Day", ylab="Congestion")
print(average_peak_congestion)
op_4_stations <- 
  head(order(congestion1$peak_mean, decreasing = TRUE), 4)
#4-1. 호선별 퇴근시간(18:00~20:00)의 기술통계
congestion_peak_mean <- congestion1 %>%
  group_by(line) %>%
  summarise(mean_congestion_peak = mean(c_across(c('s1800', 's1830', 's1900', 's1930',(s2000))), na.rm = TRUE))
print(congestion_peak_mean)
#4-2. 평균혼잡도가 가장 높은 시간대를 막대그래프로 그리기
congestion1 <- congestion1 %>%
  group_by(line) %>%
  summarise(peak_mean = mean(c_across(c('s1800', 's1830', 's1900', 's1930', 's2000')), na.rm = TRUE))
barplot(congestion1$peak_mean, 
        names.arg = congestion1$line, 
        main = "Average Congestion by Line (18:00-20:00)", 
        xlab = "Line", 
        ylab = "Average Congestion", 
        col = "lightblue", 
        border = "black",
        las = 2)  # las=2

#4-3. 평균혼잡도가 가장 높은 호선에서 기여도가 높은 역
# Use the 'congestion1' calculated in step 2-1, and sort by average congestion
top_congested_lines <- congestion1 %>%
  arrange(desc(peak_mean)) %>%
  head(5) # Extract the top 5 lines with the highest average congestion
