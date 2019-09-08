

setwd("D:/sales_anaysis/Sales-Analysis-Recommendations")
getwd()

rm(list=ls()); gc()

# rm(cust_prod_total_fix_2) ## 메모리 문제 있으므로 사용후 삭제
# load("cust_prod_total.RData") # RFM 분석 시작 직전 데이터
# glimpse(cust_prod_total_fix_2)
# # Observations: 5,410,838
# # Variables: 20
# 
# rm(userRFM_uniq)
# load("userRFM_full_uniq.RData") # custid 수정된 최종 고객 마스터 
# glimpse(userRFM_uniq)
# # Observations: 1,344,345
# # Variables: 13
# View(userRFM_uniq)

######### 코드 결과물 RData 


### 코드 작업 

library(readxl)
library(plyr)
library(tidyverse)
library(data.table)


# # file.exists("data/cust_mon_201804.xlsx")
# 
# # https://stackoverflow.com/questions/32888757/reading-multiple-excel-files-into-r-best-practice
# # 현재위치 지정
# # setwd("C:/Users/Daniel/BIgdata project Dropbox/Bigdata Project/김정규") 
# 
# # 디렉토리를 순환하면서 파일명 가져오기 
# temp_file <- list.files(pattern='cust_mon_age_cate.*\\.xlsx$', recursive = TRUE) 
# temp_file # 파일명 리스트 확인
# 
# # 모두 한번에 불러오기
# df_list <- lapply(temp_file, read_excel) # lapply를 이용해서 엑셀 읽기
# df_list[[1]] # tibble: 228,058 x 13
# # DATE member_num  grade    GPC on_off   QTY   AMT sex   age  
# # product_name  category  category_function  category_line
# 
# # https://stackoverflow.com/questions/2851327/convert-a-list-of-data-frames-into-one-data-frame
# # system.time({ df.list.rbind <- do.call("rbind", df.list) })  
# # list 형태의 data.frame 을 하나의 data.frame으로
# 
# 
# # install.packages("data.table")
# library(data.table)
# df_list_rbindlist <- rbindlist(df_list)
# 
# # system.time({df_list_rbindlist <- rbindlist(df_list)})
# # df_list_ldply <- ldply(df_list, data.frame)
# # head(df_list_ldply)
# 
# glimpse(df_list_rbindlist) 
# # Observations: 6,605,815
# # Variables: 13
# colnames(df_list_rbindlist) <- c('date', 'custid', 'grade', 'prod_code', 
#                                  'on_off', 'qty', 'amt', 'sex', 'age',
#                                  'prod_nm', 'cate', 'cate_ftn', 'cate_line')
# cust_prod_total <- df_list_rbindlist
# head(cust_prod_total)
# nrow(cust_prod_total) # 6,605,815 : 총 구매 제품 개수 
# sum(cust_prod_total$amt) # 127,132,096,385 : 총 매출액 
# 
# ### 월별로 받은 데이터를 합칠때
# ## 18/01 loading
# # cust_prod_1801 <- read_excel("data/cust_mon_cate_age_201801.xlsx")
# # dim(cust_prod_1801)
# # colnames(cust_prod_1801) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt', 
# #                              'prod_dep', 'prod_div', 'prod_name', 'sex', 'age')
# #
# ## 열 결합 : rbinding data (1~6월)
# # cust_prod_total <- rbind(cust_prod_1801, cust_prod_1802, cust_prod_1803,
# #                        cust_prod_1804, cust_prod_1805, cust_prod_1806)
# # dim(cust_prod_1801)[1] + dim(cust_prod_1802)[1] + 
# #  dim(cust_prod_1803)[1] + dim(cust_prod_1804)[1] + 
# #  dim(cust_prod_1805)[1] + dim(cust_prod_1806)[1] == dim(cust_prod_total)[1] 
# ## True 확인
# ### 
# 
# # 0, 20만원 이상, 연간 6회 이상/60만원 이상 
# # 멤버십 30% 할인할때만 사는 사람들!!
# # 할인이 있을 때만 사는 패턴을 보이는 고객들을
# # 그대로 두는게 맞는 것일까? (세일할때만 사는 고객 등급 유지 여부)
# 
# # 매년 5, 11월 이벤트 진행
# # 특이사항 :  2018년만 5월 -> 6월로 진행 

load('cust_prod_total_raw.RData')
head(cust_prod_total)
glimpse(cust_prod_total) # Observations: 6,605,815

# 고객등급명을 영문으로 변경
unique(cust_prod_total$grade)
cust_prod_total$grade[cust_prod_total$grade == '바디러브'] <- 'love'
cust_prod_total$grade[cust_prod_total$grade == '클럽'] <- 'club'
cust_prod_total$grade[cust_prod_total$grade == '골드'] <- 'gold'


### 회원등급, 온/오프라인, 성별, 연령대 -> 팩터로 변환 -> 시각화
cust_prod_total$grade <- as.factor(cust_prod_total$grade)
summary(cust_prod_total$grade)  
# 클럽 115만, 골드 42만, 러브 502만 

### 회원등급
grade <- names(table(cust_prod_total$grade))
grade
class(grade)
grade_num <- as.numeric(unname(table(cust_prod_total$grade))); grade_num
class(grade_num)

grade_df <- data.frame(grades = grade, number = grade_num)
str(grade_df)

# windows()
ggplot(grade_df, aes(x=grades, y=number)) + 
  geom_bar(stat='identity', fill="steelblue") +
  scale_x_discrete(limits=names(table(cust_prod_total$grade))) +
  theme_minimal() 

## pie chart
grade_dfc <- grade_df %>%
  mutate(share = number/sum(number)*100.0) %>%
  arrange(desc(number))

ggplot(grade_dfc, aes("", share, fill = grades)) +
  geom_bar(width = 1, size = .2, stat = "identity", color='black') +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(share), "%")), 
            position = position_stack(vjust = 0.4), size=7) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Membership Grades Ratio") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))

## 시계열별 변화보기 필요(예 6개월씩의 변화) 
## love -> club 옮기는 것이 미션 
## (간격이 너무 크기때문에 love+ 단계를 더 만들 필요)


### 온/오프라인 구매 구분.
###
cust_prod_total$on_off <- as.factor(cust_prod_total$on_off)
summary(cust_prod_total$on_off) 
# off 6,219,970, on 385,845

## bar chart
users<- names(table(cust_prod_total$on_off))
users
class(users)
users_num <- as.numeric(unname(table(cust_prod_total$on_off))) 
users_num
class(users_num)

users_df <- data.frame(users = users, number = users_num)
users_df
# 1   off 6219970
# 2    on  385845
ggplot(users_df, aes(x=users, y=number)) + 
  geom_bar(stat='identity', fill='steelblue') +
  scale_x_discrete(limits=names(table(cust_prod_total$on_off))) +
  theme_minimal()

## pie chart
str(users_df)
users_dfc <- users_df %>%
  mutate(share = number/sum(number)*100.0) %>%
  arrange(desc(number))
users_dfc

ggplot(users_dfc, aes("", share, fill = users)) +
  geom_bar(width = 1, size = .2, stat = "identity", color='black') +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(share), "%")), 
            position = position_stack(vjust = 0.4), size=7) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Member's On/Off Ratio") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))

### 성별
cust_prod_total$sex <- as.factor(cust_prod_total$sex)
summary(cust_prod_total$sex)
# table(cust_prod_total$sex)  
#  남 245,815, 여 6,359,694     NA 306 (구매내력 != 구매인원) 

## bar chart
sex <- names(table(cust_prod_total$sex))
sex
class(sex)
sex_num <- as.numeric(unname(table(cust_prod_total$sex)))
sex_num  # 남 245,815, 여 6,359,694

sex_df <- data.frame(gender = sex, number = sex_num)
sex_df
ggplot(sex_df, aes(x=gender, y=number)) + 
  geom_bar(stat='identity', fill='steelblue') +
  scale_x_discrete(limits=names(table(cust_prod_total$sex))) +
  theme_minimal()

## pie chart
sex_dfc <- sex_df %>%
  mutate(share = number/sum(number)*100.0) %>%
  arrange(desc(number))

ggplot(sex_dfc, aes("", share, fill = gender)) +
  geom_bar(width = 1, size = .2, stat = "identity", color='black') +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(share), "%")), 
            position = position_stack(vjust = 0.4), size=7) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Member's Gender Ratio") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))

### 연령대 (오프라인 가입시 인증X -> 통합하면 나이 인증 but)
cust_prod_total$age <- as.factor(cust_prod_total$age)
summary(cust_prod_total$age) # none 277만, NA 33만
table(cust_prod_total$age) # 추후 나이 예측모델 구현 가능???????? 
# 0살대 11명, 100살이상 49명.. 정보 신뢰 부족/ 결측치 많음(확인 필요)

## bar chart
## none 제외
age_table <- table(cust_prod_total$age)[1:11]
age_table
names(age_table) <-  c('0-10',' 10-20', '100+', '20-30', '30-40', '40-50', 
                       '50-60', '60-70', '70-80', '80-90', '90-100')
age_table
age <- names(age_table)
age
age_num <- as.numeric(unname(age_table))
age_num

class(age_num)
age_df <- data.frame(age_nm = age, number = age_num)
glimpse(age_df)
age_df
age_df <- rbind(age_df[-3, ], age_df[3, ]) # 100세 이상을 맨 뒤로
age_df

# windows()
ggplot(age_df, aes(x=age_nm, y=number)) + 
  geom_bar(stat='identity', fill='steelblue') +
  scale_x_discrete(limits=age_df$age_nm) +
  theme_minimal()


########### 날짜변환!!!!!!!!
head(cust_prod_total$date)
class(cust_prod_total$date) # 숫자형
cust_prod_total$date <- as.POSIXct(as.character(cust_prod_total$date), 
                                   format="%Y%m%d") # 시간형으로 변환 (시간소요)
class(cust_prod_total$date)
glimpse(cust_prod_total) # Observations: 6,605,815

# prod_code 형식을 int에서 chr로
cust_prod_total$prod_code <- as.character(cust_prod_total$prod_code)
glimpse(cust_prod_total)


##
## 날짜 변수 변환 코드 개선
##
gc()
# 영어로 로케일 설정
Sys.setlocale("LC_TIME", "English_United States.1252") # Windows
# Sys.setlocale("LC_TIME", "en_US.UTF-8") # Mac

# 날짜를 한번에 연/월/일 나누기 
# https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html
cust_prod_total <- cust_prod_total %>%
  # dplyr::mutate(date = lubridate::ymd(date)) %>% 
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date),
                weekday = lubridate::wday(date, label = TRUE))
glimpse(cust_prod_total)

# 한국어로 재설정
Sys.setlocale("LC_TIME", "Korean_Korea.949") # Windows
# Sys.setlocale("LC_TIME", "ko_KR.UTF-8") # Mac

# 수량, 구매액 이상치 확인 
range(cust_prod_total$qty) # -200 ~ 1900
summary(cust_prod_total$qty) # median 1, mean 1.245
range(cust_prod_total$amt) # -3850000 ~ 29232000
summary(cust_prod_total$amt) # median 17,000, mean 19,245

# 제품 판매가격 최소단위가 1000원
# 1000원 이하 구매 고객이 45만명 
# 포인트 최소 사용가능은 2500포인트부터 
# 12만 5천원 구매 -> 2500포인트 

## 수량, 구매액 음수(환불)는 NA 처리 : 큰 의미 없음
cust_prod_total <- cust_prod_total %>% 
  mutate(qty = replace(qty, qty<=0, NA),
         amt = replace(amt, amt<=0, NA)) 

cust_prod_total <- cust_prod_total %>%
  drop_na()

# 구매 갯수 범위
range(cust_prod_total$qty) # 1 ~ 1900개
summary(cust_prod_total$qty) # median 1, mean 1.28

# 구매액 범위
range(cust_prod_total$amt) # 3 ~ 29,232,000 (2천9백만)
summary(cust_prod_total$amt) # median 17,550, mean 20,109

# 결과 시각화 
hist(cust_prod_total$qty, xlim=c(0,20), breaks = 1500)
hist(cust_prod_total$amt, breaks = 1000000, xlim=c(0,100000))
head(cust_prod_total)

hist(cust_prod_total$amt)


####
## 구입 단가(price) 변수 추가
cust_prod_total <- cust_prod_total %>% 
  mutate(price = amt / qty)

# 단가 범위
range(cust_prod_total$price) # 3 ~ 108,200 (10만8천원)
summary(cust_prod_total$price) # median 16,200, mean 17,064
hist(cust_prod_total$price)
# 5천원 이하가 제일 많음, 15000 ~ 20000원까지가 다음
# 포인트로 결제???

dim(cust_prod_total) 
# 2년 6개월간 6,085,002건의 거래/ 변수 18개 
glimpse(cust_prod_total) 
length(unique(cust_prod_total$custid)) # 고객의 수 : 1,353,361


#########
### 기본 EDA--------------
glimpse(cust_prod_total)
#1. 매출을 기준으로 가장 소비를 많이하는 고객Id는?
salesCust <- aggregate(cust_prod_total$amt, 
                       by=list(customer_ID=cust_prod_total$custid), FUN=sum)

# 데이터의 특정 컬럼을 기준으로 통계량 구함
head(salesCust)
str(salesCust)
## 이상한 아이디 6. 6@))%^%*$)%!&(@&% ->> 최종 마스터에서 변경 

range(salesCust$x) # 10 ~ 1,667,407,595 (최대 16억 7천)
ordersales <- order(salesCust$x, decreasing=T)
head(salesCust[ordersales, ], 10)
# 1위 1,667,407,595원 (16억 7천)
#10위    24,482,770원 (2천 4백만)

## 혹은...
cust_prod_total %>%
  group_by(custid) %>%
  summarise(sum_cust_amt = sum(amt)) %>%
  arrange(desc(sum_cust_amt))


#3. 가장 매출이 많은 요일은?
salesWeekday <- aggregate(cust_prod_total$amt, 
                          by=list(weekday=cust_prod_total$weekday), FUN=sum)
head(salesWeekday)

orderWeekday <- order(salesWeekday$x, decreasing=T)
head(orderWeekday)

## 혹은...
head(cust_prod_total)
cust_prod_total %>%
  group_by(weekday) %>%
  dplyr::summarise(sum_weekday_amt = sum(amt)) %>%
  arrange(desc(sum_weekday_amt)) 

weekday_sum_df <- cust_prod_total %>%
  group_by(weekday) %>%
  dplyr::summarise(sum_weekday_amt = sum(amt))

# windows()
weekday_sum_df %>% ggplot(aes(x=weekday, y=sum_weekday_amt)) + 
  geom_bar(stat='identity', fill='steelblue') +
  scale_x_discrete(limits=weekday_sum_df$weekday) 

####
glimpse(cust_prod_total)
#### date컬럼 -> 2012-04-05 형식


# 고객들의 매장 방문(및 구매)횟수 ############################################
### Frequency 정의 
glimpse(cust_prod_total)

summary(cust_prod_total$date)
summary(cust_prod_total$custid)
str(cust_prod_total$custid)

userF <- cust_prod_total %>%
  group_by(custid) %>%
  dplyr::summarize(frequency=n_distinct(date))

head(userF)
glimpse(userF) # Observations: 1,353,361

# Frequency를 정의 하는 것은 분석가의 role
# 위와 같이 분석 -> 구매를 한 날짜로 다시 group_by, summarize
# -> 구매한 날짜의 빈도가 나올 것 
# -> 그것을 "빈도"로 정의하여 분석 가능

# 빈도의 기준 중, 하루에 여러번 구매를 하더라도 한번으로 보고
# (ifelse(x >= 1,1,0) 분석을 하는 경우 많음.


range(userF$frequency) # 1 ~ 최고 395회 방문 (구매)
summary(userF$frequency) # median : 1, mean : 2, max : 395
boxplot(userF$frequency, horizontal = T)
hist(userF$frequency, breaks = 100000)

userF %>% filter(frequency >= 100) %>% nrow
userF %>% filter(frequency >= 50) %>% nrow
userF %>% filter(frequency >= 30) %>% nrow ## frequency는 30이상을 이상치로 가정?

# 1년에 6번 구매가 골드 등급 구매 유지 횟수
# 2년 6개월이면 15번 구매 ->> frequency는 15회 이상을 이상치로 가정!!!
userF %>% filter(frequency >= 15) %>% nrow ## 8886, 전체의 0.7% 제거
# userF %>% filter(frequency >= 10) %>% nrow

head(cust_prod_total)
head(userF)
cust_prod_total_freq <- cust_prod_total %>%
  left_join(userF, by='custid')

# 이상치 제외한 데이터 생성  
head(cust_prod_total_freq)
cust_prod_total_freq_15 <- cust_prod_total_freq %>%
  filter(frequency < 15)

nrow(cust_prod_total_freq_15) # 5,513,041
nrow(cust_prod_total) # 6,085,002
head(cust_prod_total_freq_15)


### 이상치 제거를 통해 분석대상 축소
userF_15 <- userF %>% 
  filter(frequency < 15)

# boxplot
ggplot(userF_15, aes("", y=frequency)) + 
  geom_boxplot(fill='steelblue', color='black') + coord_flip()

# histogram
ggplot(userF_15, aes(userF_15$frequency)) +
  geom_histogram(breaks=seq(0, 15, by=1), fill='steelblue', color='white')

# boxplot(userF_15$frequency, horizontal = T)
# hist(userF_15$frequency, breaks = 100)


## Monetary 이상치 제거 

# 100만원 이상은 일반적으로 기업특판(노동절 선물, 창립기념 등)
# 또는 개인특판(경조사, 돌잔치, 답례품 등)의 주문건에 해당
# 고객분석에 적합한 주문이 아니라는 판단, 제외 결정

## 일별 구매금액이 100만원 이상인 주문건 제거
cust_prod_total_fix <- cust_prod_total %>%
  group_by(custid, date) %>%
  dplyr::mutate(sum_day_amt = sum(amt)) %>%
  filter(sum_day_amt < 1000000) 

(nrow(cust_prod_total) - nrow(cust_prod_total_fix)) / nrow(cust_prod_total)
# 0.01759276 : 전체의 1.8%

## 2년 6개월간 구매자 구매 횟수 컬럼 생성
cust_prod_total_fix_1 <- cust_prod_total_fix %>%
  group_by(custid) %>%
  dplyr::mutate(frequency=n_distinct(date))

## 2년 6개월간 15회 이상 구매자 제외
cust_prod_total_fix_2 <- cust_prod_total_fix_1 %>%
  filter(frequency < 15)

(nrow(cust_prod_total) - nrow(cust_prod_total_fix_2)) / nrow(cust_prod_total)
# 0.1107911 : 전체의 11%

glimpse(cust_prod_total_fix_2)
# Observations: 5,410,838
# Variables: 20

# RData는 R전용 데이터 파일이므로 R에서 읽고 쓰는 속도가 빠르고 용량이 작다.
# R에서 분석작업을 할때 RData를 사용하고 
# R아닌 곳에서 파일을 주고받을시에는 CSV를 사용한다.
# save()를 이용하여 데이터 프레임을 rda파일로 저장한다.
# rda파일을 불러올때는 load()함수를 이용한다. 
# rda파일을 불러오면 저장할때 사용한 데이터 프레임이 자동으로 만들어진다.

# df_midterm <- data.frame(english = c(90, 80, 60, 70),
#                         math = c(50, 60, 100, 20),
#                         class = c(1, 1, 2, 2))
# df_midterm
# save(df_midterm, file = "df_midterm2.rda")
# rm(df_midterm)   #데이터프레임 삭제
# load("df_midterm2.rda")  # 불러올때 저장시 사용한 df_midterm가 자동으로 생성

save(cust_prod_total_fix_2, file="cust_prod_total.RData")
rm(cust_prod_total_fix_2)

load("cust_prod_total.RData")


glimpse(cust_prod_total_fix_2)


# Observations: 5,410,838
# Variables: 20

#####


# RFM 
userRFM <- cust_prod_total_fix_2 %>% 
  group_by(custid) %>% # 고객별 
  dplyr::summarize(minRecency=min(date), # 최초 구매일
                   recency=max(date),    # 최근 구매일
                   monetary=sum(amt), # 총구매액
                   frequency=min(frequency), ### 에러 수정 (값 안들어감)
                   period=as.numeric(max(date)-min(date))) # 방문 최대 텀

userRFM

cust_key <- read.csv('userRFM_custid_key.csv')
head(cust_key)

userRFM %>% 
  left_join(cust_key, by='custid') -> userRFM


userRFM %>%
  select(-custid) %>%
  select(custid_no, minRecency:period) -> userRFM

userRFM %>% rename(custid = custid_no) -> userRFM; 
userRFM

# A tibble: 1,344,345 x 6
# custid_no minRecency          recency             monetary frequency period
# <fct>     <dttm>              <dttm>                 <dbl>     <dbl>  <dbl>
#   1 C00000001 2016-05-23 00:00:00 2018-05-15 00:00:00   63900.        2.   722.
# 2 C00000002 2016-02-27 00:00:00 2017-03-17 00:00:00  116500.        2.   384.
# 3 C00000003 2018-06-30 00:00:00 2018-06-30 00:00:00   19200.        1.     0.
# 4 C00000004 2016-10-08 00:00:00 2016-10-08 00:00:00   28000.        1.     0.
# 5 C00000006 2018-06-07 00:00:00 2018-06-07 00:00:00   46400.        1.     0.
# 6 C00000005 2016-12-04 00:00:00 2016-12-04 00:00:00   29500.        1.     0.
# 7 C00000009 2016-01-03 00:00:00 2016-11-20 00:00:00  279000.        6.   322.
# 8 C00000007 2017-06-15 00:00:00 2017-06-15 00:00:00   27500.        1.     0.
# 9 C00000008 2016-11-05 00:00:00 2016-11-05 00:00:00   71100.        1.     0.
# 10 C00000010 2016-07-25 00:00:00 2016-09-27 00:00:00   39000.        2.    64.
# # ... with 1,344,335 more rows

# nrow(userRFM) # 고객수 : 1,344,345
# glimpse(userRFM)
# 
# # userF의 <frequency>는 구매일자로 집계된 count[N=n()] 
# # 즉, 고객의 매장 방문 횟수로 정의
# 
# range(userRFM$period)  # 0 ~ 911
# summary(userRFM$period) # median 0, mean 116.7
# hist(userRFM$period, breaks = 300)
# 
# ggplot(data=userRFM, aes(userRFM$period)) +
#   geom_histogram(breaks=seq(0, 300, by=3), fill='steelblue', color='white')
# 
# ggplot(data=userRFM, aes(userRFM$period)) +
#   geom_histogram(breaks=seq(0, 300, by=5), fill='steelblue', color='white')
# 
# ggplot(data=userRFM, aes(userRFM$period)) +
#   geom_histogram(breaks=seq(0, 300, by=7), fill='steelblue', color='white')
# 
# nrow(userRFM) # 2년 6개월간 고객의 수 : 1,344,345명
# 
# 
# range(userRFM$monetary)
# # 10 6457720
# 
# 
# hist(userRFM$monetary)
# # 이상치 제거 기준?
# # 장바구니별/일자



## RFM연습 다시 시작-------
########################### Recency에 대한 EDA ###########################
### 고객들의 최초 방문일에 대한 일자별 갯수 파악
userRFM$minDate <- as.numeric(as.POSIXct(userRFM$minRecency, origin="1970-01-01"))
glimpse(userRFM)
userRFM$minDate <- as.Date(userRFM$minRecency, origin="1970-01-01")

# head(userRFM$minDate)
# summary(userRFM$minDate) 
# 
# hist(userRFM$minDate, breaks=30) # 최초 방문일??
# hist(userRFM$minDate, breaks=300)
# hist(userRFM$minDate, breaks=900) 
# 
# # 월별 고객들의 최초 방문일(first_visit_day) 수에 대한 hist
# hist(userRFM$minDate, breaks='month', 
#      xlab=deparse(substitute(userRFM$minDate)), 
#      start.on.monday = F, freq=T, format='%Y-%m-%d') 
# 
# # 주간별 고객들의 최초 방문일(first_visit_day) 수에 대한 hist
# hist(userRFM$minDate, breaks='weeks', 
#      xlab=deparse(substitute(userRFM$minDate)), 
#      start.on.monday = F, freq=T, format='%Y-%m-%d') 
# 
# # 일자별 고객들의 최초 방문일(first_visit_day) 수에 대한 hist
# hist(userRFM$minDate, breaks='days', 
#      xlab=deparse(substitute(userRFM$minDate)), 
#      start.on.monday = F, freq=T, format='%Y-%m-%d') 
# # 웹사이트 리뉴얼 & 앱오픈! 앱가입시 20% 할인 (9~10월)
# 
# 
# first_visit_day_count_df <- userRFM %>% 
#   group_by(minDate) %>%
#   dplyr::summarise(count_minDate = n()) 
# 
# first_visit_day_count_df$minDate <- as.character(first_visit_day_count_df$minDate)
# first_visit_day_count_df
# 
# # windows()
# ggplot(first_visit_day_count_df, aes(x=minDate, y=count_minDate)) + 
#   geom_bar(stat='identity', fill='steelblue', color='white') +
#   scale_x_discrete(limits=first_visit_day_count_df$minDate) +
#   theme(axis.text.x = element_text(angle=90, hjust=1))


### 고객들의 최근 방문일에 대한 일자별 갯수 파악
### Recency
# userRFM$maxDate <- as.Date(userRFM$recency, origin="1970-01-01")
# 날짜형으로 할 경우 이후 계산시간 소요 -> 숫자형으로 변경 

userRFM$maxDate <- as.numeric(as.POSIXct(userRFM$recency, origin="1970-01-01"))
head(userRFM$maxDate)
summary(userRFM$maxDate)

# 숫자형으로 할 경우 hist 안그려짐 
#hist(userRFM$maxDate, breaks=30) # 최근 방문일??

# 월별 고객들의 최근 방문일(last_visit_day) 수에 대한 hist
#hist(userRFM$maxDate, breaks='month', 
#     xlab=deparse(substitute(userRFM$maxDate)), 
#     start.on.monday = f, freq=T, format='%Y-%m-%d') 
# 주간별 고객들의 최근 방문일(last_visit_day) 수에 대한 hist
#hist(userRFM$maxDate, breaks='weeks', 
#     xlab=deparse(substitute(userRFM$maxDate)), 
#     start.on.monday = F, freq=T, format='%Y-%m-%d') 
# 일자별 고객들의 최근 방문일(last_visit_day) 수에 대한 hist
#hist(userRFM$maxDate, breaks='days', 
#     xlab=deparse(substitute(userRFM$maxDate)), 
#     start.on.monday = F, freq=T, format='%Y-%m-%d') 

# str(userRFM)
# length(unique(userRFM$maxDate))
# 
# plot(table(userRFM$maxDate), main="Customer Recency")
# plot(table(userRFM$frequency), main="Customer Frequency")
# 
# last_visit_day_count_df <- userRFM %>% 
#   group_by(maxDate) %>%
#   dplyr::summarise(count_maxDate = n())
# 
# last_visit_day_count_df$maxDate <- as.character(last_visit_day_count_df$maxDate)
# last_visit_day_count_df
# 
# # windows()
# ggplot(last_visit_day_count_df, aes(x=maxDate, y=count_maxDate)) + 
#   geom_bar(stat='identity', fill='steelblue', color='white') +
#   scale_x_discrete(limits=last_visit_day_count_df$maxDate) +
#   theme(axis.text.x = element_text(angle=90, hjust=1))
# 


########################### Frequency에 대한 EDA ###########################
# 고객별 매장방문 회수에 대한 시각화
# head(userRFM)
# range(userRFM$frequency) # 14번 구매가 최대!!
# 
# hist(userRFM$frequency, breaks = 100)
# hist(userRFM$frequency, breaks = 100, xlim=c(1, 10))
# hist(userRFM$frequency, breaks = 100, xlim=c(1, 5))
# 
# dim(userRFM) # 1,344,345   8
# 
# freq_1 <- userRFM %>%
#   filter(frequency == 1)
# 
# dim(freq_1) # 1회 구매 고객 839364명 : 전체의 62.4%
# (839364 / 1344345) * 100
# 
# freq_2 <- userRFM %>%
#   filter(frequency == 2)
# dim(freq_2) # 2번 구매 고객 261949명 : 전체의 19.5%
# (261949 / 1344345) * 100



########################## Monetary(구매액)에 대한 EDA ###########################
### 고객들의 지출 비용에 대한 분포 파악
### Monetary 

# range(userRFM$monetary) # 10 ~ 6,457,720 (645만원)
# summary(userRFM$monetary) # median 4만7천원, mean 7만8천962원
# 
# hist(userRFM$monetary, breaks=10000, main='Cust Monetary')
# hist(log10(userRFM$monetary), breaks=100)
# 
# 
# ggplot(data=userRFM, aes(userRFM$monetary)) +
#   geom_histogram(breaks=seq(0, 500000, by=10000), fill='steelblue')
# 
# ggplot(data=userRFM, aes(userRFM$monetary)) +
#   geom_histogram(breaks=seq(0, 400000, by=10000), fill='steelblue')
# 
# ggplot(data=userRFM, aes(userRFM$monetary)) +
#   geom_histogram(breaks=seq(0, 300000, by=10000), fill='steelblue')
# 
# ggplot(data=userRFM, aes(userRFM$monetary)) +
#   geom_histogram(breaks=seq(0, 300000, by=1000), fill='steelblue')
# 
# 
# # 0~10만원 구매회수 -> 10000원(만원) 단위
# hist(userRFM$monetary, breaks=2000, main='Cust Monetary', 
#      xlim = c(0,100000)) ## 안보임
# 
# # 0~10만원 구매회수 -> 1000원(천원) 단위
# hist(userRFM$monetary, breaks=20000, main='Cust Monetary', 
#      xlim = c(0,100000)) ## 안보임
# 
# # 0~60만원 구매회수
# hist(userRFM$monetary, breaks=100000, main='Cust Monetary', 
#      xlim = c(0,600000))
# # 4 ~ 5만원대가 많다
# 
# hist(userRFM$monetary, breaks=200000, main='Cust Monetary', 
#      xlim = c(0,300000))
# # 3 ~ 4만원대가 많다
# 
# hist(userRFM$monetary, breaks=1000000, main='Cust Monetary', 
#      xlim = c(0,200000))
# # 4군데 peak가 보인다 (4만9천원대에서 다시 상승)
# 
# ### long tail!!!
# 
# 
# # 제품 가격대 구성이 어떻게 되어있나???
# # 개당 가격은???
# 
# hist(userRFM$monetary, breaks=100) # 5만원 이상 희박, 0~5만원 확대하여 보기
# hist(userRFM$monetary, breaks=10000, xlim = c(0,50000))
# hist(userRFM$monetary, breaks=100000, xlim = c(0,50000))
# 
# # 가장 지출이 많이 되는 금액대는 만원~3만1천원 사이에 있어 이 부분만 확대하여 보기
# hist(userRFM$monetary, breaks=100000, xlim = c(10000,31000)) 
# # 1만원~3만1천원 사이를 200원씩 binning
# 10000/50 
# 
# 
# # 많이 지출되는 금액 확대해서 보기
# ggplot(data=userRFM, aes(userRFM$monetary)) +
#   geom_histogram(breaks=seq(0, 50000, by=1000), fill='steelblue', color='white')


# 고객들은 2만원에서 2만 1천원의 범위의 지출을 가장 많이 하였음.
# 고객들은 2만 7천원에서 2만 8천원의 범위의 지출을 두 번째로 많이 하였음.
# 고객들은 2만 9천원에서 3만원의 범위의 지출을 세 번째로 많이 하였음.


## RFM별 상위 20%가 차지하는 총 매출액 대비 비중 ###########################################

# R : 최근방문 분위수 (날짜를 numeric으로 바꾸면 분위수 계산 가능)
# quantR <- as.Date(quantile(as.numeric(userRFM$maxDate),
#                            c(0,0.2,0.4,0.6,0.8,1)), origin="1970-01-01")

####
#### 계산 속도를 위해 숫자형으로 
quantR <- as.numeric(quantile(userRFM$maxDate,c(0,0.2,0.4,0.6,0.8,1)))
# quantR
# quantR[5] # R 분위수가 0.8이 되는 일자는 ?? (날짜형으로 해야 결과 출력)


# F : 빈도 분위수 
quantF <- quantile(userRFM$frequency, c(0,0.8,0.85,0.9,0.95,1))
quantF
quantF[5] # F 분위수가 0.8이 되는 횟수는 "2"회

# M : 구매액 분위수
quantM <- quantile(userRFM$monetary, c(0,0.2,0.4,0.6,0.8,1))
quantM
quantM[5] # M 분위수가 0.8이 되는 액수는 "103,000" 원


## RFM별 상위 20%가 차지하는 총 매출액 대비 비중

# recency 상위 

View(head(userRFM, 10))
userRFM$monetary
head(userRFM, 10)

# Recency 상위 20%가 차지하는 총 매출액 대비 비중 ##################################
## 의미 : "??(날짜형 변환)" 이후 방문한 고객들의 지출이 총 매출액에 차지하는 비율
quantR[5] 
# userRFM$monetary[userRFM$maxDate >= quantR[5]]
sum(userRFM$monetary[userRFM$maxDate >= quantR[5]])
sum(userRFM$monetary[userRFM$maxDate >= quantR[5]]) / sum(userRFM$monetary) 
# 33.66%

as.Date(quantile(userRFM$recency, 0.8), origin="1970-01-01")
sumR <- sum(as.numeric(userRFM$monetary[userRFM$recency >= quantile(userRFM$recency, 0.8)]))
sumR/sum(as.numeric(userRFM$monetary)) 
# 33.66%

quantF[5] # 2
sumF <- sum(userRFM$monetary[userRFM$frequency >= quantF[5]])
sumF # 72,135,883,229 : 약 720억
sumF / sum(userRFM$monetary) # 67.95%

## 혹은 
#sumF <- sum(as.numeric(userRFM$monetary[userRFM$frequency >= quantile(userRFM$frequency, 0.8)]))
#sumF 
#sumF/sum(as.numeric(userRFM$monetary)) 

# Monetary 상위 20%가 차지하는 총 매출액 대비 비중 ##################################
## 의미 : 103,000원 이상 구매한 고객들의 지출이 총 매출액에 차지하는 비율 #########
quantM[5] # 103,000
# userRFM$monetary[userRFM$monetary >= quantM[5]]

sumM <- sum(userRFM$monetary[userRFM$monetary >= quantM[5]])
sumM #  60,525,240,356 : 약 605억
sumM / sum(userRFM$monetary) # 57.01%

## 혹은 
#sumM <- sum(as.numeric(userRFM$monetary[userRFM$monetary >= quantile(userRFM$monetary, 0.8)]))
#sumM
#sumM/sum(as.numeric(userRFM$monetary)) 

# 가중치 계산
# (RFM지수 = weightR * Recency + weightF * Frequency + weightM * Monetary)
weightR <- sumR/(sumR + sumF + sumM)
weightF <- sumF/(sumR + sumF + sumM)
weightM <- sumM/(sumR + sumF + sumM)

weightR # 0.2825612
weightF # 0.2388452
weightM # 0.4785935

# parse 함수 활용
columnName <- paste0("userRFM", "$", "frequency")
head(columnName)
eval(parse(text=columnName))[2] 
# 문자열 조합으로 데이터프레임의 열을 찾는 방법
head(userRFM$frequency)


## 등급 부여하는 함수 제작 (data.table로 속도 향상)

intervalGrade <- function(mainData, fileName, rfmName, quantileData) {
  results <- rep (0, nrow(mainData))
  dataSource <- data.table(eval(parse(text=paste0(fileName,"$",rfmName))))
  for (i in 1:nrow(mainData)) {
    data = dataSource$V1[i]
    if (data >= quantileData[1] && data < quantileData[2] ) {
      results[i] <- 1
    } else if (data >= quantileData[2] && data < quantileData[3]) {
      results[i] <- 2
    } else if (data >= quantileData[3] && data < quantileData[4]) {
      results[i] <- 3
    } else if (data >= quantileData[4] && data < quantileData[5]) {
      results[i] <- 4
    } else { 
      results[i] <- 5 
    }
  }
  return(results)
}

# 메모리를 위해 기존 작업한 RData 파일 삭제
# rm(cust_prod_total_fix_2)
# rm(userRFM_uniq)
gc()

head(userRFM)
glimpse(userRFM)
c
# R/F/M 값 계산
userRFM$R <- intervalGrade(userRFM, "userRFM", "maxDate", quantR) # 날짜 -> 숫자형 
userRFM$F <- intervalGrade(userRFM, "userRFM", "frequency", quantF)
userRFM$M <- intervalGrade(userRFM, "userRFM", "monetary", quantM)

# system.time({userRFM$M <- intervalGrade(userRFM, "userRFM", "monetary", quantM)})
#    사용자  시스템 elapsed 
#   1831.59    0.95 1857.02 
# ->  23.63    0.08   23.77 

# RFM 지수 = weightR * Recency + weightF * Frequency + weightM * Monetary
userRFM$score <- (weightR * userRFM$R + weightF * userRFM$F + weightM * userRFM$M)*100/5

hist(userRFM$score)

table(userRFM$R)
table(userRFM$F)
table(userRFM$M)


glimpse(userRFM)
# dim(userRFM) # 1,344,345      12

# RFM 등급 부여
quantS <- quantile(userRFM$score, c(0, 0.2, 0.4, 0.6, 0.8, 1))
summary(userRFM$score)


# RFM에 의한 고객등급 분류 함수 제작 (data.table로 성능 향상)
finalGrade <- function(mainData, fileName, rfmName, quantileData) {
  results <- rep (0, nrow(mainData))
  dataSource <- data.table(eval(parse(text=paste0(fileName,"$",rfmName))))
  for (i in 1:nrow(mainData)) {
    data = dataSource$V1[i]
    if (data >= quantileData[1] && data < quantileData[2] ) {
      results[i] <- "E"
    } else if (data >= quantileData[2] && data < quantileData[3]) {
      results[i] <- "D"
    } else if (data >= quantileData[3] && data < quantileData[4]) {
      results[i] <- "C"
    } else if (data >= quantileData[4] && data < quantileData[5]) {
      results[i] <- "B"
    } else { results[i] <- "A" }
  }
  return(results)
}

# RFM 점수로 고객등급 분류
userRFM$grade <- finalGrade(userRFM, "userRFM", "score", quantS)
# system.time({userRFM$grade <- finalGrade(userRFM, "userRFM", "score", quantS)})
#  사용자  시스템 elapsed 
#   26.76    0.10   27.03 
#-> 30.02    0.11   30.40 

userRFM$grade <- as.factor(userRFM$grade)
glimpse(userRFM)
### 성별/ 연령/ 제품코드는 이후에 table join으로 반영

userRFM$grade <- as.factor(userRFM$grade)
summary(userRFM$grade)
#     A      B      C      D      E 
# 281576 280404 253847 282545 245973 

head(userRFM)
glimpse(userRFM) # Observations: 1,344,345
summary(userRFM$minRecency)
summary(userRFM$recency)
summary(userRFM$period) # median : 0, mean : 116.7
summary(userRFM$minDate)
summary(userRFM$maxDate)
summary(userRFM$score)

#save(userRFM_uniq, file="userRFM_full_uniq.RData")

#### custid 수정 ->> 최종 고객 마스터 생성


# summary(userRFM$grade)
# #     A      B      C      D      E 
# # 281576 280404 253847 282545 245973 
# 
# head(userRFM)
# glimpse(userRFM) # Observations: 1,344,345
# summary(userRFM$minRecency)
# summary(userRFM$recency)
# summary(userRFM$period) # median : 0, mean : 116.7
# summary(userRFM$minDate)
# summary(userRFM$maxDate)
# summary(userRFM$score)

save(userRFM, file = 'userRFM.RData')


# #### custid 수정 ->> 최종 고객 마스터 생성
# 
# # 계정 숫자가 이상, 특수문자 포함, 실명 노출되는 문제 
# # cust_id를 임의의 단일값으로 변경하기 위해서 row_number를 이용해 값을 구하고 
# # 앞에 빈칸을 채우기 위해서 sprintf 를 사용하여 0으로 채움
# # 
# # https://stackoverflow.com/questions/11996135/create-a-sequential-number-counter-for-rows-within-each-group-of-a-dataframe
# # https://stackoverflow.com/questions/14409084/pad-with-leading-zeros-to-common-width
# userRFM_uniq <- userRFM %>%
#   dplyr::mutate(custid = sprintf("C%08d", row_number()))
# userRFM_uniq  
# glimpse(userRFM_uniq)
# 
# 
# ## 후속 작업용 파일 생성 
# 
# #1) csv (->> userRFM_file로 작업 진행)
# # write.csv(userRFM, file = "userRFM_full.csv", row.names=FALSE)
# # write.csv(userRFM_uniq, file = "userRFM_full_uniq.csv", row.names=FALSE)
# 
# # userRFM_file <- read.csv("userRFM_full_uniq.csv")
# # glimpse(userRFM_file)
# # View(userRFM_file)
# 
# 
# #2) RData (->> userRFM_uniq로 작업 진행)
# save(userRFM_uniq, file="userRFM_full_uniq.RData")
# rm(userRFM_uniq)
# # load("userRFM_full_uniq.RData")
# 
# # glimpse(userRFM_uniq)
# # View(userRFM_uniq)



