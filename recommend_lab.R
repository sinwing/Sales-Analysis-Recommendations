
getwd()
#setwd("C:/Users/gonybella/Desktop/프로젝트 공유/Git Repository/rfm")
#setwd("C:/Users/USER/Desktop/고웁/2018-2/빅데이터 프로젝트/github_share/rfm")

#install.packages("recommenderlab")

library(recommenderlab)
library(readxl)
library(plyr)
library(tidyverse)
library(reshape)
library(reshape2)
library(data.table)
library(ggplot2)

setwd("D:/sales_anaysis/Sales-Analysis-Recommendations")


gc()
memory.limit(size=300000)
rm(list = ls())

#추천 모델 평가 개선
#1514732400 은 2018년 1월 1일 숫자형.
#1514732400 == 2018-01-01


#head(userRFM)
#head(userRFM_uniq)

#회원구매 기본정보 호출, cust_prod_total_fix_2
#load('cust_prod_total.RData')
#head(cust_prod_total_fix_2)
#save(cust_prod_total_fin, file = 'cust_prod_total_fin.RData')
load('cust_prod_total_fin.RData')
head(cust_prod_total_fin)



#회원rfm 정보.
load("userRFM.RData")
head(userRFM)


## custid 매핑을 위한 key 테이블 호출 
#userRFM_custid_key <- read.csv("userRFM_custid_key.csv")

# join 후 custid 를 변경
#cust_prod_total_fin <- left_join(cust_prod_total_fix_2, userRFM_custid_key, by="custid")


head(cust_prod_total_fin)
glimpse(cust_prod_total_fin)


#date 를 숫자 타입으로 변경.
cust_prod_total_fin <- cust_prod_total_fin %>% 
  mutate(date_num = as.numeric(date))

distinct(cust_prod_total_fin %>%
           filter(date_num >= 1514732400) %>%
           select(custid_no)) -> sample_custid_after

distinct(cust_prod_total_fin %>%
           filter(date_num < 1514732400) %>%
           select(custid_no)) -> sample_custid_before


head(sample_custid_before)
nrow(sample_custid_before)
#1196335
head(sample_custid_after)
nrow(sample_custid_after)
#295969

#2018년 이전과 이후  공통적으로 구매한 사람들 구하기.
training_test_custid <- merge(sample_custid_after, sample_custid_before, by = "custid_no", all = FALSE)

head(training_test_custid)
nrow(training_test_custid)
#148366
colnames(training_test_custid) <- c("custid", "1", "2")

#training, test 에 맞게 melt 데이터 생성
##추천 로직에 기준이되는 데이터 추출, RFM 모두 5점.
royal_custid <- userRFM_uniq %>%
  filter(F > 4 ) %>%
  filter(M > 4 ) %>%
  filter(R > 4 ) %>%
  select(custid)

nrow(royal_custid)

#2018 이전 이후 구매 자 중 로얄 고객 추출.
training_test_custid_1 <- merge(training_test_custid, royal_custid, by = "custid", all = FALSE)
nrow(training_test_custid_1)
head(training_test_custid_1)

#연산속도 향상을 위하여 data table 변경 후 custid를 인덱스로 변경.
cust_prod_total_fin_dt <- as.data.table(cust_prod_total_fin)
training_test_custid_1_dt <- as.data.table(training_test_custid_1)

setindex(cust_prod_total_fin_dt, custid_no) 
setindex(training_test_custid_1_dt, custid) 

#known, unknown 데이터 구분, 2018 이전은 known 이후는 unknown
cust_training_set <- cust_prod_total_fin_dt %>%
  filter(date_num < 1514732400 ) %>%
  filter(custid_no %in% training_test_custid_1_dt$custid)

cust_test_set <- cust_prod_total_fin_dt %>%
  filter(date_num >= 1514732400 ) %>%
  filter(custid_no %in% training_test_custid_1_dt$custid)

nrow(cust_training_set)
head(cust_training_set)
nrow(cust_test_set)
head(cust_test_set)


#known , unknown의 melt 데이터 추출.
cust_training_set_melt <- cust_training_set %>%  
  group_by(custid_no, prod_nm) %>%
  summarise(frequency = n())

cust_test_set_melt <- cust_test_set %>%  
  group_by(custid_no, prod_nm) %>%
  summarise(frequency = n())

head(cust_training_set_melt)
head(cust_test_set_melt)

colnames(cust_training_set_melt) <- c("custid", "prod_nm","frequency")
colnames(cust_test_set_melt) <- c("custid", "prod_nm","frequency")


#write.csv(unique(cust_prod_total_fin_dt$prod_nm), "dummy_melt_prod.csv")

#전체 컬럼이 일정해야 하기에 item 컬럼 더미 데이터 입력.
dummy_cd_base_info_1_melt <- read_excel("dummy_melt_prod.xlsx")

head(dummy_cd_base_info_1_melt)
dummy_cd_base_info_1_melt <- as.data.table(dummy_cd_base_info_1_melt)

cust_training_set_melt_1 <- rbind(as.data.table(cust_training_set_melt), dummy_cd_base_info_1_melt)
cust_test_set_melt_1 <- rbind(as.data.table(cust_test_set_melt), dummy_cd_base_info_1_melt)

head(cust_training_set_melt_1)
head(cust_test_set_melt_1)

##realRatinMatrix 변경을 위하여 dcast 형으로 변경.
cust_traning_set_dcast <- dcast(cust_training_set_melt_1, custid ~ prod_nm, value.var="frequency")
cust_test_set_dcast <- dcast(cust_test_set_melt_1, custid ~ prod_nm, value.var="frequency")

nrow(cust_traning_set_dcast)
NCOL(cust_traning_set_dcast)
nrow(cust_test_set_dcast)
NCOL(cust_test_set_dcast)

cust_traning_set_matrix <- as.matrix(cust_traning_set_dcast)
cust_test_set_matrix <- as.matrix(cust_test_set_dcast)


###realRatingMatrix생성.
cust_traning_set_rrm <- as(cust_traning_set_matrix[1:3000,2:ncol(cust_traning_set_dcast)], "realRatingMatrix")
cust_test_set_rrm <- as(cust_test_set_matrix[1:3000,2:ncol(cust_test_set_dcast)], "realRatingMatrix")
cust_traning_set_rrm
cust_test_set_rrm

#save(cust_traning_set_rrm, cust_test_set_rrm, file = "cust_training_test_rrm.RData")
#load("cust_training_test_rrm.RData")

#고객id 추출을 위한 전체 데이터 melt 변경.
cust_prod_fre_melt <- cust_prod_total_fin %>%  
  group_by(custid, prod_nm) %>%
  summarise(frequency = n())

colnames(cust_prod_fre_melt) <- c("custid", "prod_nm","frequency")
head(cust_prod_fre_melt,30)
nrow(cust_prod_fre_melt)
tail(cust_prod_fre_melt,30)
cust_prod_fre_melt[10:25,]


head(userRFM_uniq)
###전체 모델 수정.


cf_base_info_fix <- rbind(cf_base_info,dummy_cd_base_info_1_melt)
head(cf_base_info_fix)
head(dummy_cd_base_info_1_melt)
##dcast 좀 오래 걸림.
cf_base_info_1_dcast_fix <- dcast(cf_base_info_fix, custid ~ prod_nm, value.var="frequency")
nrow(cf_base_info_1_dcast_fix) ##125285
dim(cf_base_info_1_dcast_fix)
ncol(cf_base_info_fix)
cf_base_info_1_dcast_fix[125718,1]


##matrix,realRatingMatrix  형태로 변환, 오래 거림.
cf_base_info_1_matrix_fix <- as.matrix(cf_base_info_1_dcast_fix)
dim(cf_base_info_1_matrix_fix)
#1929 ##중요, prod_code와 prod_nm 수가 다름, 아마 제품명이 같고 prod_code는 다름 품목 있는듯.
##첫번째(고객 id) 컬럼을 제외 하고 realRatingMatrix 생성.
cf_base_info_1_rrm_fix <- as(cf_base_info_1_matrix_fix[1:125717,2:2071], "realRatingMatrix")

#object.size(cf_base_info_1_matrix) #1,948,275,856 bytes
#object.size(cf_base_info_1_rrm) #17,078,864 bytes

#save(cf_base_info_1_rrm_fix, file = "cf_base_info_1_rrm_fix.RData")
load("cf_base_info_1_rrm_fix.RData")

cf_base_info_1_rrm_fix
#125717 x 2070 rating matrix of class ‘realRatingMatrix’ with 1408737 ratings.

#save(cf_base_info_1_rrm, file = "cf_base_info_1_rrm.Rdata")
load("cf_base_info_1_rrm.Rdata")


cf_base_info_2_rrm_fix <- cf_base_info_1_rrm_fix

cf_base_info_2_rrm_fix <- cf_base_info_2_rrm_fix[rowCounts(cf_base_info_2_rrm_fix) > 10,]
cf_base_info_2_rrm_fix <- cf_base_info_2_rrm_fix[rowCounts(cf_base_info_2_rrm_fix) < 13,]

#cf_base_info_2_rrm_fix <- cf_base_info_2_rrm_fix[,colCounts(cf_base_info_1_rrm_fix) > 10]
#cf_base_info_2_rrm_fix <- cf_base_info_2_rrm_fix[,colCounts(cf_base_info_1_rrm_fix) < 50000]

cf_base_info_2_rrm_fix

cf_base_model_ub_cos <- Recommender(train_rrm,method="UBCF", parameter="Cosine")
cf_base_model_ub_pear <- Recommender(train_rrm,method="UBCF", parameter="Pearson")

cf_base_model_Ib_cos <- Recommender(train_rrm,method="IBCF", parameter="Cosine")
cf_base_model_Ib_pear <- Recommender(train_rrm,method="IBCF", parameter="Pearson")

cf_base_model_ub_jac <- Recommender(train_rrm_bin,method="UBCF", parameter="Jaccard")
cf_base_model_ib_jac <- Recommender(train_rrm_bin,method="IBCF", parameter="Jaccard")





##평가 적용.

eval_prediction <- predict(object = cf_base_model_ub_cos, 
                           newdata = cust_traning_set_rrm,
                           n = 3, type = "ratings")
class(eval_prediction)

eval_accracy_ub <- calcPredictionAccuracy(x = eval_prediction,
                                          data = cust_test_set_rrm,
                                          byUser = TRUE)
avg(eval_accracy_ub)

##구매자의 참고 아이템
item_to_keep <- 8
##구매 인정 갯수 ex) 아래와 같은 경우 1개 이상구매 시 인정된다는 뜻
rating_threshold <- 1
##훈련횟ㅅ
n_eval <- 3

eval_sets <- evaluationScheme(data = cf_base_info_1_rrm_eval, method = "cross-validation", train = 0.8, 
                              given = item_to_keep, goodRating = rating_threshold, k = n_eval)

?evaluationScheme

eval_recommander_ub <- Recommender(data = getData(eval_sets, "train"), method = "UBCF", parameter = "cosine")

class(a1)
class(a2)
##예측 평점 매트릭스 생성
eval_prediction <- predict(object = eval_recommander_ub, 
                           newdata = getData(eval_sets, "known"),
                           n = 3, type = "ratings")
class(eval_prediction)

eval_accracy_ub <- calcPredictionAccuracy(x = eval_prediction,
                                          data = getData(eval_sets, "unknown"),
                                          byUser = TRUE)








##melt data 와 병합.
head(cust_prod_fre_melt)



head(training_test_custid)