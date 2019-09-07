rm(list=ls()) 
gc()
getwd()
#memory size 
memory.limit(size=300000)



getwd()
#setwd("C:/Users/gonybella/Desktop/프로젝트 공유/Git Repository/rfm")
#setwd("C:/Users/USER/Desktop/고웁/2018-2/빅데이터 프로젝트/github_share/rfm")
setwd("D:/sales_anaysis/Sales-Analysis-Recommendations")

library(recommenderlab)
library(readxl)
library(plyr)

library(dplyr)
library(tidyverse)
library(reshape)
library(reshape2)
library(data.table)
library(ggplot2)
library(tidyverse)
library(data.table)
library(stringr)

#google drive 첨부.
load("cust_prod_total_fin.RData")

glimpse(cust_prod_total_fin)
cust_prod_total_fin$cate <- as.factor(cust_prod_total_fin$cate)
cust_prod_total_fin$cate_ftn <- as.factor(cust_prod_total_fin$cate_ftn) # 51개
cust_prod_total_fin$cate_line <- as.factor(cust_prod_total_fin$cate_line)
summary(cust_prod_total_fin$cate)
summary(cust_prod_total_fin$cate_ftn)
summary(cust_prod_total_fin$cate_line)

## lda를 위한 데이터 변환 
# 품목(상품목록), 쇼핑(고객/상품/횟수) 테이블 생성


######################################### 
##개선 전 코드 -> 130행으로 jump!########

cust_lda <- cust_prod_total_fin %>% 
  dplyr::select(custid, cate_ftn) %>%
  dplyr::group_by(custid, cate_ftn) %>%
  dplyr::summarise(buynum = n())
View(cust_lda)

#custid(고객)를 0 제외한 숫자만으로 추출
cust_lda$custid %>% 
  str_replace_all(c('C0000000' = '',
                    'C000000' = '', 
                    'C00000' = '', 
                    'C0000' = '', 
                    'C000' = '', 
                    'C00' = '', 
                    'C0' = '', 
                    'C' = ''
  )) -> cust_lda$custid 

cust_lda$custid <- as.integer(cust_lda$custid)
View(cust_lda)  
glimpse(cust_lda) # 고객id가 숫자만 추출 

#cate_ftn(상품)을 숫자로 변경
## 품목 테이블 생성하여 엑셀에서 txt 파일로 변경
cate_ftn <- data.frame(cate = c('Accessory_Bag', 'Accessory_Body', 'Accessory_Face', 'Accessory_Hair', 'Accessory_Hand/Foot', 
                                'Accessory_Makeup', 'Accessory_Skin', 'Body_Butter', 'Body_Cleanser', 'Body_Deodorant',  
                                'Body_Lotion', 'Body_Mist', 'Body_Oil', 'Body_Scrub&Mask', 'Body_Set', 
                                'Body_Soap', 'Body_Travelkit', 'ETC', 'Foot', 'Foot_Set', 
                                'Fragrance_EDT', 'Fragrance_Home', 'Fragrance_Set', 'Gift', 'Hair_Conditioner', 
                                'Hair_Scrub&Mask', 'Hair_Serum&Wax', 'Hair_Set', 'Hair_Shampoo', 'Hand', 
                                'Hand_Set', 'Makeup_Cheek', 'Makeup_Eye', 'Makeup_Face', 'Makeup_Lipcare', 
                                'Makeup_Lipstic', 'Makeup_Nail', 'Makeup_Set', 'Sampling', 'Skin_Cleanser', 
                                'Skin_Eyecream', 'Skin_Lipcare', 'Skin_Men', 'Skin_Mist', 'Skin_Moisturize', 
                                'Skin_Oil', 'Skin_Scrub&Mask', 'Skin_Set', 'Skin_SPF', 'Skin_Toner', 
                                'Skin_Travelkit'))

# 팩터는 숫자로 변경이 안됨
cust_lda$cate_ftn <- as.character(cust_lda$cate_ftn) 

cust_lda$cate_ftn[cust_lda$cate_ftn == 'Accessory_Bag'] <- 1
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Accessory_Body'] <- 2
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Accessory_Face'] <- 3
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Accessory_Hair'] <- 4
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Accessory_Hand/Foot'] <- 5 
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Accessory_Makeup'] <- 6
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Accessory_Skin'] <- 7
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Body_Butter'] <- 8
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Body_Cleanser'] <- 9
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Body_Deodorant'] <- 10 
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Body_Lotion'] <- 11
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Body_Mist'] <- 12
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Body_Oil'] <- 13
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Body_Scrub&Mask'] <- 14
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Body_Set'] <- 15
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Body_Soap'] <- 16
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Body_Travelkit'] <- 17 
cust_lda$cate_ftn[cust_lda$cate_ftn == 'ETC'] <- 18
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Foot'] <- 19
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Foot_Set'] <- 20
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Fragrance_EDT'] <- 21
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Fragrance_Home'] <- 22
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Fragrance_Set'] <- 23 
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Gift'] <- 24
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Hair_Conditioner'] <- 25 
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Hair_Scrub&Mask'] <- 26
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Hair_Serum&Wax'] <- 27
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Hair_Set'] <- 28
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Hair_Shampoo'] <- 29
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Hand'] <- 30
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Hand_Set'] <- 31
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Makeup_Cheek'] <- 32
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Makeup_Eye'] <- 33
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Makeup_Face'] <- 34
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Makeup_Lipcare'] <- 35
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Makeup_Lipstic'] <- 36
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Makeup_Nail'] <- 37
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Makeup_Set'] <- 38
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Sampling'] <- 39
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Skin_Cleanser'] <- 40
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Skin_Eyecream'] <- 41
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Skin_Lipcare'] <- 42 
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Skin_Men'] <- 43
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Skin_Mist'] <- 44
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Skin_Moisturize'] <- 45 
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Skin_Oil'] <- 46
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Skin_Scrub&Mask'] <- 47 
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Skin_Set'] <- 48
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Skin_SPF'] <- 49
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Skin_Toner'] <- 50
cust_lda$cate_ftn[cust_lda$cate_ftn == 'Skin_Travelkit'] <- 51

cust_lda$cate_ftn <- as.integer(cust_lda$cate_ftn)
#range(cust_lda$cate_ftn)
summary(cust_lda$cate_ftn)
#View(cust_lda)  
glimpse(cust_lda)

# write.csv(cust_lda, 'shopping.csv')
# shop1 <- read.csv('shopping.csv') # 쇼핑.csv의 자료 형태 
# glimpse(shop1) 
# shop1 <- shop1[, -1] # 불필요한 행번호 X 컬럼 삭제
# glimpse(shop1)

########################################
## 2) 개선된 코드 

cust_lda <- cust_prod_total_fin %>% 
  dplyr::select(custid, cate_ftn) %>%
  dplyr::group_by(custid, cate_ftn) %>%
  dplyr::summarise(buynum = n())

# cate_ftn과 custid 를 vector를 이용해서 값 변경
# https://stackoverflow.com/questions/3922461/extract-column-from-data-frame-as-a-vector
head(cust_lda$cate_ftn)
cate_ftn <- as.vector(cust_lda$cate_ftn)
head(cate_ftn)
cate_ftn.v <- cust_lda$cate_ftn
# cat(cate_ftn.v) 
# cat(cate_ftn)
cust_lda$cate_ftn <- as.integer(cate_ftn.v) # 알파벳순 숫자 부여 

cust_lda$custid <- as.factor(cust_lda$custid)
custid <- as.vector(cust_lda$custid)
custid.v <- cust_lda$custid # 순서대로 숫자 부여 
# cat(custid.v)
# cat(custid)
cust_lda$custid <- as.integer(custid.v)
glimpse(cust_lda)

# 저장안하고 진행해보기
shop <- cust_lda
# shop <- shop[, -1] # cust_lda 에서 바로 가면 앞에 인덱스 행이 없기 때문에 할 필요 없다. csv에서 읽으면 필요

glimpse(shop) # 개선된 방식
# glimpse(shop1) # 기존 방식 
# all(shop == shop1) # 동일!!
# all.equal(shop, shop1) # 같은 정도 계산 


## “lda” 패키지를 사용하기 위해 “shopping.csv”를 
## “lda” 패키지에서 요구하는 형태인 “쇼핑.dat”로 변형 필요 

## 중첩 for loop 개선 

# mj <- table(shop[, 1])
# outfile <- file('shopping.dat')
# line = c(); k = 1
# for (j in 1:length(mj)) {
#   line = paste0(line, mj[j])
#   for (i in k:(k+mj[j]-1)) {
#     line = paste0(line, ' ', shop[i,2]-1, ':', shop[i,3])
#   }
#   line = paste0(line, '\n')
#   k = k + mj[j]
# }
# writeLines(line, outfile)
# close(outfile)

mj <- table(shop[, 1])
mj

## data.table로 변환
mj <- as.data.table(mj)
mj
shop <- as.data.table(shop)
shop

# https://stackoverflow.com/questions/38514988/concatenate-strings-by-group-with-dplyr
# https://stackoverflow.com/questions/42288757/concatenate-strings-by-group-with-dplyr-for-multiple-columns

system.time({shopping.dat <- shop %>% 
  group_by(custid) %>% 
  dplyr::summarize(outputs = paste(buy = n(), paste0(cate_ftn-1, ':', buynum, collapse = " "))) %>%
  dplyr::select(outputs)})
# 사용자  시스템 elapsed 
# 152.86    0.27  157.05 
#  44.56    0.03   44.82 !!
shopping.dat
View(shopping.dat)

## 개선전 (382만건 * 382만건 = 14조 5천억번 연산)
# for (j in 1:length(mj)) {
#   line = paste0(line, mj[j])
#   for (i in k:(k+mj[j]-1)) {
#     line = paste0(line, ' ', shop[i,2]-1, ':', shop[i,3])
#   }
#   line = paste0(line, '\n')
#   k = k + mj[j]
# }

# https://stackoverflow.com/questions/10608526/writing-a-matrix-to-a-file-without-a-header-and-row-numbers
# 저장할 때 따옴표(quote) 제거, 컬럼 이름(outputs)  제거, 행번호 제거
# write.table(shopping.dat, 'shopping.dat', 
#            quote = FALSE, col.names=FALSE, row.names = FALSE)

#lda package 설치.
# install.packages('lda')
library(lda)


shop2 <- read.documents('shopping.dat')
item_name <- read.vocab('item.txt')   # 사전에 엑셀로 제작 
# View(item_name)

#http://www.imsbio.co.jp/RGM/R_rdfile?f=lda/man/lda.collapsed.gibbs.sampler.Rd&d=R_CC

# 토픽 수를 20개에서 하나씩 줄여서 반복시행
# 데이터가 명확하고 비즈니스적으로도 유의미한 갯수로 5 결정 
system.time(lda <- lda.collapsed.gibbs.sampler(documents = shop2, 
                                               K = 5,  ## 토픽수를 5개로 결정 
                                               vocab = item_name, 
                                               num.iterations = 500, 
                                               alpha = 1, 
                                               eta = 1, 
                                               compute.log.likelihood = T))

# 사용자  시스템 elapsed 
#1360.67    1.23 1364.97 # 10
#1455.26    1.50 1460.05 # 11
#1860.58    3.22 1871.84 # 15
# 739.99    0.81  742.38 # 9
#1201.05    1.66 1204.00 # 8
#1074.50    1.19 1077.17 # 7
save(lda, file="lda_num5.RData")
load('lda5.RData')
glimpse(lda)
## 로그 우도 그래프 
library(ggplot2)
iter <- 500
plot <- data.frame(iteration = c(1:iter, 1:iter),
                   loglike = c(lda$log.likelihoods[1,], lda$log.likelihoods[2,]),
                   type = c(rep(1, iter), rep(2, iter)))
ggplot(plot, aes(x = iteration, y = loglike, group = type)) +
  geom_line()


str(lda)

###
library(reshape2)
theme_set(theme_bw())

# 토픽 
n = 1344345; K = 5; W = 51 # 고객 1344345명, 토픽 5, 품목 51개
theta <- matrix(0, nrow = n, ncol = K)
for(i in 1:n) {
  theta[i,] = t(lda$document_sums[,i]) / sum(lda$documnt_sums[,i])}
phi <- matrix(0, nrow = K, ncol = W)
colnames(phi) <- item_name
for(i in 1:K) {
  phi[i,] = lda$topics[i,] / sum(lda$topics[i,])}
idx <- 1:K
phi.df <- melt(cbind(data.frame(phi[idx,]), topic = factor(idx)), 
               variable.name = 'item', id.vars = 'topic')
glimpse(phi.df) 
# qplot(item, value, fill=item, ylab='value', data=phi.df, geom='bar', stat='identity') +
#   theme(axis.text.x = element_text(angle=90, hjust=1), legend.position='none') +
#   coord_flip() + facet_wrap(~topic, ncol=length(idx))
ggplot(data = phi.df, aes(x = item, y = value, fill = item), # qplot 에러 -> ggplot으로 코드 수정
       xlab = 'item', ylab= 'value') +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle=90, hjust=1), legend.position = 'none') +
  coord_flip() + facet_wrap( ~ topic, ncol = length(idx))


################################################
# 토픽으로 뻔해서
# 리프트 (김용대 교수 첨부된 pdf 논문 참조.) 
K = 5; W = 51
p <- colSums(lda$topics) / sum(lda$topics)
lift <- matrix(0, nrow = K, ncol = W)
colnames(lift) <- item_name 
topic_name = c()
for(i in 1:K) {
  lift[i,] = phi[i,] / p
  sorted = sort(lift[i,], decreasing=T)[1:2]
  topic_name = c(topic_name, paste(names(sorted), collapse="."))
}
phi.df.lift <- melt(cbind(data.frame(lift[idx,]), topic = factor(idx)), 
                    variable.name = 'item', id.vars = 'topic')
glimpse(phi.df.lift)
ggplot(data = phi.df.lift, aes(x = item, y = value, fill = item), 
       xlab = 'item', ylab = 'value') +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle=90, hjust=1), legend.position = 'none') +
  coord_flip() + facet_wrap( ~ topic, ncol = length(idx))


##custid 적용.
theme_set(theme_bw())
#colnames(theta) <-  char_name
idx <- c(1, 2, 3)
theta.df <- melt(cbind(data.frame(theta[idx, ]),
                       client = factor(idx)), varible.name = 'topic', id.vars = 'client')


theta.df

#cust id 재추출.
cust_ftn_list <- cust_prod_total_fin %>% 
  group_by(custid, cate_ftn) %>%
  summarise(buynum = n()) ;head(cust_ftn_list,20)

cust_id_list <- sort(unique(cust_ftn_list$custid))
cust_id_list_length <- length(cust_id_list)
head(cust_id_list)

cust_lda_list <- cbind(custid = cust_id_list[1:cust_id_list_length], data.frame(theta))
head(cust_lda_list,10)

cust_lda_list_dt <- as.data.table(cust_lda_list)
setindex(cust_lda_list_dt, custid)

lda_num_list <- data.frame()


#lda 분석에 따른 고객 분류.
system.time(for (i in 1:cust_id_list_length) {
  tmp_class_num = 0;
  tmp_cust <- cust_lda_list_dt[i];
  for (j in 2:6) {
    if(tmp_cust[1,j, with = F] == "Inf"){
      tmp_class_num = tmp_class_num + 2^(j-2);
    }
  }
  lda_num_list <- rbind(lda_num_list, data.frame(tmp_class_num))
})

head(lda_num_list)
nrow(lda_num_list)

cust_lda_list <- cbind(cust_lda_list, lda_group = lda_num_list)

head(cust_lda_list)
colnames(cust_lda_list) <- c("custid",  "X1",  "X2",  "X3",  "X4",  "X5", "lda_group")

save(cust_lda_list, file = "cust_lda_list.RData")
load("cust_lda_list.RData")

table(cust_lda_list$lda_group)
head(cust_lda_list)


glimpse(cust_prod_total_fin)

cust_prod_total_fin_lda <- merge(cust_prod_total_fin, cust_lda_list %>% select(custid, lda_group),by="custid", all=FALSE)

cust_prod_total_fin_lda_sum <- cust_prod_total_fin_lda %>%
  group_by(lda_group) %>%
  summarize(lda_price_sum = sum(price))

head(cust_prod_total_fin_lda_sum,31)
cust_prod_total_fin_lda_sum[21:31,]


#분류 넘버링.
# X1  X2  X3  X4  X5 lda_group
#Inf NaN NaN NaN NaN         1
#NaN Inf NaN NaN NaN         2
#Inf Inf NaN NaN NaN         3
#NaN NaN Inf NaN NaN         4
#Inf NaN Inf NaN NaN         5
#NaN Inf Inf NaN NaN         6
#Inf Inf Inf NaN NaN         7
#NaN NaN NaN Inf NaN         8
#Inf NaN NaN Inf NaN         9
#NaN Inf NaN Inf NaN        10
#Inf Inf NaN Inf NaN        11
#NaN NaN Inf Inf NaN        12
#Inf NaN Inf Inf NaN        13
#NaN Inf Inf Inf NaN        14
#Inf Inf Inf Inf NaN        15
#NaN NaN NaN NaN Inf        16
#Inf NaN NaN NaN Inf        17
#NaN Inf NaN NaN Inf        18
#Inf Inf NaN NaN Inf        19
#NaN NaN Inf NaN Inf        20
#Inf NaN Inf NaN Inf        21
#NaN Inf Inf NaN Inf        22
#Inf Inf Inf NaN Inf        23
#NaN NaN NaN Inf Inf        24
#Inf NaN NaN Inf Inf        25
#NaN Inf NaN Inf Inf        26
#Inf Inf NaN Inf Inf        27
#NaN NaN Inf Inf Inf        28
#Inf NaN Inf Inf Inf        29
#NaN Inf Inf Inf Inf        30
#Inf Inf Inf Inf Inf        31


