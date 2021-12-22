# install.packages("data.table")
setwd("C:/Users/doubley/Desktop/lpoint")
########################
#### 사용한 library ####
########################
# library(data.table)
# library(dplyr)
# library(plyr)
# library(pca3d)
# library(cluster)
# library(fpc)
# library(scales)
# library(tseries)
# library(forecast)
# library(tsoutliers)
# library(lmtest)
# library(tsDyn)
# library(vars)
# library(dyn)

#1 파일 불러오기 ####

library(data.table)
product <- fread("Pruduct.csv",data.table = FALSE, encoding = "UTF-8")
search1 <- fread("Search1.csv",data.table = FALSE, encoding = "UTF-8")
search2 <- fread("Search2.csv",data.table = FALSE, encoding = "UTF-8")
custom<- fread("Custom.csv",data.table = FALSE, encoding = "UTF-8")
session <- fread("Session.csv",data.table = FALSE, encoding = "UTF-8")
master <- fread("Master.csv",data.table = FALSE, encoding = "UTF-8")

#################################### 2 product dataset 전처리 ###########################

#2-1 hit_time = 1 NA 처리
# product_nrm <- product # 결측치 처리 전은 product_nrm으로 복사해둠
product <- subset(product, !product$HITS_SEQ == 1)
# nrow(product)   # 5019730 (5176 obs removed)
# rm(product_nrm)로 지울 수 있음


#2-2 chr 처리되어있는 PD_BUY_AM 및 PD_BUY_CT 변수 num로 변환 (천단위 , 제거)

product$PD_BUY_AM <- gsub(",","",product$PD_BUY_AM) # 가격변수 천 단위 , 제거로 NA 생성 방지
product$PD_BUY_CT <- gsub(",","",product$PD_BUY_CT) # 수량 천 단위 , 제거 c(29650,34106) 행 천개 이상 구매
product$PD_BUY_AM <- as.numeric(product$PD_BUY_AM)
product$PD_BUY_CT <- as.numeric(product$PD_BUY_CT)


#2-3 브랜드 이름의 [,],.,space 없애기

product$PD_BRA_NM <- gsub("[", "", product$PD_BRA_NM, fixed = T)
product$PD_BRA_NM <- gsub("]", "", product$PD_BRA_NM, fixed = T)
product$PD_BRA_NM <- gsub(" ", "", product$PD_BRA_NM, fixed = T)
product$PD_BRA_NM <- gsub(".", "", product$PD_BRA_NM, fixed = T)

#################################### 3 search1, search2 dataset 전처리 ###########################

str(search1)   #search1 - 상품을 구매한 방문자의 검색어만 포함한 데이터 / 전처리 별도로 안함 
str(search2)  # search2 - 상품을 구매하지 않은 방문자의 검색어 또한 포함한 데이터

# 3-1 SEARCH_CNT num으로 바꾸고 천단위 , 제거
search2$SEARCH_CNT <- gsub(",","",search2$SEARCH_CNT) # 해당 검색어 검색량 SEARCH_CNT 변수 천 단위 , 제거
search2$SEARCH_CNT <- as.numeric(search2$SEARCH_CNT)  

str(custom)
custom$CLNT_GENDER <- as.factor(custom$CLNT_GENDER) # 성별 변수

#################################### 4 session dataset 전처리 ###########################

str(session)


# 4-1 TOT_SESS_HR_V(세션 내 총 시간 (초 단위))  num으로 바꾸고 천단위 , 제거

session$TOT_SESS_HR_V <- gsub(",","",session$TOT_SESS_HR_V)
session$TOT_SESS_HR_V <- as.numeric(session$TOT_SESS_HR_V)


# 4-2 DVC_CTG_NM (기기유형) mobile = 1, desktop = 2, tablet = 3 으로 할당 및 팩터화

unique(session$DVC_CTG_NM)  # 기기 유형 -  mobile, desktop, tablet
session$DVC_CTG_NM<-gsub("mobile",1,session$DVC_CTG_NM) # mobile == 1
session$DVC_CTG_NM<-gsub("desktop",2,session$DVC_CTG_NM) # desktop == 2
session$DVC_CTG_NM<-gsub("tablet",3,session$DVC_CTG_NM) # tablet ==3
session$DVC_CTG_NM<-as.factor(session$DVC_CTG_NM)
# 4-3 NA 제거
session <- session[!(is.na(session$TOT_SESS_HR_V)|is.na(session$TOT_PAG_VIEW_CT)),]

# (보류) "데스크탑 인터넷 익스플로러에서 모바일 버전으로 접속"하는 경우, 
# 한 CLNT_ID에 대해 기기 유형(DVC_CTG_NM)에 Desktop과 Mobile 모두 나타날 수 있다 /

#CLNT_count_DVC <- plyr::count(session, c("CLNT_ID", "DVC_CTG_NM"))
#head(CLNT_count_DVC[order(CLNT_count_DVC$freq, decreasing=T),])
#str(session)

#library(plyr)
# session$DVC_CTG_NM<-as.numeric(session$DVC_CTG_NM)
# CLNT_count_DVC <- subset(session, DVC_CTG_NM %in% c(1,2))
# str(CLNT_count_DVC)

# CLNT_count_DV <- aggregate(session$DVC_CTG_NM, by = list(session$CLNT_ID), unique)
# CLNT_count_DV <- ddply(session, .(CLNT_ID), summarise, diff = max(DVC_CTG_NM) - min(DVC_CTG_NM))
# str(CLNT_count_DV)
# CLNT_count_DV[CLNT_count_DV$mobile - CLNT_count_DV$desktop == 0]

# 4-3 (보류) ZON_NM, CITY_NM 지역정보
# unique(session$ZON_NM)
# unique(session$CITY_NM)
# head(session)

#################################### 5 master dataset 전처리 ###########################
#str(master)  # CLNT_ID 없음

# 5-1 (보류) 대분류 및 중분류 팩터화 방법
# with(master, unique(CLAC1_NM))
# with(master, unique(CLAC2_NM))
master$CLAC2_NM <- gsub(" ", "", master$CLAC2_NM , fixed = T)
master$CLAC3_NM <- gsub(" ", "", master$CLAC3_NM , fixed = T)

#################################### 6 outlier  ######### 
library(dplyr)
library(plyr)

pro_master <- dplyr::inner_join(product, master, by=c("PD_C"))
pro_master_raw <- pro_master   # copy raw dataframe

`%out%` <- function(a,b) ! a %in% b

#### 1 가구 (43357 / 43398) ####
furniture <- pro_master[pro_master$CLAC1_NM=="가구", ]
furniture.1 <- furniture %>%
  group_by(PD_C) %>%
  dplyr::mutate(count = n_distinct(PD_BUY_AM), diff = max(PD_BUY_AM)/min(PD_BUY_AM)) %>%
  filter(count > 1) %>%
  filter(diff > 1000)
# str(furniture.1)
# multiply2 <- furniture.1$PD_C
# unlist(furniture.1[furniture.1$PD_C %in% multiply2,7]) # 2672원.
# price max value = 9370000이니까 9370원 까지의 이상한 값이 있나 확인.
multiply2 <- c(furniture.1$PD_C, 5510,  5786, 821996, 841274, 823994,
               713248, 641228, 7008, 843335, 578446, 841154, 676968, 73610, 776787,
               627655, 832198, 833496, 832194, 827519, 783528, 846130, 486077, 5776, 821603,
               726736, 791311, 743102, 628519, 780000, 706786, 800461, 593517, 846423, 658371,
               455237, 455237, 830645, 842210, 9349, 502287, 770761, 836900, 827488, 5777,
               580912, 713321, 827489, 827489, 637113, 637113, 570253, 814042, 552196, 552196,
               502269, 588291, 628165, 628165, 628157, 768007, 823654, 512717, 552142, 552142,757296)
# 1000곱할 것 확인.
# furniture[furniture$PD_C %in% multiply2,c(4,6,7,9)]
# 1000 곱해주기.
# furniture$PD_BUY_AM <- ifelse(furniture$PD_C %in% multiply2, 
# ifelse(furniture$PD_BUY_AM < 4700, furniture$PD_BUY_AM * 1000, furniture$PD_BUY_AM)
# , furniture$PD_BUY_AM)
# c(641514,769124,88147) 상품권들 PD_C
# furniture <- furniture[furniture$PD_C %out% c(641514,769124,88147),]
product$PD_BUY_AM <- ifelse(product$PD_C %in% multiply2, 
                            ifelse(product$PD_BUY_AM < 2700, product$PD_BUY_AM * 1000, product$PD_BUY_AM)
                            , product$PD_BUY_AM)
product <- product[product$PD_C %out% c(641514,769124,88147),]
rm(furniture)
rm(furniture.1)
rm(multiply2)

#### 2 건강식품 #####
health.num <- c(785657,467168)
product$PD_BUY_AM <- ifelse(product$PD_C %in% health.num, 
                            ifelse(product$PD_BUY_AM < 500, product$PD_BUY_AM * 1000, product$PD_BUY_AM)
                            , product$PD_BUY_AM)

#to remove
health.rm <- c(1982,7066,718132,755403)
product <- product[!(product$PD_C %in% health.rm & product$PD_BUY_AM < 1000),]

#### 3 계절가전  #####
seasonapp.num <- c(329859,360885,411409,414816,444470,444487,450776,454369,495120,495148,495183,495225,503083,503089,513314
                   ,533244,533505,533546,533751,634397,714498,398555,493106,471612,503090,375000,550745,420369,420369,550799)
product$PD_BUY_AM <- ifelse(product$PD_C %in% seasonapp.num, 
                            ifelse(product$PD_BUY_AM < 5000, product$PD_BUY_AM * 1000, product$PD_BUY_AM)
                            , product$PD_BUY_AM)
#### 5 구기/필드스포츠 ######
ball.num <- c(818809,295079,513116,526980)
product$PD_BUY_AM <- ifelse(product$PD_C %in%ball.num, 
                            ifelse(product$PD_BUY_AM < 3000, product$PD_BUY_AM * 1000, product$PD_BUY_AM)
                            , product$PD_BUY_AM)
##### 8 냉장/세탁가전 ####
mach.num <- c(307446,351348,372452,372452,414114,414115,414142,414356,498936,500959,539441,554283,554711,554714,566371,567216,567219,586722,567992
              ,586826,601925,624521,626261,637766,637897,639496,639498,640348,641220,644381,646247,661000,661004,661266,665570,665787,669994,675118
              ,675186,675728,687356,749656,749660,756592,770169,771357,776642,791570,792550,813624)
product$PD_BUY_AM <- ifelse(product$PD_C %in% mach.num, 
                            ifelse(product$PD_BUY_AM < 5000, product$PD_BUY_AM * 1000, product$PD_BUY_AM)
                            , product$PD_BUY_AM)
#remvoe 744545
product <- product[!(product$PD_C==744545 & product$PD_BUY_AM < 5000),]


#### 10 모바일 #####
#multiply *1000 
mob.num <- c(505823,505867,511905,511935,511942,511965,562836,566030,590749,650603)
product$PD_BUY_AM <- ifelse(product$PD_C %in% mob.num, 
                            ifelse(product$PD_BUY_AM < 2000, product$PD_BUY_AM * 1000, product$PD_BUY_AM)
                            , product$PD_BUY_AM)
#price < 20 => remove
mob <- product[product$CLAC1_NM=="모바일", ] #49582
mob.low <- mob[mob$PD_BUY_AM < 20,]
product <- product[!(product$PD_C %in% mob.low$PD_C),]


#### 11 문구/사무용품 ####
mun.rm <-c(772231) 
product <- product[!(product$PD_C %in% mun.rm),]


#### 12 상품권          S  ####
# 토다이랑 롯데시네마 티켓 금액 수정
sangpum <- pro_master[pro_master$CLAC1_NM=="상품권" , ]
sangpum.ordered <- sangpum[order(sangpum$PD_BUY_AM, decreasing=F), -c(1:3,5)]
multiply <- sangpum.ordered[1:20,1] 
product$PD_BUY_AM <- ifelse(product$PD_C %in% multiply, 
                            ifelse(product$PD_BUY_AM < 300, product$PD_BUY_AM * 1000, product$PD_BUY_AM)
                            , product$PD_BUY_AM)


#### 13 생활/주방가전   S  ####
# 생활/주방가전권 전체 boxplot
kitchen <- pro_master[pro_master$CLAC1_NM=="생활/주방가전" , ]
kitchen.ordered <- kitchen[order(kitchen$PD_BUY_AM, decreasing=F), -c(1:3,5,9)]
multiply <- kitchen.ordered[c(1:2,4:11,18,20,27:64,67:130), 1] # 여기까지 고가 가전. 
# product 바꾸기.

product$PD_BUY_AM <- ifelse(product$PD_C %in% multiply, 
                            ifelse(product$PD_BUY_AM < 10000, product$PD_BUY_AM * 1000, product$PD_BUY_AM)
                            , product$PD_BUY_AM)

#### 16 스포츠패션      S  #####
sport.f <- pro_master[pro_master$CLAC1_NM=="스포츠패션" , ]
sport.f.ordered <- sport.f[order(sport.f$PD_BUY_AM, decreasing=F), -c(1:3,5,9)]
# 디스커버리 지우기
removal <- sport.f.ordered[18:19,1] # 위에 2개의 PD_C
product <- product[!(product$PD_C %in% removal & product$PD_BUY_AM == 549),] 
multiply <- sport.f.ordered[1:17,1] # 여기까지 고가 가전. 

product$PD_BUY_AM <- ifelse(product$PD_C %in% multiply, 
                            ifelse(product$PD_BUY_AM < 1000, product$PD_BUY_AM * 1000, product$PD_BUY_AM)
                            , product$PD_BUY_AM)
#### 18 식기/조리기구  S/B ####
spoon <- pro_master[pro_master$CLAC1_NM=="식기/조리기구" , ]
product[product$PD_C==504578 & (product$PD_BUY_AM==65|product$PD_BUY_AM==69), 7] <- product[product$PD_C==504578 & (product$PD_BUY_AM==65|product$PD_BUY_AM==69), 7] * 1000
product[product$PD_C==504573 & product$PD_BUY_AM==72, 7] <- product[product$PD_C==504573 & product$PD_BUY_AM==72, 7] * 1000
product[product$PD_C==845221, 7] <- product[product$PD_C==845221, 7] * 1000

### 가격이 너무 높은 애들 지우기 
spoon.ordered2 <- spoon[order(spoon$PD_BUY_AM, decreasing=T), -c(1:3,5,9)]
removal <- spoon.ordered2[1:4,1] # 위에 4개의 PD_C
product <- product[!(product$PD_C %in% removal),] 

#### 19 아웃도어/레저   S  ####
leisure <- pro_master[pro_master$CLAC1_NM=="아웃도어/레저" , ]
leisure.ordered <- leisure[order(leisure$PD_BUY_AM, decreasing=F), -c(1:3,5)]
removal <- leisure.ordered[2:4,1] # 위에 2개의 PD_C
product <- product[!(product$PD_C %in% removal & (product$PD_BUY_AM == 49|product$PD_BUY_AM == 69)),] 

#### 20 여성의류        S  ####
clothes.f <- pro_master[pro_master$CLAC1_NM=="여성의류" , ]
# clothes.f.ordered <- clothes.f[order(clothes.f$PD_BUY_AM, decreasing=F), -c(1:3,5)]
# product 바꾸기.
product[(product$PD_C==718054) & (product$PD_BUY_AM==200), 7] <- product[(product$PD_C==718054) & (product$PD_BUY_AM==200), 7] * 1000
product[(product$PD_C==402783) & (product$PD_BUY_AM==249), 7] <- product[(product$PD_C==402783) & (product$PD_BUY_AM==249), 7] * 1000
product[(product$PD_C==425221) & (product$PD_BUY_AM==249), 7] <- product[(product$PD_C==425221) & (product$PD_BUY_AM==249), 7] * 1000


#### 21 영상/음향가전   S  ########
video.sound <- pro_master[pro_master$CLAC1_NM=="영상/음향가전" , ]
video.sound.ordered <- video.sound[order(video.sound$PD_BUY_AM, decreasing=F), -c(1:3,5,9)]
multiply <- video.sound.ordered[c(1:3,13,20:72,75:81,84:85,245:256,259,262:291,461:479,543:545),1]
# head(product[product$PD_C %in% multiply,7],250)
product$PD_BUY_AM <- ifelse(product$PD_C %in% multiply, 
                            ifelse(product$PD_BUY_AM < 5000, product$PD_BUY_AM * 1000, product$PD_BUY_AM)
                            , product$PD_BUY_AM)

#### 22 완구           S/B ####
pencil <- pro_master[pro_master$CLAC1_NM=="완구" , ]
multiply <- 773017
product[product$PD_C %in% multiply, 7]  <- product[product$PD_C %in% multiply, 7]*1000

### 가격이 너무 높은 애들 지우기
pencil.ordered2 <- pencil[order(pencil$PD_BUY_AM, decreasing=T),-c(1:3,5,9)]
removal <- pencil.ordered2[1:6,1] # 위에 6개의 PD_C
product <- product[!(product$PD_C %in% removal),] 

#### 23 원예/애완      S/B ####
# 전체 boxplot 
plant.pet <- pro_master[pro_master$CLAC1_NM=="원예/애완" , ]
#퓨리나 제거
product <- product[!(product$PD_C == 607661 & product$PD_BUY_AM == 21),] 
plant.pet.ordered2 <- plant.pet[order(plant.pet$PD_BUY_AM, decreasing=T),-c(1:3,5,9)]
removal <- pencil.ordered2[1:2,1] # 위에 2개의 PD_C
product <- product[!(product$PD_C %in% removal),] 

# 안겹침.
# plant.pet.ordered[plant.pet.ordered$PD_C==789988,


#### 24 유아동의류      B  #####
child <- pro_master[pro_master$CLAC1_NM=="유아동의류" , ]
child.ordered2 <- child[order(child$PD_BUY_AM, decreasing=T),-c(1:3,5,9)]
removal <- child.ordered2[1:2,1] # 위에 2개의 PD_C
product <- product[!(product$PD_C %in% removal),] 


#### 25 음료            S  #####
drink <- pro_master[pro_master$CLAC1_NM=="음료" , ]
drink.ordered <- drink[order(drink$PD_BUY_AM, decreasing=F), -c(1:3,5,9)]
multiply <- drink.ordered[1:15,1] # 여기까지 고가 가전. 
# head(product[product$PD_C %in% multiply,7],17)
product$PD_BUY_AM <- ifelse(product$PD_C %in% multiply,
                            ifelse(product$PD_BUY_AM < 200, product$PD_BUY_AM * 1000, product$PD_BUY_AM)
                            , product$PD_BUY_AM)


#### 26 인테리어/조명  S/ B#####
interior <- pro_master[pro_master$CLAC1_NM=="인테리어/조명" , ]
interior.ordered2 <- interior[order(interior$PD_BUY_AM, decreasing=T),-c(1:3,5,9)]
removal <- child.ordered2[1:4,1] # 위에 2개의 PD_C
product <- product[!(product$PD_C %in% removal),] 



#### 28 주방잡화        B  #####
kitchen.zap <- pro_master[pro_master$CLAC1_NM=="주방잡화" , ]
kitchen.zap.ordered2 <- kitchen.zap[order(kitchen.zap$PD_BUY_AM, decreasing=T),-c(1:3,5,9)]
removal <- kitchen.zap.ordered2[1:13,1] # 위에 2개의 PD_C
product <- product[!(product$PD_C %in% removal),] 



#### 30 축산물          B  #####
meat <- pro_master[pro_master$CLAC1_NM=="축산물" , ]
meat.ordered2 <- meat[order(meat$PD_BUY_AM, decreasing=T),-c(1:3,5,9)]
removal <- meat.ordered2[1:13,1] # 위에 2개의 PD_C
product <- product[!(product$PD_C %in% removal & product$PD_BUY_AM >2000000),] 



#### 31 출산/육아용품 #####
#small range price arranging 1 obs
baby.num <- c(800438)
product$PD_BUY_AM <- ifelse(product$PD_C %in% baby.num, 
                            ifelse(product$PD_BUY_AM < 1000, product$PD_BUY_AM * 1000, product$PD_BUY_AM)
                            , product$PD_BUY_AM)
#large range removal 2 obs with 2 PD_C

removal <- c(611947,611933)
product <- product[!(product$PD_C %in% removal),] 

#### 32 침구/수예 ####
#small X/ large PD_C 3개
removal <-c(50121,768277,768685)
product <- product[!(product$PD_C %in% removal),]

#### 33 컴퓨터 ####
#remove 102 won 
product <- product[!(product$CLNT_ID==9392 & product$SESS_ID == 10826122 & product$PD_C==841456),]
#laptops
com.number <- c(651302,428463,333624,405652,582801,587765,410049,589522,589522,589519,668368,659773,659665
                
                ,659774,659674,659670,659780,589519,589521,673257,437695,581979,413328,539083,683482,539083
                
                ,539083,491348,673242)
product$PD_BUY_AM <- ifelse(product$PD_C %in% com.number, 
                            ifelse(product$PD_BUY_AM < 1000, product$PD_BUY_AM * 1000, product$PD_BUY_AM)
                            , product$PD_BUY_AM)
#### 34 패션잡화 ####
#remove
fas.rm <- c(51344,64167,684060)
product <- product[!(product$PD_C %in% fas.rm),] #?
#multiply 1000
fas.num <- c(427300,598449,610943,641568,689224,785819)
product$PD_BUY_AM <- ifelse(product$PD_C %in% fas.num, 
                            ifelse(product$PD_BUY_AM < 8000, product$PD_BUY_AM * 1000, product$PD_BUY_AM)
                            , product$PD_BUY_AM)

#### 35 퍼스널케어 ####
product <- product[!(product$PD_C==384523 & product$PD_BUY_AM < 100),]

#### 36 헬스/피트니스 ####
fit.num <- c(559095,601263,774649)
product$PD_BUY_AM <- ifelse(product$PD_C %in% fit.num, 
                            ifelse(product$PD_BUY_AM < 2000, product$PD_BUY_AM * 1000, product$PD_BUY_AM)
                            , product$PD_BUY_AM)
fit.rm <-c(597179)
product <- product[!(product$PD_C %in% fit.rm),]

#### 37화장품/뷰티케어 ####
bea.rm <-c(514197,580759,584448,789236)
product$PD_BUY_AM <- ifelse(product$PD_C %in% bea.rm, 
                            ifelse(product$PD_BUY_AM < 1000, product$PD_BUY_AM * 1000, product$PD_BUY_AM)
                            , product$PD_BUY_AM)
bea.rm2 <-c(402140)
product <- product[!(product$PD_C %in% bea.rm2),] 






###### 7 grep by CLAC3_NM  ######### 
###### CLAC3_NM remove mislocated observations ############
###### product.outlier.rmvd <- product################
product.outlier.rmvd <- product
#####_________________pro_master 생성_______________________  ####
#library(dplyr)
# pro_master <- inner_join(product, master, by= c("PD_C","CLAC1_NM","CLAC2_NM","CLAC3_NM","PD_NM"))
pro_master <- inner_join(product, master, by= c("PD_C"))
#한번 더해서 변수가 이상해짐... so 변수이름 다 지정한것 
pro_master_raw <- pro_master   # copy raw dataframe
###_______________________________________________________________________________________###

####  rank 1 남성티셔츠  ####
man_t <- pro_master[pro_master$CLAC3_NM=="남성티셔츠", ]
mis_man_t1 <- grep("여성.?", man_t$PD_ADD_NM, ignore.case = T)
mis_man_t2 <- grep("여성.?", man_t$PD_NM, ignore.case = T)
mis_man_t3 <- grep("여자.?", man_t$PD_ADD_NM, ignore.case = T)
mis_man_t4 <- grep("여자.?", man_t$PD_NM, ignore.case = T)
man_t_removable <- man_t %>%
  filter(man_t$PD_ADD_NM %in% unique(man_t$PD_ADD_NM[mis_man_t1])|man_t$PD_NM %in% unique(man_t$PD_NM[mis_man_t2])
         |man_t$PD_ADD_NM %in% unique(man_t$PD_ADD_NM[mis_man_t3])|man_t$PD_NM %in% unique(man_t$PD_NM[mis_man_t4]))
pro_master <- pro_master[!(pro_master$PD_C  %in% unique(man_t_removable$PD_C)), ]

####  rank 2 여성원피스  ####

w_ops <- pro_master[pro_master$CLAC3_NM=="여성원피스", ]
mis_w_ops <- grep("남성.", w_ops$PD_ADD_NM, ignore.case = T)
mis_w_ops2 <- grep("남성.", w_ops$PD_NM, ignore.case = T)
mis_w_ops3 <- grep("남자.", w_ops$PD_ADD_NM, ignore.case = T)
mis_w_ops4 <- grep("남자.", w_ops$PD_NM, ignore.case = T)

####  rank 3 여성티셔츠/탑  ####

w_top <- pro_master[pro_master$CLAC3_NM=="여성티셔츠/탑", ]
mis_w_top <- grep("남성.", w_top$PD_ADD_NM, ignore.case = T)
mis_w_top2 <- grep("남성.", w_top$PD_NM, ignore.case = T)
mis_w_top3 <- grep("남자.", w_top$PD_ADD_NM, ignore.case = T)
mis_w_top4 <- grep("남자.", w_top$PD_NM, ignore.case = T)

w_top_removable <- w_top %>%
  filter(w_top$PD_ADD_NM %in% c(unique(w_top$PD_ADD_NM[mis_w_top]),unique(w_top$PD_ADD_NM[mis_w_top3]))
         |w_top$PD_NM %in% c(unique(w_top$PD_NM[mis_w_top2],unique(w_top$PD_NM[mis_w_top4]))))

pro_master <- pro_master[!(pro_master$PD_C  %in% unique(w_top_removable$PD_C)), ]


####  rank 4 여성남방셔츠  ####

f_shirt <- pro_master[pro_master$CLAC3_NM=="여성남방셔츠",]
f_shirt_grep <- f_shirt[c(grep("남성.", f_shirt$PD_NM)),]
pro_master <- pro_master[!(pro_master$PD_C %in% f_shirt_grep$PD_C),]

####  rank 5 남성캐주얼바지  ####

# 112059 -> 69693
#남성캐주얼바지 remove #여성, 여
m_pant <- pro_master[pro_master$CLAC3_NM=="남성캐주얼바지",]
m_1 <- grep("여성?",m_pant$PD_NM) #44889 #여성, 여, 남여공용 제거할라고 추출 
m_12 <- grep("남여공용.?",m_pant$PD_NM) #2523 # 남녀공용은 제거 안할라고 추출 
number <- m_1[!(m_1 %in% m_12)] #여성, 여, 남여공용 중에 남녀공용이 아닌 것 obs id number 추출 
m_pant_grep <- m_pant[c(number),]
pro_master <- pro_master[!(pro_master$PD_C %in% m_pant_grep$PD_C),]

####  rank 6 여성바지######
f_pant <- pro_master[pro_master$CLAC3_NM=="여성바지",]
f_pant_1 <- unique(f_pant) #47821
f_pant_11 <- grep("남성.?", f_pant_1$PD_NM, ignore.case = T)
f_pant_12 <- grep("남성.?", f_pant_1$PD_ADD_NM, ignore.case = T)
f_pant_21 <- grep("남자.?", f_pant_1$PD_NM)
f_pant_22 <- grep("남자.?", f_pant_1$PD_ADD_NM)
f_pant_removable <- f_pant_1 %>%
  filter(f_pant_1$PD_NM %in% f_pant_1$PD_NM[f_pant_11]|f_pant_1$PD_ADD_NM %in% f_pant_1$PD_ADD_NM[f_pant_12]
         |f_pant_1$PD_NM %in% f_pant_1$PD_NM[f_pant_21]|f_pant_1$PD_ADD_NM %in% f_pant_1$PD_ADD_NM[f_pant_22])
pro_master <- pro_master[!(pro_master$PD_C  %in% f_pant_removable$PD_C), ]

####  rank 7 블러셔/쉐이딩/하이라이터  ######

blu_shad <- pro_master[pro_master$CLAC3_NM=="블러셔/쉐이딩/하이라이터" , ]
blu_shad_nm <- unique(blu_shad$PD_ADD_NM)

# '색상'이란 단어가 없는 경우?
# blu_shad_col <- grep(".?색상", blu_shad$PD_ADD_NM, ignore.case = T)
# 특이사항 없음

# '색상'이란 단어가 없는 것들 
# blu_shad_ps <- unique(blu_shad[blu_shad_col,"PD_ADD_NM"])
# blu_shad_no_col <- blu_shad[blu_shad$PD_ADD_NM %out% blu_shad_ps,-c(1:4)]
# 특이사항 없음

# 틴트, 립밤 등 립제품 섞인 것들 제거 (총 309개)
blu_shad_mis_lip <- grep("립.?", blu_shad$PD_ADD_NM, ignore.case = T)
blu_shad_mis_lip2 <- grep("립.", blu_shad$PD_NM, ignore.case = T)
blu_shad_removable <- blu_shad %>%
  filter(blu_shad$PD_ADD_NM %in% unique(blu_shad$PD_ADD_NM[blu_shad_mis_lip])
         |blu_shad$PD_NM %in% c(unique(blu_shad$PD_NM[blu_shad_mis_lip2])))
pro_master <- pro_master[!(pro_master$PD_C  %in% unique(blu_shad_removable$PD_C)), ]

####  rank 8 스킨케어세트 ######

skincare <- pro_master[pro_master$CLAC3_NM=="스킨케어세트", ]

# '색상'이란 단어가 있는것들 / 쿠션 추가증정 (특이사항 없음)
# skincare_col <- grep("색상.?", skincare$PD_ADD_NM, ignore.case = T)
# skincare_ps <- unique(skincare[skincare_col,"PD_ADD_NM"])
# skincare_col <- skincare[skincare$PD_ADD_NM %in% skincare_ps,-c(1:4)]
# '호', '호수'란 단어가 있는것들 / 쿠션 추가증정 (특이사항 없음)
# skincare_num <- grep("호수.?", skincare$PD_ADD_NM, ignore.case = T)
# skincare_ps_n <- unique(skincare[skincare_num,"PD_ADD_NM"])
# skincare_num <- skincare[skincare$PD_ADD_NM %in% skincare_ps_n,-c(1:4)]
# skincare_ho <- grep("호.?", skincare$PD_ADD_NM, ignore.case = T)
# skincare_ps_h <- unique(skincare[skincare_ho,"PD_ADD_NM"])
# skincare_ho <- skincare[skincare$PD_ADD_NM %in% skincare_ps_h,-c(1:4)]
# '메이크업'이란 단어가 있는것들 / 쿠션 추가증정 (특이사항 없음)
# skincare_mb <- grep("메이크업.?", skincare$PD_ADD_NM, ignore.case = T)
# skincare_ps_m <- unique(skincare[skincare_mb,"PD_ADD_NM"])
# skincare_mb <- skincare[skincare$PD_ADD_NM %in% skincare_ps_m,-c(1:4)] # 선크림과 메이크업베이스 세트
# 'BB'란 단어가 있는것들 / 쿠션 추가증정 (특이사항 없음)
# skincare_bb <- grep("BB.?", skincare$PD_ADD_NM, ignore.case = T)
# skincare_ps_b <- unique(skincare[skincare_bb,"PD_ADD_NM"])
# skincare_bb <- skincare[skincare$PD_ADD_NM %in% skincare_ps_b,-c(1:4)]
# 틴트, 립밤 등 립제품 섞인 것들 제거 (총 26개)
skincare_mis_lip <- grep("립.?", skincare$PD_ADD_NM, ignore.case = T)
skincare_mis_lip2 <- grep("립.?", skincare$PD_NM, ignore.case = T)
skincare_removable <- skincare %>%
  filter(skincare$PD_ADD_NM %in% unique(skincare$PD_ADD_NM[skincare_mis_lip])
         |skincare$PD_NM %in% c(unique(skincare$PD_NM[skincare_mis_lip2])))
pro_master <- pro_master[!(pro_master$PD_C  %in% unique(skincare_removable$PD_C)), ]


####  rank 9 BB/파운데이션/컴팩트류 ######

bb_fd <- pro_master[pro_master$CLAC3_NM=="BB/파운데이션/컴팩트류" , ]

# '색상'이란 단어가 들어간 경우
# bb_col <- grep(".?색상", bb_fd$PD_ADD_NM, ignore.case = T)
# bb_col_ps <- unique(bb_fd[bb_col,"PD_ADD_NM"])
# bb_s <- bb_fd[bb_fd$PD_ADD_NM %in% bb_col_ps,-c(1:4)] # 특이점 없음

# '색상'이란 단어가 들어가있지 않은 경우
# `%out%` <- function(a,b) ! a %in% b
# bb_no_col <- bb_fd[bb_fd$PD_ADD_NM %out% bb_col_ps,-c(1:4)] # 특이점 없음

# '사이즈'란 단어가 들어간 경우
# bb_size <- grep("사이즈.?", bb_fd$PD_ADD_NM, ignore.case = T)
# length(unique(bb_fd[bb_size,"PD_ADD_NM"]))
# bb_size_ps <- unique(bb_fd[bb_size,"PD_ADD_NM"])
# bb_sizes <- bb_fd[bb_fd$PD_ADD_NM %in% bb_size_ps,-c(1:4)] # 특이점 없음

# 틴트, 립밤, 립스틱 등 립제품이 섞인 경우 (총 117개) 
# bb_fd_mis_lip <- grep("립.?", bb_fd$PD_ADD_NM, ignore.case = T)  #파운데이션+립 세트 제품


####  rank 10 남성스포츠티셔츠 ####

m_sport <- pro_master[pro_master$CLAC3_NM=="남성스포츠티셔츠",]#58065
m_sport_1 <- m_sport[,4:12]
m_sport_2 <- unique(m_sport_1) #29235
m_s1 <- grep("여성.?", m_sport_2$PD_NM)
m_s2 <- grep("여성.?", m_sport_2$PD_ADD_NM)
m_s3 <- grep("(여).?", m_sport_2$PD_NM)
m_s4 <- grep("(여).?", m_sport_2$PD_ADD_NM)
m_sport_removable <- m_sport_2 %>%
  filter(m_sport_2$PD_NM %in% m_sport_2$PD_NM[m_s1] | m_sport_2$PD_ADD_NM %in% m_sport_2$PD_ADD_NM[m_s2]
         |m_sport_2$PD_NM %in% m_sport_2$PD_NM[m_s3] | m_sport_2$PD_ADD_NM %in% m_sport_2$PD_ADD_NM[m_s4])
pro_master <- pro_master[!(pro_master$PD_C  %in% m_sport_removable$PD_C), ]



##### 구매자수가 많은/적은 CLAC3_NM top10 처리전,후 비교 ##############
# CLAC3_count <- plyr::count(pro_master_raw,"CLAC3_NM") %>% arrange(desc(freq))
# CLAC3_count_removed <- plyr::count(pro_master,"CLAC3_NM") %>% arrange(desc(freq))
# 새롭게 추가된 소분류 확인
# baby_t <- pro_master[pro_master$CLAC3_NM=="영유아티셔츠/탑",]


#####_________________ create 8 new variables_________________ ######### 

#####_________________ session_master 생성 ___________________####
## NA 확인 ##
# session에 NA 제거
session <- session[!(is.na(session$TOT_SESS_HR_V)|is.na(session$TOT_PAG_VIEW_CT)),]
# 물품 구매 한 session 고르기
buy.session <- session[session$SESS_ID %in% pro_master$SESS_ID,]
# product와 merge
buy.session_pro <- dplyr :: inner_join(buy.session, pro_master, by=c("SESS_ID","CLNT_ID")) #왜냐면 SESS_ID 고유하지 않다...
# 걔네를 다시 master와 merge
session_master <- dplyr :: inner_join(buy.session_pro, master, by=c("PD_C","CLAC1_NM","CLAC2_NM","CLAC3_NM","PD_NM")) #merge takes too much time 
rm(buy.session)
rm(buy.session_pro)
# sum(is.na(session_master()))
# str(session_master)
# sum(is.na(session_master)) # NA 더이상 없음. 


#### hit_avg #########
# 소분류 hit_seq 합 / 소분류 SESS_ID 개수
total.HITS_SEQ <- with(session_master,aggregate(HITS_SEQ,by=list(CLAC3_NM=CLAC3_NM),sum,simplify = T)) # 소분류 hits_seq 합
# head(total.HITS_SEQ)
nrow.SESS_ID <- with(session_master, aggregate(SESS_ID, by=list(CLAC3_NM=CLAC3_NM),length, simplify =T)) # 소분류 SESS_ID 행수
# head(nrow.SESS_ID)
hits_avg.temp <- merge(total.HITS_SEQ, nrow.SESS_ID, by="CLAC3_NM")
# head(hits_avg.temp)
hits_avg.temp $ hits_avg <- hits_avg.temp[,2] / hits_avg.temp [,3]
colnames(hits_avg.temp) <- c("CLAC3_NM", "sum.hit_seq", "nrow.sess_id", "hits_avg" )



#### duration_avg ######### 
# session 중 구매한 session 만 빼야댐.
# 소분류 duration 합 / 소분류 session 개수
ASD <- with(session_master, aggregate(TOT_SESS_HR_V, by=list(CLAC3_NM=CLAC3_NM), sum))
colnames(ASD) <- c("CLAC3_NM", "ASD")
# head(ASD)
nrow.SESS_ID2 <- with(session_master, aggregate(SESS_ID, by=list(CLAC3_NM=CLAC3_NM),length, simplify =T)) # 소분류 SESS_ID 행수
ASD <- merge(ASD, nrow.SESS_ID2, by="CLAC3_NM")
ASD $ asd <- ASD[,2] / ASD[,3]
colnames(ASD) <- c("CLAC3_NM", "ASD", "nrow.SESS_ID2", "duration_avg")


#### page_view_avg #########
# 소분류 page 합 / 소분류 SESS_ID 행수.
# 구매한 세션 평균 페이지 수 / 
# 세션 별 평균 페이지 수 ?
page.per.sess <- with(session_master, aggregate(TOT_PAG_VIEW_CT, by=list(CLAC3_NM=CLAC3_NM), sum))
colnames(page.per.sess) <- c("CLAC3_NM", "sum")
page_view_avg <- page.per.sess[,c(1,2)]
page_view_avg <- merge(page_view_avg, nrow.SESS_ID2, by="CLAC3_NM")
page_view_avg $ page_view_avg <- page_view_avg[,2] / page_view_avg[,3] 
# head(page_view_avg)
colnames(page_view_avg) <- c("CLAC3_NM", "sum", "nrow.SESS_ID2", "page_view_avg")


#### buy.count$buy ########
buy.count.CLAC23 <- plyr::count(session_master, c("CLAC2_NM","CLAC3_NM","SESS_ID"))
count.2.3 <- with(buy.count.CLAC23, aggregate(freq, by=list(CLAC2_NM=CLAC2_NM,CLAC3_NM=CLAC3_NM), sum))
count.2 <- with(buy.count.CLAC23, aggregate(freq, by=list(CLAC2_NM=CLAC2_NM), sum))
plz <- dplyr::full_join(count.2, count.2.3, by= "CLAC2_NM") 
plz$buy <- plz$x.y/plz$x.x
buy.count <- plz[,c(1,3,5)]


#### price$p.avg #########
p.count <- plyr::count(session_master, c("CLAC3_NM","SESS_ID")) # 소분류별 구매건수 count
p.count2 <- with(p.count, aggregate(freq, by=list(CLAC3_NM=CLAC3_NM),sum)) # 소분류별 구매건수 count sum
p.sum <- with(session_master, aggregate(PD_BUY_AM, by=list(CLAC3_NM=CLAC3_NM),sum)) # 소분류별 구매상품 1개당 구매금액 sum
price <- dplyr::inner_join(p.count2, p.sum, by="CLAC3_NM")
colnames(price) <- c("CLAC3_NM", "p.count2","p.sum")
price$p.avg <- price$p.sum/price$p.count2
price$p.avg <- round(price$p.avg,-1)

#### PD_C count #####
# 소분류별 PD_C 개수. 
PD_C_count <- with(session_master,aggregate(PD_C,by=list(CLAC3_NM=CLAC3_NM),unique,simplify = T)) # 소분류 PD_C unique합
PD_C_count[,2] <- lengths(PD_C_count$x)
##### 워크스페이스 정리 ####

rm(list=setdiff(ls(), c("product","pro_master","session_master","ASD","hits_avg.temp","page_view_avg","buy.count","price","search1","search2","session","master", "PD_C_count")))

#### search.ratio ##########

# 3-1 SEARCH_CNT num으로 바꾸고 천단위 , 제거
search2$SEARCH_CNT <- gsub(",","",search2$SEARCH_CNT) # 해당 검색어 검색량 SEARCH_CNT 변수 천 단위 , 제거
search2$SEARCH_CNT <- as.numeric(search2$SEARCH_CNT)  

# str(custom)
# custom$CLNT_GENDER <- as.factor(custom$CLNT_GENDER) # 성별 변수


library(plyr)
library(dplyr)
# search1, search2 를 KWD_NM (검색어)로 연결, 검색량은 다를 수 있음
search1_reduced <- search1[,c("KWD_NM","SEARCH_CNT")]
search2_reduced <- search2[,c("KWD_NM","SEARCH_CNT")]
colnames(search2_reduced)[colnames(search2_reduced) == 'SEARCH_CNT'] <- 'SEARCH_CNT2'
product_reduced <- session_master[,c("CLNT_ID","SESS_ID","PD_C","CLAC3_NM")]
# length(unique(product_reduced$CLAC3_NM)) #824ㄷ
# length(unique(s1.pm$CLAC3_NM)) #824ㄷ
#### search2 중 search1 에 없는 키워드 제거
search_list <- unique(search1_reduced$KWD_NM)
search2_reduced <- search2_reduced %>% #7862300
  filter(KWD_NM %in% search_list)

#search1
#### ___________________________________________________여기서 823이 되는데___________________________________________###
s1.pm <- dplyr::inner_join(search1,product_reduced, by=c("CLNT_ID","SESS_ID")) #CLNT_ID,SESS_ID,KWD_NM,SEARCH_CNT,PD_C,CLAC3_NM #5992863
s1.pm2 <- s1.pm[,c(4,6)] #SEARCH_CNT, CLAC3_NM
s1.sum <- with(s1.pm2,aggregate(SEARCH_CNT, by=list(CLAC3_NM=CLAC3_NM),sum)) #sum of SEARCH_CNT by CLAC3_NM #CLAC3_NM, x
# list1 <- unique(product_reduced$CLAC3_NM)
# list2 <- unique(s1.sum$CLAC3_NM)
# length(list1)
# length(list2)
# list1[list1 %in% list2==F]
# list2[list2=="DIY완구"]

#search2
key.clac3 <- s1.pm[,c(3,6)] #KWD_NM, CLAC3_NM #5992863
key.clac3.uni <- unique(key.clac3) #1628087

#next step 용량 개많이 차지 so remove!
# rm(product)
# rm(pro_master)
# rm(search1)
rm(search2)
rm(s1.pm)
rm(key.clac3)
rm(search_list)
rm(product_reduced)
rm(s1.pm2)

s2.sum.kwd <- with(search2_reduced,aggregate(SEARCH_CNT2,by=list(KWD_NM=KWD_NM),sum)) #KWD_NM,SEARCH_CNT2
s2.clac3 <- dplyr::inner_join(key.clac3.uni, s2.sum.kwd, by="KWD_NM") #KWD_NM, CLAC3_NM, x
colnames(s2.clac3)[3] <- "SEARCH_CNT2" #change name
s2.clac3.2 <- s2.clac3[,c(2,3)] #CLAC3_NM, SEARCH_CNT2
#rm(s2.clac3)
s2.sum <- with(s2.clac3.2,aggregate(SEARCH_CNT2, by=list(CLAC3_NM=CLAC3_NM),sum)) #CLAC3_NM,x

#ratio
colnames(s2.sum)[2] <- "SEARCH_CNT2_SUM"
colnames(s1.sum)[2] <- "SEARCH_CNT1_SUM"
identical(s2.sum$CLAC3_NM,s1.sum$CLAC3_NM) 
s1.sum$SEARCH_CNT2_SUM <- s2.sum$SEARCH_CNT2_SUM
s1.sum$search.ratio <- s1.sum$SEARCH_CNT1_SUM/s1.sum$SEARCH_CNT2_SUM
s.sum <- s1.sum
s.sum$search.ratio<-round(s.sum$search.ratio,10)
# s.sum$search.ratio
# length(unique(pro_master$CLAC3_NM))

#### mobile ratio #############
s1 <- plyr::count(search1[,c(1,2,3)], c("CLNT_ID","SESS_ID","KWD_NM")) #각 세션별로 키워드를 검색한 건수를 셈 so drop freq(검색어 개수를 볼라고 한명이 여러번 ㄴㄴ)
s3.count <- plyr::count(s1[,c(1,2,3)], c("KWD_NM"))
#KWD_NM & CLAC3_NM 결합하자!
p.m <- pro_master[,c(1,2,4,12)] #CLNT_ID, SESS_ID, PD_C, CLAC3_NM
s1.pm <- dplyr::inner_join(search1[,c(1,2,3)], p.m , by=c("CLNT_ID","SESS_ID")) #CLNT_ID,SESS_ID,KWD_NM,PD_C,CLAC3_NM #5992863
s6 <- dplyr::inner_join(s3.count, unique(s1.pm[,c(3,5)]), by="KWD_NM")
mobile <- session_master[,c(1,2,7,19)]
mobile$binary <- ifelse(mobile$DVC_CTG_NM==2, 0, 1)
sum <- plyr::count(mobile, c("binary","CLAC3_NM"))
count <- plyr::count(mobile, c("CLAC3_NM"))
chu <- sum[sum$binary==1, ]
chuchu <- dplyr::inner_join(chu,count,by=c("CLAC3_NM"))
chuchu$mobile.ratio <- (chuchu$freq.x)/(chuchu$freq.y)
gigi <- chuchu[,c("CLAC3_NM","mobile.ratio")]
# gigi<-gigi[!(gigi$CLAC3_NM == "남녀공용향수세트"),]

#### online으로 통합 #######
# rm하자
rm(list=setdiff(ls(), c("hits_avg.temp","ASD","page_view_avg","buy.count","price","s.sum","session_master","pro_master","product","gigi","PD_C_count")))
#online으로 통합.
online <- join_all(list(ASD[,c(1,4)],hits_avg.temp[,c(1,4)], page_view_avg[,c(1,4)], buy.count[,c(2,3)],
                        price[,c(1,4)], s.sum[,c(1,4)], PD_C_count, gigi), by='CLAC3_NM', type='inner')
str(online) # just checking on our data
colnames(online) <- c("CLAC3_NM","duration.avg","hits.avg","view.avg","puchase","price.avg","search.ratio", "PD_C.count", "mobile.ratio")
# online.include.outlier <- online
#online에서 outlier제거
online <- online[!(online$CLAC3_NM=="남녀공용향수세트"),]
rm(list=setdiff(ls(), c("online","session_master","pro_master")))




#####_________________________PCA_____________________________ ########
#### online cor리 확이 & PCA 전처리 ###############
str(online)
round(cor(online[,2:9]),3)
rownames(online) <- online$CLAC3_NM
online.1 <- online[,-1]
# str(online.1)

#### online DATA에 전체 8개 변수 모두 넣고 PCA 실행해봄. 잘 안나옴. ####
pca.online <- prcomp(online.1, retx=T, center=T, scale.=T)
summary(pca.online)
pca.online$rotation
pca.online$sdev


## Calculate the loading matrix
U <- pca.online$rotation
D <- diag(pca.online$sdev^2)
online.loading <- U %*% D^.5
online.loading
# pc.name <- c("PC.1", "PC.2", "PC.3", "PC.4", "PC.5", "PC.6","PC.7","PC.8")
# dimnames(online.loading)[[2]] <- pc.name
# online.loading
round(online.loading[,1:3], 3)

## Column sums of squares of loading matrix are eignevalues
eigen.online <- apply(online.loading^2, 2, sum)
round(eigen.online, 3)
round(pca.online$sdev^2-eigen.online, 3)

#pdf("loading.pdf", height=5, width=10)

## I will create 1*2 figure layout
par(mfrow=c(1,1))

## Do a factor loading plot
# plot(online.loading[,1], online.loading[,2], xlab="PCA 1", ylab="PCA 2",
#      type="n", xlim=c(-0.9,0.9), ylim=c(-0.9,0.9)) 
# arrows(0,0, online.loading[,1], online.loading[,2], length=0.1,
#        angle=20, col="red")
# text(online.loading[,1]*1.1, online.loading[,2]*1.1, 1:nrow(online), col="black", cex=0.7)

## Do a PCA scores plot
# plot(pca.online$x[,1], pca.online$x[,2], xlab="PCA 1", ylab="PCA 2",
#      type="n", xlim=c(-3,7), ylim=c(-4,3)) 
# text(pca.online$x[,1]*1, pca.online$x[,2]*1, 1:nrow(online), col="black", cex=0.7)
#dev.off()

library(pca3d)
str(online)
# pca.online <- prcomp(online[,-c(1,2)], retx=T, center=T, scale.=T)
gr <- factor(online[,1])
U <- pca.online$rotation
D <- diag(pca.online$sdev^2)
online.loading <- U %*% D^.5
online.loading
pc.name <- c("PC.1", "PC.2", "PC.3", "PC.4", "PC.5", "PC.6")
dimnames(online.loading)[[2]] <- pc.name
online.loading
round(online.loading[,1:3], 3)

## Column sums of squares of loading matrix are eignevalues
eigen.online <- apply(online.loading^2, 2, sum)
round(eigen.online, 3)
round(pca.online$sdev^2-eigen.online, 3)
pca3d(pca.online)

pca2d(pca.online,biplot=TRUE,group=gr,biplot.vars=3)
gr <- factor(online[,6])

#### [PCA로 축소] online DATA에 3개(= hits + view + duration) 축소  ####
str(online.1)
online.1 <- online.1[,-c(4:8)] # duration & hits_acg & page_view만 PCA로 줄인다.
head(online.1)
pca.online <- prcomp(online.1, retx=T, center=T, scale.=T)
screeplot(pca.online,type="lines", main="3 variables PCA")
# 잘 됐는지 확인.
summary(pca.online)
# pca.online$rotation
pca.online$sdev
# # Calculate the loading matrix
U <- pca.online$rotation
D <- diag(pca.online$sdev^2)
online.loading <- U %*% D^.5
online.loading
pc.name <- c("PC.1", "PC.2", "PC.3")
dimnames(online.loading)[[2]] <- pc.name
online.loading
round(online.loading[,1:3], 3)
# head(pca.online$x)
plot(pca.online$x[,1])
identify(pca.online$x[,1])
# online[104,]
# online[55,]
# online[651,]
# online[71,]
new.online <- online
# str(new.online)
new.online[,10] <- pca.online$x[,1]
str(new.online)
colnames(new.online) <- c("CLAC3_NM", "duration_avg", "hits_avg", "view_avg", "purchase", "price.avg", 
                          "search.ratio", "PD_C.count", "mobile.ratio", "digital" )
new.online <- new.online[,c(1,5,6,7,8,9,10)]
# rm(list=setdiff(ls(), c("new.online","online","session_master","pro_master")))





#####________________________Cluster 시작_____________________ #################
head(new.online)
str(new.online)
new.online.1 <- new.online[,2:7]
str(new.online.1)

###### 변수들 끼리의 plot. 아웃라이어확인. #####
# par(mfrow=c(1,2))
plot(new.online.1[,1], new.online.1[,2], main="New Online", xlab="purchase", ylab="price.avg")
plot(new.online.1[,1], new.online.1[,3], main="New Online", xlab="purchase", ylab="search.ratio")
plot(new.online.1[,1], new.online.1[,4], main="New Online", xlab="purchase", ylab="PD_C.count")
plot(new.online.1[,1], new.online.1[,5], main="New Online", xlab="purchase", ylab="mobile.ratio")
plot(new.online.1[,1], new.online.1[,6], main="New Online", xlab="purchase", ylab="digital")

plot(new.online.1[,2], new.online.1[,3], main="New Online", xlab="price.avg", ylab="search.ratio") # 확인
plot(new.online.1[,2], new.online.1[,4], main="New Online", xlab="price.avg", ylab="PD_C.count")
plot(new.online.1[,2], new.online.1[,5], main="New Online", xlab="price.avg", ylab="mobile.ratio")
plot(new.online.1[,2], new.online.1[,6], main="New Online", xlab="price.avg", ylab="digital")

plot(new.online.1[,3], new.online.1[,4], main="New Online", xlab="search.ratio", ylab="PD_C.count")
plot(new.online.1[,3], new.online.1[,5], main="New Online", xlab="search.ratio", ylab="mobile.ratio")
plot(new.online.1[,3], new.online.1[,6], main="New Online", xlab="search.ratio", ylab="digital")

plot(new.online.1[,4], new.online.1[,5], main="New Online", xlab="PD_C.count", ylab="mobile.ratio")
plot(new.online.1[,4], new.online.1[,6], main="New Online", xlab="PD_C.count", ylab="digital")

plot(new.online.1[,5], new.online.1[,6], main="New Online", xlab="mobile.ratio", ylab="digital") # 예쁘다 럭비공 모양 







## Calculate the distance measure
new.online.1.scaled <- scale(new.online.1)
d.new.online.1 <- dist(new.online.1.scaled, method="euclidean")

#####  Single-Linkage Method ####
# new.online.1
clust.new.online.1 <- hclust(d.new.online.1, method="single")
cl.new.online.1 <- cutree(clust.new.online.1, 4)
table(cl.new.online.1)


# Dendogram for new.online.1
#pdf("dendo_single_new.online.1.pdf", height=7, width=10)
plot(clust.new.online.1, hang=-1, main="(a) Cluster Dendrogram:  ONLINE")
rect.hclust(clust.new.online.1, 4)
#dev.off()

##### Ward's Method ####
# new.online.1
clust.new.online.1.ward <- hclust(d.new.online.1, method="ward.D")
cl.new.online.1.ward <- cutree(clust.new.online.1.ward, 2)
table(cl.new.online.1.ward)

## Dendogram for new.online.1
#pdf("dendo_ward_new.online.1.pdf", height=7, width=10)
plot(clust.new.online.1.ward, hang=-1, main="(a) Cluster Dendrogram: new.online.1")
rect.hclust(clust.new.online.1.ward, 2)
#dev.off()
str(clust.new.online.1.ward)


##### 여러 cluster를 실행해 보았으나, 잘 분류 안되는것 같고 우리는 outlier도 제거 햇으니 K-means 간다 ########

###### outlier(DIY가구 & 아이젠) 제거 #############
new.online.1 <- new.online[,2:7]
new.online.1 <- new.online.1[!(rownames(new.online.1)=="DIY가구"),]
new.online.1 <- new.online.1[!(rownames(new.online.1)=="아이젠"),]
new.online.1.scaled <- scale(new.online.1)

##### ( WSS )  How many clusters? K = 6으로 결정 # ##########
k.max <- 50
wss <- sapply(1:k.max, 
              function(k){kmeans(new.online.1.scaled, k, nstart=200, iter.max = 25 )$tot.withinss})
# wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares", main="elbow")

##### K-means without outliers #####
set.seed(41)
clust.new.online.1.k <- kmeans(new.online.1.scaled, 5, iter.max=30,nstart = 25)
clust.new.online.1.k$centers
round(clust.new.online.1.k$centers,3)
clust.new.online.1.k$size
clust.new.online.1.k$size[order(clust.new.online.1.k$size)]
# gr1 <- as.data.frame(clust.new.online.1.k$cluster)

# 어느 소분류가 어느 클러스터인지.
cluster.number <- clust.new.online.1.k$cluster
cluster.number <- as.data.frame(cluster.number)
cluster.number[,2] <- rownames(cluster.number)
colnames(cluster.number) <- c("cluster.number", "CLAC3_NM")
cluster.number <- dplyr::inner_join(online, cluster.number,by=c("CLAC3_NM"))
cluster.number <- cluster.number[,c(1,10)]
write.csv(cluster.number, file="0115 new K5 without DIY_furn N IZ.csv")   
# str(cluster.number)


# par(mfrow=c(1,2))
library(cluster)
# 시간이 매우 오래 걸림.
clusplot(new.online.1, clust.new.online.1.k$cluster, color=TRUE, shade=TRUE, labels=1, lines=0)
# install.packages("fpc")
library(fpc)
plotcluster(new.online.1, clust.new.online.1.k$cluster) 


##### INDEX TIME  #### 

round(clust.new.online.1.k$centers,3)
weight<- c(0.11,0.01,0.11,0.11,0.16,0.50) #by pairwise comparison
weight.cluster <- round(clust.new.online.1.k$centers,3) %*% weight
weight.cluster2 <- as.data.frame(unlist(weight.cluster)[1:5])
colnames(weight.cluster2) <- "cluster.weight"

# install.packages("scales")
library(scales)
rescale(weight.cluster2$cluster.weight, to=c(0.1,1))
weight.cluster2$rescale.weight <- rescale(weight.cluster2$cluster.weight, to=c(0.1,1))
sum(rescale(weight.cluster2$cluster.weight, to=c(0.1,1)))
weight.cluster2

weight.cluster2$cluster <- c(1,2,3,4,5)
weight.cluster2 <- weight.cluster2[,2:3]

#head(clust.newOnline.k.6$cluster)
gr <- as.data.frame(clust.new.online.1.k$cluster)
#head(gr)
gr$CLAC3_NM <- row.names(gr) #make column of CLAC3_NM 
colnames(gr)[1] <- "cluster" #change name to cluster
group <- session_master[,c("CLAC3_NM","CLAC2_NM","CLAC1_NM")] 
CLAC <- unique(group)

##### making category #####
categ1 <- c("남성의류","여성의류","유아동의류")
categ2 <- c("패션잡화")
categ3 <- c("화장품/뷰티케어","퍼스널케어")
categ4 <- c("영상/음향가전","컴퓨터","냉장/세탁가전","계절가전","자동차용품")
categ5 <- c("인테리어/조명","가구","침구/수예")
categ6 <- c("출산/육아용품","완구")
categ7 <- c("과일","음료","냉장식품","축산물","냉동식품")
categ8 <- c("아웃도어/레저","시즌스포츠","헬스/피트니스","구기/필드스포츠","스포츠패션")
categ9 <- c("문구/사무용품","주방잡화","속옷/양말/홈웨어","식기/조리기구","원예/애완","청소/세탁/욕실용품","생활/주방가전","세제/위생","건강식품")
# categ10 <- c("상품권","모바일")

CLAC$categ <- ifelse (CLAC$CLAC1_NM %in% categ1, "fashion.clothes"
                      , ifelse(CLAC$CLAC1_NM %in% categ2, "fashion.zap"
                               , ifelse(CLAC$CLAC1_NM %in% categ3, "beauty"
                                        , ifelse(CLAC$CLAC1_NM %in% categ4, "machine"
                                                 , ifelse(CLAC$CLAC1_NM %in% categ5, "furni.interior"
                                                          , ifelse(CLAC$CLAC1_NM %in% categ6, "baby"
                                                                   , ifelse(CLAC$CLAC1_NM %in% categ7, "food"
                                                                            , ifelse(CLAC$CLAC1_NM %in% categ8, "sports"
                                                                                     , ifelse(CLAC$CLAC1_NM %in% categ9, "life.health", "coupon")))))))))
group2 <- dplyr::inner_join(CLAC, gr,by="CLAC3_NM") #gr: cluster, CLAC3_NM ||| CLAC: unique(CLAC)
#head(group2)

#weight & cluster & CLAC
group3 <- dplyr::inner_join(group2,weight.cluster2,by="cluster") #
#head(group3)

##calculate index
index_clac2 <- with(group3, aggregate(rescale.weight, by=list(CLAC2_NM=CLAC2_NM,CLAC1_NM=CLAC1_NM,categ=categ),sum))
#head(index_clac2)
index_clac1 <- with(group3, aggregate(rescale.weight, by=list(CLAC1_NM=CLAC1_NM,categ=categ),sum))
#head(index_clac1)

#divide by numbers
clac.out <- c("남녀공용향수세트","DIY가구","DIY완구","아이젠") 
CLAC <- CLAC[!(CLAC$CLAC3_NM %in% clac.out),]
count.clac2 <- plyr::count(CLAC, c("CLAC1_NM","CLAC2_NM"))
count.clac1 <- plyr::count(CLAC, c("CLAC1_NM"))


###index by clac1
index_clac1 <- dplyr::inner_join(index_clac1,count.clac1,by="CLAC1_NM")
#head(index_clac1)
index_clac1$index <- index_clac1$x/index_clac1$freq
head(index_clac1[order(index_clac1$index,decreasing=TRUE),],20)
head(index_clac1[order(index_clac1$index),],20) #low rank

#head(index_clac1)

###index by category
count.categ <- plyr::count(index_clac1[,1:2],c("categ"))
index_categ <- with(index_clac1, aggregate(index, by=list(categ=categ),sum))
#identical(index_categ$categ,count.categ$categ)
index_categ <- dplyr::inner_join(index_categ,count.categ ,by="categ")
#head(index_categ)
index_categ$index <- index_categ$x / index_categ$freq
categ <- index_categ[,c(1,4)]
#rownames(categ) <- categ$categ
barplot(categ$index , main="ONLINE INDE BY MAIN CATEGORY",las=2, names.arg=categ$categ, col="#e65aaa", ylim=c(0,1))
categ[order(categ$index,decreasing=TRUE),]

######_______________________TREND START______________________ #####
##### palin 생성. #####
# 구매량 - 네이버 트렌드 카테고리별 SESS_DT 별로 구매 수량 .

# str(session_master)
# str(pro_master)
# str(CLAC)

pro_sess_master <- dplyr :: inner_join(session_master[,c(1,2,4)], pro_master[,c(1,2,4,8,10,11,12)], by=c("SESS_ID","CLNT_ID")) 
pro_sess_master <- dplyr :: inner_join(pro_sess_master, CLAC, by=c("CLAC1_NM", "CLAC2_NM", "CLAC3_NM"))
# str(pro_sess_master)
# length(unique(pro_sess_master$CLAC3_NM)) # 뭐 없어졌나 확인. 안 없어짐.

# 이 날에 카테고리별로 거래가 몇번 있는지.
# 이 날에 행 수.
palin <- with(pro_sess_master,aggregate(PD_BUY_CT, by=list(SESS_DT=SESS_DT, categ=categ), sum)) #sum of SEARCH_CNT by CLAC3_NM #CLAC3_NM, x
str(palin)
colnames(palin) <- c( "SESS_DT", "categ", "palin")


### The external regressors to produce a forecast for 7 days
# new furi coupon applied
# baby y diff7 

########################
####  ARIMA y only  ####
########################
library(tseries)
library(forecast)
# install.packages("tsoutliers")
library(tsoutliers)
#### 1 Fashion clothes ####
y.FC <- palin[palin$categ=="fashion.clothes",c(1,3)]
y.FC$SESS_DT <- as.character(y.FC$SESS_DT)
y.FC$SESS_DT <- as.Date(y.FC$SESS_DT, "%Y%m%d")
y.FC.ts <- ts(y.FC$palin, start=y.FC$SESS_DT[1], end = y.FC$SESS_DT[length(y.FC$palin)], freq=1)
anjung <- ((y.FC.ts)^(-1/2))/(-1/2)
station7.FC.y <- diff(((y.FC.ts)^(-1/2))/(-1/2),1)
y.FC.var <- station7.FC.y
# split the data
y.FC.train <- subset(station7.FC.y, end=length(station7.FC.y)-28)
y.FC.valid <- subset(station7.FC.y, start = length(station7.FC.y)-27, end = length(station7.FC.y)-7)
y.FC.test <- subset(station7.FC.y, start = length(station7.FC.y)-6, end = length(station7.FC.y))
y.FC.arima.train <- arima(y.FC.train, c(2,0,2), seasonal=list(order = c(1,0,1), period=7), include.mean = F) # -1741.27
acf(y.FC.arima.train$residuals)
y.FC.test.arima <- Arima(y.FC.valid, model=y.FC.arima.train)
accuracy(y.FC.test.arima)  # RMSE  0.001338672

#### 2 BABY  ####
y.baby <- palin[palin$categ=="baby",c(1,3)]
y.baby$SESS_DT <- as.character(y.baby$SESS_DT)
y.baby$SESS_DT <- as.Date(y.baby$SESS_DT, "%Y%m%d")
y.baby.ts <- ts(y.baby$palin, start=y.baby$SESS_DT[1], end = y.baby$SESS_DT[length(y.baby$palin)], freq=1)
anjung <- ((y.baby.ts)^(-1))/(-1)
station0.baby.y <- anjung

# station0 o?=o?= outlier 2
outliers_station0.baby.y <- tso(station0.baby.y, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_station0.baby.y
(outliers_idx <- outliers_station0.baby.y$outliers$ind)
outliers_station0.baby.y$outliers$time

# 1st outlier 
n <- length(station0.baby.y)
mo_tc <- outliers("AO", outliers_idx[1])
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_station0.baby.y$outliers["coefhat"][[1]])
tc_effect <- coefhat[1]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(station0.baby.y), start = start(station0.baby.y))
station0.baby.y.intervent1 <- station0.baby.y - tc_effect_ts

# 2nd outlier  
mo_tc <- outliers("TC", outliers_idx[2])
tc <- outliers.effects(mo_tc, n)
tc_effect <- coefhat[2]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(station0.baby.y), start = start(station0.baby.y))
station0.baby.y.intervent2 <- station0.baby.y.intervent1 - tc_effect_ts
# station0.baby.y.intervent2  <- diff(station0.baby.y.intervent2, 7)
# var 
y.baby.var <- station0.baby.y.intervent2
# split the data
y.baby.train <- subset(station0.baby.y.intervent2, end=length(station0.baby.y.intervent2)-28)
y.baby.valid <- subset(station0.baby.y.intervent2, start = length(station0.baby.y.intervent2)-27, end = length(station0.baby.y.intervent2)-7)
y.baby.test <- subset(station0.baby.y.intervent2, start = length(station0.baby.y.intervent2)-6, end = length(station0.baby.y.intervent2))
y.baby.arima.train <- arima(y.baby.train, c(0,0,1), seasonal=list(order = c(1,0,0), period=7), include.mean = T)
# revised order c(1,0,1)
y.baby.test.arima <- Arima(y.baby.valid, model=y.baby.arima.train)
accuracy(y.baby.test.arima)  # RMSE 0.0001191859  # revised


#### 3 ?P<???H- #####
y.FZ <- palin[palin$categ=="fashion.zap",c(1,3)]
y.FZ$SESS_DT <- as.character(y.FZ$SESS_DT)
y.FZ$SESS_DT <- as.Date(y.FZ$SESS_DT, "%Y%m%d")
y.FZ.ts <- ts(y.FZ$palin, start=y.FZ$SESS_DT[1], end = y.FZ$SESS_DT[length(y.FZ$palin)], freq=1)
anjung <- y.FZ.ts
station7.FZ.y <- diff(anjung,7)
y.FZ.var <- station7.FZ.y
# split the data
y.FZ.train <- subset(station7.FZ.y, end=length(station7.FZ.y)-28)
y.FZ.valid <- subset(station7.FZ.y, start = length(station7.FZ.y)-27, end = length(station7.FZ.y)-7)
y.FZ.test <- subset(station7.FZ.y, start = length(station7.FZ.y)-6, end = length(station7.FZ.y))
y.FZ.arima.train <- arima(y.FZ.train, c(2,0,1), seasonal=list(order = c(1,0,1), period = 7),include.mean = T)
y.FZ.test.arima <- Arima(y.FZ.valid, model=y.FZ.arima.train)
accuracy(y.FZ.test.arima)  # RMSE 1282.862 / 2.546817e-05


#### 4 Sports  #####
y.sports <- palin[palin$categ=="sports",c(1,3)]
y.sports$SESS_DT <- as.character(y.sports$SESS_DT)
y.sports$SESS_DT <- as.Date(y.sports$SESS_DT, "%Y%m%d")
y.sports.ts <- ts(y.sports$palin, start=y.sports$SESS_DT[1], end = y.sports$SESS_DT[length(y.sports$palin)], freq=1)
anjung <- ((y.sports.ts)^(-1))/(-1)
station7.sports.y <- diff(anjung,7)
y.sports.var <- station7.sports.y
# split the data
y.sports.train <- subset(station7.sports.y, end=length(station7.sports.y)-28)
y.sports.valid <- subset(station7.sports.y, start = length(station7.sports.y)-27, end = length(station7.sports.y)-7)
y.sports.test <- subset(station7.sports.y, start = length(station7.sports.y)-6, end = length(station7.sports.y))
y.sports.arima.train <- arima(y.sports.train, c(1,0,0), seasonal=list(order = c(0,0,1), period = 7), include.mean = F)
y.sports.test.arima <- Arima(y.sports.valid, model=y.sports.arima.train)
accuracy(y.sports.test.arima)  # RMSE 3.232621e-05


#### 5 Machine ####
y.machine <- palin[palin$categ=="machine",c(1,3)]
y.machine$SESS_DT <- as.character(y.machine$SESS_DT)
y.machine$SESS_DT <- as.Date(y.machine$SESS_DT, "%Y%m%d")
y.machine <- ts(y.machine$palin, start=y.machine$SESS_DT[1], end = y.machine$SESS_DT[length(y.machine$palin)], freq=1)
station7.machine.y <- diff(y.machine,7)

#### station7 outlier 5?? ####
outliers_station7.machine.y <- tso(station7.machine.y, types = c("TC", "AO", "LS", "IO", "SLS"), maxit.iloop = 3, maxit.oloop = 3) #7???? ???O8? ???T5? ???W$?
outliers_idx <- outliers_station7.machine.y$outliers$ind
coefhat <- as.numeric(outliers_station7.machine.y$outliers["coefhat"][[1]])
#### outlier 5, AO TC TC IO AO 
# 1st outlier
n <- length(station7.machine.y)
mo_ao <- outliers("AO", outliers_idx[1])
ao <- outliers.effects(mo_ao, n)
ao_effect <- coefhat[1]*ao
ao_effect_ts <- ts(ao_effect, frequency = frequency(station7.machine.y), start = start(station7.machine.y))
intervent1 <- station7.machine.y - ao_effect_ts
# 2nd outlier TC 
mo_tc <- outliers("TC", outliers_idx[2])
tc <- outliers.effects(mo_tc, n)
tc_effect <- coefhat[2]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(station7.machine.y), start = start(station7.machine.y))
intervent2 <- intervent1 - tc_effect_ts
# 3rd outlier TC 
mo_tc <- outliers("TC", outliers_idx[3])
tc <- outliers.effects(mo_tc, n)
tc_effect <- coefhat[3]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(station7.machine.y), start = start(station7.machine.y))
intervent3 <- intervent2 - tc_effect_ts
# 4th outlier IO 
JB <- arima(station7.machine.y, c(0,0,2))
pars <- coefs2poly(JB)
mo_io <- outliers("IO", outliers_idx[4])
io <- outliers.effects(mo_io, pars=list(arcoefs= pars$arcoefs, macoefs=pars$macoefs), n)
io_effect <- coefhat[4]*io
io_effect_ts <- ts(io_effect, frequency = frequency(station7.machine.y), start = start(station7.machine.y))
intervent4 <- intervent3 - io_effect_ts
# 5th outlier AO 
mo_ao <- outliers("AO", outliers_idx[5])
ao <- outliers.effects(mo_ao, n)
ao_effect <- coefhat[5]*ao
ao_effect_ts <- ts(ao_effect, frequency = frequency(station7.machine.y), start = start(station7.machine.y))
diff7_intervent5 <- intervent4 - ao_effect_ts

y.machine.var <- diff7_intervent5

# split the data
y.machine.train <- subset(diff7_intervent5, end=length(diff7_intervent5)-28)
y.machine.valid <- subset(diff7_intervent5, start = length(diff7_intervent5)-27, end = length(diff7_intervent5)-7)
y.machine.test <- subset(diff7_intervent5, start = length(diff7_intervent5)-6, end = length(diff7_intervent5))
y.machine.arima.train <- arima(y.machine.train, c(1,0,0), seasonal=list(order = c(1,0,0), period = 7))
y.machine.test.arima <- Arima(y.machine.valid, model=y.machine.arima.train)
accuracy(y.machine.test.arima)  # RMSE  149.2325

#### 6 Furi  ####
y.furi <- palin[palin$categ=="furni.interior",]
y.furi$SESS_DT <- as.Date(as.character(y.furi$SESS_DT), "%Y%m%d")
y.furi.ts <- ts(y.furi$palin , start=y.furi$SESS_DT[1], end = y.furi$SESS_DT[length(y.furi$palin)], freq=1)
y.furi.ts <- log(y.furi.ts)
station7.furi.y <- diff(y.furi.ts,7) #???????? 7 ?G=?
##### Furi  outlier 1?? #####

#rm outliers
outliers_y.furi7 <- tso(station7.furi.y, types = c("TC", "AO", "LS", "IO", "SLS")) #7???? ???O8? ???T5? ???W$?
plot(outliers_y.furi7) # ?_<?
outliers_idx <- outliers_y.furi7$outliers$ind

# 1st outlier
n <- length(station7.furi.y)
mo_tc <- outliers("AO", outliers_idx[1])
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_y.furi7$outliers["coefhat"][[1]])
tc_effect <- coefhat[1]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(station7.furi.y), start = start(station7.furi.y))
intervent1 <- station7.furi.y - tc_effect_ts
station7.furi.y <- intervent1

y.furi.var <- station7.furi.y
# split the data
y.furi.train <- subset(station7.furi.y, end=length(station7.furi.y)-28)
y.furi.valid <- subset(station7.furi.y, start = length(station7.furi.y)-27, end = length(station7.furi.y)-7)
y.furi.test <- subset(station7.furi.y, start = length(station7.furi.y)-6, end = length(station7.furi.y))
y.furi.arima.train <- arima(y.furi.train, c(1,0,1), seasonal=list(order = c(0,0,1), period=7), include.mean = F) 
pacf(y.furi.arima.train$residuals)
y.furi.test.arima <- Arima(y.furi.valid, model=y.furi.arima.train)
accuracy(y.furi.test.arima)  # RMSE 0.6969105

#### 7 coupon ####
y.coupon <- palin[palin$categ=="coupon",c(1,3)]
str(y.coupon)
y.coupon$SESS_DT <- as.character(y.coupon$SESS_DT)
y.coupon$SESS_DT <- as.Date(y.coupon$SESS_DT, "%Y%m%d")
y.coupon <- ts(y.coupon$palin, start=y.coupon$SESS_DT[1], end = y.coupon$SESS_DT[length(y.furi$palin)], freq=1)
station7.coupon.y <- diff(log(y.coupon),7) #???????? 7 ?G=?
### station7 outlier 3 ####
outliers_y.coupon7 <- tso(station7.coupon.y, types = c("TC", "AO", "LS", "IO", "SLS"), maxit.iloop = 3, maxit.oloop = 3) 
outliers_idx <- outliers_y.coupon7$outliers$ind

# 1st outlier 
n <- length(station7.coupon.y)
mo_tc <- outliers("AO", outliers_idx[1])
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_y.coupon7$outliers["coefhat"][[1]])
tc_effect <- coefhat[1]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(station7.coupon.y), start = start(station7.coupon.y))
station7.coupon.y.intervent1 <- station7.coupon.y - tc_effect_ts
#plot(cbind(station7.coupon.y, intervent1, tc_effect_ts))
#plot(station7.coupon.y, type ='b', ylab = "excess birth ratio")
#lines(intervent1, col = 'red', lty = 3, type ='b')

# 2nd outlier 
mo_tc <- outliers("AO", outliers_idx[2])
tc <- outliers.effects(mo_tc, n)
tc_effect <- coefhat[2]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(station7.coupon.y), start = start(station7.coupon.y))
station7.coupon.y.intervent2 <- station7.coupon.y.intervent1 - tc_effect_ts

# 3rd outlier 
mo_tc <- outliers("TC", outliers_idx[3])
tc <- outliers.effects(mo_tc, n)
tc_effect <- coefhat[3]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(station7.coupon.y), start = start(station7.coupon.y))
station7.coupon.y.intervent3 <- station7.coupon.y.intervent2 - tc_effect_ts

y.coupon.var <- station7.coupon.y.intervent3
# split the data
y.coupon.train <- subset(station7.coupon.y.intervent3, end=length(station7.coupon.y.intervent3)-28)
y.coupon.valid <- subset(station7.coupon.y.intervent3, start = length(station7.coupon.y.intervent3)-27, end = length(station7.coupon.y.intervent3)-7)
y.coupon.test <- subset(station7.coupon.y.intervent3, start = length(station7.coupon.y.intervent3)-6, end = length(station7.coupon.y.intervent3))
y.coupon.arima.train <- arima(y.coupon.train, c(1,0,2), seasonal=list(order = c(0,0,1), period=7), include.mean = F) 
y.coupon.test.arima <- Arima(y.coupon.valid, model=y.coupon.arima.train)
accuracy(y.coupon.test.arima)  # RMSE 0.2657184 


#### 8 Life Health  #####
y.LH <- palin[palin$categ=="life.health",c(1,3)]
y.LH$SESS_DT <- as.character(y.LH$SESS_DT)
y.LH$SESS_DT <- as.Date(y.LH$SESS_DT, "%Y%m%d")
y.LH.ts <- ts(y.LH$palin, start=y.LH$SESS_DT[1], end = y.LH$SESS_DT[length(y.LH$palin)], freq=1)
station7.LH.y <- diff(y.LH.ts,7)
### station7 o?=o?= outlier 3
outliers_station7.LH.y <- tso(station7.LH.y, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_idx <- outliers_station7.LH.y$outliers$ind
### 1st outlier 
n <- length(station7.LH.y)
mo_tc <- outliers("AO", outliers_idx[1])
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_station7.LH.y$outliers["coefhat"][[1]])
tc_effect <- coefhat[1]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(station7.LH.y), start = start(station7.LH.y))
station7.LH.intervent1 <- station7.LH.y - tc_effect_ts

### 2nd outlier 
n <- length(station7.LH.y)
mo_tc <- outliers("AO", outliers_idx[2])
tc <- outliers.effects(mo_tc, n)
# coefhat <- as.numeric(outliers_station7.LH.y$outliers["coefhat"][[1]])
tc_effect <- coefhat[2]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(station7.LH.y), start = start(station7.LH.y))
station7.LH.intervent2 <- station7.LH.intervent1 - tc_effect_ts


### 3rd outlier 
n <- length(station7.LH.y)
mo_tc <- outliers("TC", outliers_idx[3])
tc <- outliers.effects(mo_tc, n)
# coefhat <- as.numeric(outliers_station7.LH.y$outliers["coefhat"][[1]])
tc_effect <- coefhat[3]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(station7.LH.y), start = start(station7.LH.y))
station7.LH.intervent3 <- station7.LH.intervent2 - tc_effect_ts

y.LH.var <- station7.LH.intervent3
# split the set
y.LH.train <- subset(station7.LH.intervent3, end=length(station7.LH.intervent3)-28)
y.LH.valid <- subset(station7.LH.intervent3, start = length(station7.LH.intervent3)-27, end = length(station7.LH.intervent3)-7)
y.LH.test <- subset(station7.LH.intervent3, start = length(station7.LH.intervent3)-6, end = length(station7.LH.intervent3))
y.LH.arima.train <-  arima(y.LH.train, c(1,0,0), seasonal=list(order = c(1,0,1), period=7), include.mean = T)
y.LH.test.arima <- Arima(y.LH.valid, model=y.LH.arima.train)
accuracy(y.LH.test.arima)  # RMSE 5665.498

#### 9 beauty  ####
y.beauty <- palin[palin$categ=="beauty",c(1,3)]
y.beauty$SESS_DT <- as.character(y.beauty$SESS_DT)
y.beauty$SESS_DT <- as.Date(y.beauty$SESS_DT, "%Y%m%d")
y.beauty.ts <- ts(y.beauty$palin, start=y.beauty$SESS_DT[1], end = y.beauty$SESS_DT[length(y.beauty$palin)], freq=1)
anjung <- ((y.beauty.ts)^(-1))/(-1)
station0.beauty.y <- anjung

y.beauty.var <- station0.beauty.y
## split the data
y.beauty.train <- subset(station0.beauty.y, end=length(station0.beauty.y)-28)
y.beauty.valid <- subset(station0.beauty.y, start = length(station0.beauty.y)-27, end = length(station0.beauty.y)-7)
y.beauty.test <- subset(station0.beauty.y, start = length(station0.beauty.y)-6, end = length(station0.beauty.y))
y.beauty.arima.train <- arima(y.beauty.train, order = c(2,0,0), include.mean = T)
acf(y.beauty.arima.train$residuals)
y.beauty.test.arima <- Arima(y.beauty.valid, model=y.beauty.arima.train)
accuracy(y.beauty.test.arima)  # RMSE 2.996743e-05

#### 10 food  ####
y.food <- palin[palin$categ=="food",c(1,3)]
y.food$SESS_DT <- as.character(y.food$SESS_DT)
y.food$SESS_DT <- as.Date(y.food$SESS_DT, "%Y%m%d")
y.food.ts <- ts(y.food$palin, start=y.food$SESS_DT[1], end = y.food$SESS_DT[length(y.food$palin)], freq=1)
station1.food.y <- diff(log(y.food.ts))


y.food.var <- station1.food.y
## split the data
y.food.train <- subset(station1.food.y, end=length(station1.food.y)-28)
y.food.valid <- subset(station1.food.y, start = length(station1.food.y)-27, end = length(station1.food.y)-7)
y.food.test <- subset(station1.food.y, start = length(station1.food.y)-6, end = length(station1.food.y))
y.food.arima.train <- arima(y.food.train, c(1,0,1), seasonal=list(order = c(1,0,0), period=7), include.mean = F)
y.food.test.arima <- Arima(y.food.valid, model=y.food.arima.train)
accuracy(y.food.test.arima)  # RMSE 0.7313127









####################
#####   newC   #####
####################

#### making CLAC and newC####
###making category
categ1 <- c("남성의류","여성의류","유아동의류")
categ2 <- c("패션잡화")
categ3 <- c("화장품/뷰티케어","퍼스널케어")
categ4 <- c("영상/음향가전","컴퓨터","냉장/세탁가전","계절가전","자동차용품")
categ5 <- c("인테리어/조명","가구","침구/수예")
categ6 <- c("출산/육아용품","완구")
categ7 <- c("과일","음료","냉장식품","축산물","냉동식품")
categ8 <- c("아웃도어/레저","시즌스포츠","헬스/피트니스","구기/필드스포츠","스포츠패션")
categ9 <- c("문구/사무용품","주방잡화","속옷/양말/홈웨어","식기/조리기구","원예/애완","청소/세탁/욕실용품","생활/주방가전","세제/위생","건강식품")
# categ10 <- c("상품권","모바일")
CLAC$categ <- ifelse (CLAC$CLAC1_NM %in% categ1, "fashion.clothes"
                      , ifelse(CLAC$CLAC1_NM %in% categ2, "fashion.zap"
                               , ifelse(CLAC$CLAC1_NM %in% categ3, "beauty"
                                        , ifelse(CLAC$CLAC1_NM %in% categ4, "machine"
                                                 , ifelse(CLAC$CLAC1_NM %in% categ5, "furni.interior"
                                                          , ifelse(CLAC$CLAC1_NM %in% categ6, "baby"
                                                                   , ifelse(CLAC$CLAC1_NM %in% categ7, "food"
                                                                            , ifelse(CLAC$CLAC1_NM %in% categ8, "sports"
                                                                                     , ifelse(CLAC$CLAC1_NM %in% categ9, "life.health", "coupon")))))))))

newC <- session_master[session_master$SESS_SEQ == 1,]
newC2 <- newC[,c("SESS_DT","CLAC1_NM")]
newC3 <- dplyr::inner_join(newC2,CLAC,by=("CLAC1_NM"))
newC4 <- plyr::count(newC3,c("SESS_DT","categ"))
#### 1. newC FC #####
unique(newC4$categ)
newC.FC <-newC4[newC4$categ=="fashion.clothes",]
newC.FC$SESS_DT<- as.character(newC.FC$SESS_DT)
newC.FC$SESS_DT <- as.Date(as.character(newC.FC$SESS_DT), "%Y%m%d")
newC.FC <- newC.FC[,c(1,3)]
newC.FC.ts <- ts(newC.FC$freq, start=newC.FC$SESS_DT[1], end = newC.FC$SESS_DT[length(newC.FC$freq)], freq=1)

## raw_newC.FC 
par(mfrow=c(1,1))
plot(newC.FC.ts,main="newC.FC - diff=0",col="blue") 
par(new=T)
plot(newC.FC$freq)
# # text(newC.FC$freq, labels=c(rep(c("??","??","H-","??","??","??","??"),20)),cex=1) 
adf.test(newC.FC.ts, alternative="stationary", k=0) 

## -1/2 ?? ?????V>?o?=o?=  
plot(((newC.FC.ts)^(-1/2))/(-1/2),main="y.FC - diff=0, -1",col="blue")
par(new=T)
# plot(((y.FC$palin)^(-1))/(-1))
# # text(((newC.FC.ts)^(-1/2))/(-1/2), labels=c(rep(c("??","??","H-","??","??","??","??"),20)),cex=1) # ?????O?? ???? ???? ?????O?? ?H;?.
adf.test(((newC.FC.ts)^(-1/2))/(-1/2), alternative="stationary") # p-value=0.9
station.newC.FC.ts <- ((newC.FC.ts)^(-1/2))/(-1/2)


## ???? H.?? 

# install.packages("tsoutliers")
# install.packages("TSA")
# install.packages("lmtest")
# install.packages("astsa")
# install.packages("FitARMA")
# install.packages("strucchange")
# install.packages("reshape")
# install.packages("Rmisc")
# install.packages("fBasics")
# install.packages("forcast")
# install.packages("fUnitRoots")
# suppressPackageStartupMessages(library(forecast))
# suppressPackageStartupMessages(library(fUnitRoots))
# suppressPackageStartupMessages(library(FitARMA))
# suppressPackageStartupMessages(library(strucchange))
# suppressPackageStartupMessages(library(reshape))
# suppressPackageStartupMessages(library(Rmisc))
# suppressPackageStartupMessages(library(fBasics))
# suppressPackageStartupMessages(library(TSA))
# suppressPackageStartupMessages(library(astsa))
# suppressPackageStartupMessages(library(tsoutliers))
suppressPackageStartupMessages(library(lmtest))

## station.newC.FC.ts o?=o?= outlier 1??.
outliers_newC.FC.ts <- tso(station.newC.FC.ts, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_newC.FC.ts
plot(outliers_newC.FC.ts)
outliers_newC.FC.ts$outliers
(outliers_idx <- outliers_newC.FC.ts$outliers$ind)
outliers_newC.FC.ts$outliers$time

# 1st outlier TC
n <- length(station.newC.FC.ts)
mo_tc <- outliers("TC", outliers_idx[1])
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_newC.FC.ts$outliers["coefhat"][[1]])
tc_effect <- coefhat[1]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(newC.FC.ts), start = start(newC.FC.ts))
newC.FC.ts.intervent1 <- station.newC.FC.ts - tc_effect_ts
par(mfrow=c(1,1))
plot(cbind(station.newC.FC.ts, newC.FC.ts.intervent1, tc_effect_ts))
plot(station.newC.FC.ts, type ='b', ylab = "excess birth ratio")
lines(newC.FC.ts.intervent1, col = 'red', lty = 3, type ='b')


# ???? o?=o?=???? adf test. 
par(mfrow=c(1,1))
plot(newC.FC.ts.intervent1,main="newC.FC.intervent5 diff=0",col="blue") # ?O4? ???? plot
par(new=T)
# plot(newC.FC$freq,main="newC.FC.intervent5 diff=0",col="blue") # ?O4? ???? plot
# # text(newC.FC.ts.intervent1, labels=c(rep(c("??","??","H-","??","??","??","??"),20)),cex=1) #
adf.test(newC.FC.ts.intervent1, alternative="stationary", k=0) # o?=o?=???? ?????? ??o?=o?=?? ????.
# ???????? ?V>????L1? ?O4B5?. ????o?=o?= y?? ?????? ?O4O1? ?W4??? ????
#### var 
newC.FC.var <- newC.FC.ts.intervent1
#___________________ [??o?=o?= ????] newC.FC.ts.intervent1 : -1/2?????V0? ???? ?Q0? ?????V>?o?=o?=!!
#### 2. newC baby #####
newC.baby <-newC4[newC4$categ=="baby",]
newC.baby$SESS_DT<- as.character(newC.baby$SESS_DT)
newC.baby$SESS_DT <- as.Date(as.character(newC.baby$SESS_DT), "%Y%m%d")
newC.baby <- newC.baby[,c(1,3)]
newC.baby.ts <- ts(newC.baby$freq, start=newC.baby$SESS_DT[1], end = newC.baby$SESS_DT[length(newC.baby$freq)], freq=1)

## raw_newC.baby 
par(mfrow=c(1,1))
plot(newC.baby.ts,main="newC.baby - diff=0",col="blue") # ?O4? ???? plot
par(new=T)
plot(newC.baby$freq)
# # text(newC.baby$freq, labels=c(rep(c("??","??","H-","??","??","??","??"),20)),cex=1) # ???????? ?V>????L1? ?O4B5?.
adf.test(newC.baby.ts, alternative="stationary", k=0) # o?=o?=???? ?????? ??o?=o?=?? ????.

## ???? H.?? 
## newC.baby.ts o?=o?= outlier ??o?=o?= 
outliers_newC.baby.ts <- tso(newC.baby.ts, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_newC.baby.ts
outliers_newC.baby.ts <- tso(((newC.baby.ts)^(-1/2))/(-1/2), types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_newC.baby.ts

## -1/2 
plot(((newC.baby.ts)^(-1/2))/(-1/2),main="y.baby - diff=0, -1",col="blue")
par(new=T)
# plot(((y.baby$palin)^(-1))/(-1))
# # text(((newC.baby.ts)^(-1/2))/(-1/2), labels=c(rep(c("??","??","H-","??","??","??","??"),20)),cex=1) # ?????O?? ???? ???? ?????O?? ?H;?.
adf.test(((newC.baby.ts)^(-1/2))/(-1/2), alternative="stationary") # o?=o?=???? ?? ?????? ??o?=o?=?? ????. p-value=0.9


## -1/2newC.baby.ts o?=o?= outlier 2?? -- ?_<?????o?=o?=..
outliers_newC.baby.ts <- tso(((newC.baby.ts)^(-1/2))/(-1/2), types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_newC.baby.ts
plot(outliers_newC.baby.ts)
outliers_newC.baby.ts$outliers
(outliers_idx <- outliers_newC.baby.ts$outliers$ind)
outliers_newC.baby.ts$outliers$time

## TC AO  
# 1st outlier TC
n <- length(newC.baby.ts)
mo_tc <- outliers("TC", outliers_idx[1])
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_newC.baby.ts$outliers["coefhat"][[1]])
tc_effect <- coefhat[1]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(newC.baby.ts), start = start(newC.baby.ts))
newC.baby.ts.intervent1 <- ((newC.baby.ts)^(-1/2))/(-1/2) - tc_effect_ts
par(mfrow=c(1,1))
plot(cbind(((newC.baby.ts)^(-1/2))/(-1/2), newC.baby.ts.intervent1, tc_effect_ts))
plot(((newC.baby.ts)^(-1/2))/(-1/2), type ='b', ylab = "excess birth ratio")
lines(newC.baby.ts.intervent1, col = 'red', lty = 3, type ='b')

# 2nd outlier AO
n <- length(newC.baby.ts)
mo_tc <- outliers("AO", outliers_idx[2])
tc <- outliers.effects(mo_tc, n)
# coefhat <- as.numeric(outliers_newC.baby.ts$outliers["coefhat"][[1]])
tc_effect <- coefhat[2]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(newC.baby.ts), start = start(newC.baby.ts))
newC.baby.ts.intervent2 <- newC.baby.ts.intervent1 - tc_effect_ts
par(mfrow=c(1,1))
plot(cbind(((newC.baby.ts)^(-1/2))/(-1/2), newC.baby.ts.intervent1, tc_effect_ts))
plot(((newC.baby.ts)^(-1/2))/(-1/2), type ='b', ylab = "excess birth ratio")
lines(newC.baby.ts.intervent2, col = 'red', lty = 3, type ='b')
#### var 
newC.baby.var <- newC.baby.ts.intervent2
#____________________ [??o?=o?= ????] newC.baby.ts.intervent2 : -1/2?????V0? ???? ?N0? ?????V>?o?=o?=!!
#### 3. newC FZ #####
newC.FZ <-newC4[newC4$categ=="fashion.zap",]
newC.FZ$SESS_DT<- as.character(newC.FZ$SESS_DT)
newC.FZ$SESS_DT <- as.Date(as.character(newC.FZ$SESS_DT), "%Y%m%d")
newC.FZ <- newC.FZ[,c(1,3)]
newC.FZ.ts <- ts(newC.FZ$freq, start=newC.FZ$SESS_DT[1], end = newC.FZ$SESS_DT[length(newC.FZ$freq)], freq=1)

## raw_newC.FZ 
par(mfrow=c(1,1))
plot(newC.FZ.ts,main="newC.FZ - diff=0",col="blue") # ?O4? ???? plot
par(new=T)
plot(newC.FZ$freq)
# # text(newC.FZ$freq, labels=c(rep(c("??","??","H-","??","??","??","??"),20)),cex=1) # ???????? ?V>????L1? ?O4B5?.
adf.test(newC.FZ.ts, alternative="stationary", k=0) # o?=o?=???? ?????? ??o?=o?=?? ????.

## newC.FZ.ts o?=o?= outlier 3?? 
outliers_newC.FZ.ts <- tso(newC.FZ.ts, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_newC.FZ.ts

plot(outliers_newC.FZ.ts)
outliers_newC.FZ.ts$outliers
(outliers_idx <- outliers_newC.FZ.ts$outliers$ind)
outliers_newC.FZ.ts$outliers$time

## IO AO LS  
# 1st outlier IO
# IO outlier?? ???Y7N???C4?
auto.arima(newC.FZ.ts)
JB <- arima(newC.FZ.ts, c(1,0,1), seasonal=list(order = c(0,1,1), period = 7))
JB <- arima(newC.FZ.ts, c(1,1,1))
# JB
# coeftest(JB)
pars <- coefs2poly(JB)
mo_tc <- outliers("IO", outliers_idx[1])
# pars?? ?V4? ??o?=o?= ???? arcoefs?? macoefs?? ??o?=o?=??????. ?W3? ??o?=o?=?O1? ?o?=o?=???.
pars
tc <- outliers.effects(mo_tc, pars=list(arcoefs=pars$arcoefs ,macoefs=pars$macoefs), n)
coefhat <- as.numeric(outliers_newC.FZ.ts$outliers["coefhat"][[1]])
tc_effect <- coefhat[1]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(newC.FZ.ts), start = start(newC.FZ.ts))
newC.FZ.ts.intervent1 <- newC.FZ.ts - tc_effect_ts
par(mfrow=c(1,1))
plot(cbind(newC.FZ.ts, newC.FZ.ts.intervent1, tc_effect_ts))
plot(newC.FZ.ts, type ='b', ylab = "excess birth ratio")
lines(newC.FZ.ts.intervent1, col = 'red', lty = 3, type ='b')


# 2nd outlier AO
n <- length(newC.FZ.ts)
mo_tc <- outliers("AO", outliers_idx[2])
tc <- outliers.effects(mo_tc, n)
# coefhat <- as.numeric(outliers_newC.FZ.ts$outliers["coefhat"][[1]])
tc_effect <- coefhat[2]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(newC.FZ.ts), start = start(newC.FZ.ts))
newC.FZ.ts.intervent2 <- newC.FZ.ts.intervent1 - tc_effect_ts
par(mfrow=c(1,1))
plot(cbind(newC.FZ.ts, newC.FZ.ts.intervent2, tc_effect_ts))
plot(newC.FZ.ts, type ='b', ylab = "newC.FZ.ts")
lines(newC.FZ.ts.intervent2, col = 'red', lty = 3, type ='b')

# 3rd outlier LS
n <- length(newC.FZ.ts)
mo_tc <- outliers("LS", outliers_idx[3])
tc <- outliers.effects(mo_tc, n)
# coefhat <- as.numeric(outliers_newC.FZ.ts$outliers["coefhat"][[1]])
tc_effect <- coefhat[3]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(newC.FZ.ts), start = start(newC.FZ.ts))
newC.FZ.ts.intervent3 <- newC.FZ.ts.intervent2 - tc_effect_ts
par(mfrow=c(1,1))
plot(cbind(newC.FZ.ts, newC.FZ.ts.intervent3, tc_effect_ts))
plot(newC.FZ.ts, type ='b', ylab = "newC.FZ.ts")
lines(newC.FZ.ts.intervent3, col = 'blue', lty = 3, type ='b')
#### var 
newC.FZ.var <- newC.FZ.ts.intervent3
#____________________ [??o?=o?= ????] newC.FZ.ts.intervent3 : ?F9??M5? ???O0? ???? 3?? ?????V>?o?=o?=!!
#### 4. newC sports #####
newC.sports <-newC4[newC4$categ=="sports",]
newC.sports$SESS_DT<- as.character(newC.sports$SESS_DT)
newC.sports$SESS_DT <- as.Date(as.character(newC.sports$SESS_DT), "%Y%m%d")
newC.sports <- newC.sports[,c(1,3)]
newC.sports.ts <- ts(newC.sports$freq, start=newC.sports$SESS_DT[1], end = newC.sports$SESS_DT[length(newC.sports$freq)], freq=1)

## raw_newC.sports 
par(mfrow=c(1,1))
plot(newC.sports.ts,main="newC.sports - diff=0",col="blue") # ?O4? ???? plot
par(new=T)
plot(newC.sports$freq)
# # text(newC.sports$freq, labels=c(rep(c("??","??","H-","??","??","??","??"),20)),cex=1) # ???????? ?V>????L1? ?O4B5?.
adf.test(newC.sports.ts, alternative="stationary", k=0) # o?=o?=???? ?????? ??o?=o?=?? ????.

## -1/2 ?? ?????V>?o?=o?=  
plot(((newC.sports.ts)^(-1/2))/(-1/2),main="y.sports - diff=0, -1",col="blue")
par(new=T)
# plot(((y.sports$palin)^(-1))/(-1))
# # text(((newC.sports.ts)^(-1/2))/(-1/2), labels=c(rep(c("??","??","H-","??","??","??","??"),20)),cex=1) # ?????O?? ???? ???? ?????O?? ?H;?.
adf.test(((newC.sports.ts)^(-1/2))/(-1/2), alternative="stationary") # o?=o?=???? ?? ?????? ??o?=o?=?? ????. p-value=0.9
station.newC.sports.ts <- ((newC.sports.ts)^(-1/2))/(-1/2)
# station.newC.sports.ts <- ((newC.sports.ts)^(-1))/(-1)
# station.newC.sports.ts <- newC.sports.ts


## ???? H.?? 
## station.newC.sports.ts o?=o?= outlier 1??.
outliers_newC.sports.ts <- tso(station.newC.sports.ts, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_newC.sports.ts
plot(outliers_newC.sports.ts)
outliers_newC.sports.ts$outliers
(outliers_idx <- outliers_newC.sports.ts$outliers$ind)
outliers_newC.sports.ts$outliers$time

##  AO 
# 1st outlier AO
n <- length(station.newC.sports.ts)
mo_tc <- outliers("AO", outliers_idx[1])
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_newC.sports.ts$outliers["coefhat"][[1]])
tc_effect <- coefhat[1]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(newC.sports.ts), start = start(newC.sports.ts))
newC.sports.ts.intervent1 <- station.newC.sports.ts - tc_effect_ts
par(mfrow=c(1,1))
plot(cbind(station.newC.sports.ts, newC.sports.ts.intervent1, tc_effect_ts))
plot(station.newC.sports.ts, type ='b', ylab = "excess birth ratio")
lines(newC.sports.ts.intervent1, col = 'red', lty = 3, type ='b')


# ???? o?=o?=???? adf test. 
par(mfrow=c(1,1))
plot(newC.sports.ts.intervent1,main="newC.sports.intervent1 diff=0 -1/2",col="blue") # ?O4? ???? plot
par(new=T)
# plot(newC.sports$freq,main="newC.sports.intervent5 diff=0",col="blue") # ?O4? ???? plot
# # text(newC.sports.ts.intervent1, labels=c(rep(c("??","??","H-","??","??","??","??"),20)),cex=1) #
adf.test(newC.sports.ts.intervent1, alternative="stationary", k=0) # o?=o?=???? ?????? ??o?=o?=?? ????.
#### var 
newC.sports.var <- newC.sports.ts.intervent1
#___________________ [??o?=o?= ????] newC.sports.ts.intervent1 : -1/2?????V0? ???? ?Q0? ?????V>?o?=o?=!!
#### 5. newC machine #####
newC.machine <-newC4[newC4$categ=="machine",]
newC.machine$SESS_DT<- as.character(newC.machine$SESS_DT)
newC.machine$SESS_DT <- as.Date(as.character(newC.machine$SESS_DT), "%Y%m%d")
newC.machine <- newC.machine[,c(1,3)]
newC.machine.ts <- ts(newC.machine$freq, start=newC.machine$SESS_DT[1], end = newC.machine$SESS_DT[length(newC.machine$freq)], freq=1)

## raw_newC.machine 
par(mfrow=c(1,1))
plot(newC.machine.ts,main="newC.machine - diff=0",col="blue") # ?O4? ???? plot
par(new=T)
plot(newC.machine$freq)
# # text(newC.machine$freq, labels=c(rep(c("??","??","H-","??","??","??","??"),20)),cex=1) # ???????? ?V>????L1? ?O4B5?.
adf.test(newC.machine.ts, alternative="stationary", k=0) # o?=o?=???? ?????? ??o?=o?=?? ????.

## ???? H.??

## station.newC.machine.ts o?=o?= outlier 0??.
outliers_newC.machine.ts <- tso(newC.machine.ts, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_newC.machine.ts
# 
# # ???? ???? 7 
# par(mfrow=c(1,1))
# plot(diff(newC.machine.ts,7),main="y.machine diff=7")
# par(new=T)
# plot(diff(newC.machine$freq,7))
# # text(diff(newC.machine$freq,7), labels=c(rep(c("??","??","H-","??","??","??","??"),20)),cex=1) # ???????? ????o?=o?=o?=o?=o?=o?= H.??.
# adf.test(diff(newC.machine.ts,7), alternative="stationary") # o?=o?=???? OK
# # adf.test(newC.furi.ts, alternative="stationary") # o?=o?=???? OK

# station.newC.machine.ts <- diff(newC.machine.ts,7)
newC.machine.var <- newC.machine.ts

#___________________ [??o?=o?= ????] station.newC.machine.ts : ?F9??M5? ????. 7????o?=o?= ???????
#### 6. newC furi #####
newC.furi <-newC4[newC4$categ=="furni.interior",]
newC.furi$SESS_DT<- as.character(newC.furi$SESS_DT)
newC.furi$SESS_DT <- as.Date(as.character(newC.furi$SESS_DT), "%Y%m%d")
newC.furi <- newC.furi[,c(1,3)]
newC.furi.ts <- ts(newC.furi$freq, start=newC.furi$SESS_DT[1], end = newC.furi$SESS_DT[length(newC.furi$freq)], freq=1)

## raw_newC.furi 
par(mfrow=c(1,1))
plot(newC.furi.ts,main="newC.furi - diff=0",col="blue") # ?O4? ???? plot
par(new=T)
plot(newC.furi$freq)
# text(newC.furi$freq, labels=c(rep(c("??","??","H-","??","??","??","??"),20)),cex=1) # ???????? ?V>????L1? ?O4B5?.
adf.test(newC.furi.ts, alternative="stationary", k=0) # o?=o?=???? ?????? ??o?=o?=?? ????.
par(mfrow=c(1,2))

## ???? H.?? 
## newC.furi.ts o?=o?= outlier ??o?=o?=!!! 
outliers_newC.furi.ts <- tso(newC.furi.ts, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_newC.furi.ts

## ????0 
# par(mfrow=c(1,1))
# plot(diff(newC.furi.ts,7),main="y.FC diff=7")
# par(new=T)
# plot(diff(newC.furi$freq,7))
# # text(diff(newC.furi$freq,7), labels=c(rep(c("??","??","H-","??","??","??","??"),20)),cex=1) # ???????? ????o?=o?=o?=o?=o?=o?= H.??.
# # adf.test(diff(newC.furi.ts,7), alternative="stationary") # o?=o?=???? OK
# # adf.test(newC.furi.ts, alternative="stationary") # o?=o?=???? OK

station0.newC.furi.ts <-newC.furi.ts
#### var 
newC.furi.var <- station0.newC.furi.ts
#____________________ [??o?=o?= ????] station0.newC.furi.ts : ?F9??M5? ????.
#### 7. newC coupon #####
newC.coupon <-newC4[newC4$categ=="coupon",]
newC.coupon$SESS_DT<- as.character(newC.coupon$SESS_DT)
newC.coupon$SESS_DT <- as.Date(as.character(newC.coupon$SESS_DT), "%Y%m%d")
newC.coupon <- newC.coupon[,c(1,3)]
newC.coupon.ts <- ts(newC.coupon$freq, start=newC.coupon$SESS_DT[1], end = newC.coupon$SESS_DT[length(newC.coupon$freq)], freq=1)

## raw_newC.coupon 
par(mfrow=c(1,1))
plot(newC.coupon.ts,main="newC.coupon - diff=0",col="blue") # ?O4? ???? plot #log?O1??? ??o?=o?=
par(new=T)
plot(newC.coupon$freq)
# text(newC.coupon$freq, labels=c(rep(c("??","??","H-","??","??","??","??"),20)),cex=1) # ???????? ?V>????L1? ?O4B5?.
adf.test(newC.coupon.ts, alternative="stationary", k=0) # o?=o?=???? ?????? ??o?=o?=?? ????.
# log
plot(log(newC.coupon.ts),main="newC.coupon - diff=0",col="blue") #log plot
newC.coupon.ts <- log(newC.coupon.ts) 

## ???? H.?? 
## newC.coupon.ts o?=o?= outlier 2?? 
outliers_newC.coupon.ts <- tso(newC.coupon.ts, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_newC.coupon.ts

#rm outliers
outliers_idx <- outliers_newC.coupon.ts$outliers$ind
par(mfrow=c(1,1))

# 1st outlier
n <- length(newC.coupon.ts)
mo_tc <- outliers("AO", outliers_idx[1])
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_newC.coupon.ts$outliers["coefhat"][[1]])
tc_effect <- coefhat[1]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(newC.coupon.ts), start = start(newC.coupon.ts))
intervent1 <- newC.coupon.ts - tc_effect_ts
plot(cbind(newC.coupon.ts, intervent1, tc_effect_ts))
plot(newC.coupon.ts, type ='b', ylab = "coupon.naver")
lines(intervent1, col = 'red', lty = 3, type ='b')

# 2nd outlier
mo_tc <- outliers("TC", outliers_idx[2])
tc <- outliers.effects(mo_tc, n)
tc_effect <- coefhat[2]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(newC.coupon.ts), start = start(newC.coupon.ts))
intervent2 <- intervent1 - tc_effect_ts
plot(cbind(newC.coupon.ts, intervent1, intervent2, tc_effect_ts))
plot(intervent1, type ='b', ylab = "coupon.naver")
lines(intervent2, col = 'red', lty = 3, type ='b')

station0.newC.coupon.ts<- intervent2

#### var 
newC.coupon.var <-station0.newC.coupon.ts
#____________________ [??o?=o?= ????] newC.coupon.ts : log ?O0? ???? 2?? ??o?=o?=.
#### 8. newC LH #####
newC.LH <-newC4[newC4$categ=="life.health",]
newC.LH$SESS_DT<- as.character(newC.LH$SESS_DT)
newC.LH$SESS_DT <- as.Date(as.character(newC.LH$SESS_DT), "%Y%m%d")
newC.LH <- newC.LH[,c(1,3)]
newC.LH.ts <- ts(newC.LH$freq, start=newC.LH$SESS_DT[1], end = newC.LH$SESS_DT[length(newC.LH$freq)], freq=1)

## raw_newC.LH 
par(mfrow=c(1,1))
plot(newC.LH.ts,main="newC.LH - diff=0",col="blue") # ?O4? ???? plot
par(new=T)
plot(newC.LH$freq)
# text(newC.LH$freq, labels=c(rep(c("??","??","H-","??","??","??","??"),20)),cex=1) # ???????? ?V>????L1? ?O4B5?.
adf.test(newC.LH.ts, alternative="stationary", k=0) # o?=o?=???? ?????? ??o?=o?=?? ????.

## ???? H.??
## newC.LH.ts o?=o?= outlier ??o?=o?=.
outliers_newC.LH.ts <- tso(newC.LH.ts, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_newC.LH.ts  # no outliers were detected

#### var 
newC.LH.var <- newC.LH.ts
#___________________ [??o?=o?= ????] station.newC.LH.ts : ?F9??M5? ????. !!
#### 9. beauty #####
newC.beauty <-newC4[newC4$categ=="beauty",]
newC.beauty$SESS_DT<- as.character(newC.beauty$SESS_DT)
newC.beauty$SESS_DT <- as.Date(as.character(newC.beauty$SESS_DT), "%Y%m%d")
newC.beauty <- newC.beauty[,c(1,3)]
newC.beauty.ts <- ts(newC.beauty$freq, start=newC.beauty$SESS_DT[1], end = newC.beauty$SESS_DT[length(newC.beauty$freq)], freq=1)

### newC.beauty.ts o?=o?= outlier 1??.
outliers_newC.beauty.ts <- tso(newC.beauty.ts, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_newC.beauty.ts
# (outliers_newC.beauty.ts)
# outliers_newC.beauty.ts$outliers
outliers_idx <- outliers_newC.beauty.ts$outliers$ind
#outliers_newC.beauty.ts$outliers$time

### 1st outlier 
n <- length(newC.beauty.ts)
mo_ao <- outliers("AO", outliers_idx[1])
ao <- outliers.effects(mo_ao, n)
coefhat <- as.numeric(outliers_newC.beauty.ts$outliers["coefhat"][[1]])
ao_effect <- coefhat[1]*ao
ao_effect_ts <- ts(ao_effect, frequency = frequency(newC.beauty.ts), start = start(newC.beauty.ts))
newC.beauty.ts.intervent1 <- newC.beauty.ts - ao_effect_ts
# par(mfrow=c(1,1))
# plot(cbind(newC.beauty.ts, newC.beauty.ts.intervent1, ao_effect_ts))
# plot(newC.beauty.ts, type ='b', ylab = "excess birth ratio")
# lines(newC.beauty.ts.intervent1, col = 'red', lty = 3, type ='b')

### ???? o?=o?=???? adf test. 
par(mfrow=c(1,1))
plot(newC.beauty.ts.intervent1,main="newC.beauty.intervent5 diff=0",col="blue") # ?O4? ???? plot
par(new=T)

#### var 
newC.beauty.var <- newC.beauty.ts.intervent1 
#___________________ [??o?=o?= ????] station.newC.LH.ts : ???? ?Q0? ?????V>?o?=o?=!!

#######10. food ######
newC4 <- plyr::count(newC3,c("SESS_DT","categ"))
newC.food <-newC4[newC4$categ=="food",]
newC.food$SESS_DT<- as.character(newC.food$SESS_DT)
newC.food$SESS_DT <- as.Date(as.character(newC.food$SESS_DT), "%Y%m%d")
newC.food <- newC.food[,c(1,3)]
newC.food.ts <- ts(newC.food$freq, start=newC.food$SESS_DT[1], end = newC.food$SESS_DT[length(newC.food$freq)], freq=1)

par(mfrow=c(1,1))
plot(newC.food.ts,main="newC.food - diff=0",col="blue") # ?O4? ???? plot
par(new=T)
plot(newC.food$freq)
# text(newC.food$freq, labels=c(rep(c("??","??","H-","??","??","??","??"),20)),cex=1) ## mon high, sat sun low
adf.test(newC.food.ts, alternative="stationary", k=0) # o?=o?=???? ?????? ??o?=o?=?? ????.


### newC.food.ts o?=o?= outlier 12???N5? ???? 4???? C3??.
outliers_newC.food.ts <- tso(newC.food.ts, types = c("TC", "AO", "LS", "IO", "SLS"))
outliers_newC.food.ts
plot(outliers_newC.food.ts)
outliers_newC.food.ts$outliers
outliers_idx <- outliers_newC.food.ts$outliers$ind
outliers_newC.food.ts$outliers$time

### 1st outlier AO
n <- length(newC.food.ts)
mo_ao <- outliers("AO", outliers_idx[1])
ao <- outliers.effects(mo_ao, n)
coefhat <- as.numeric(outliers_newC.food.ts$outliers["coefhat"][[1]])
ao_effect <- coefhat[1]*ao
ao_effect_ts <- ts(ao_effect, frequency = frequency(newC.food.ts), start = start(newC.food.ts))
newC.food.ts.intervent1 <- newC.food.ts - ao_effect_ts
par(mfrow=c(1,1))
plot(cbind(newC.food.ts, newC.food.ts.intervent1, ao_effect_ts))
plot(newC.food.ts, type ='b', ylab = "")
lines(newC.food.ts.intervent1, col = 'red', lty = 3, type ='b')


### 2nd outlier AO
mo_ao <- outliers("AO", outliers_idx[2])
ao <- outliers.effects(mo_ao, n)
ao_effect <- coefhat[2]*ao
ao_effect_ts <- ts(ao_effect, frequency = frequency(newC.food.ts), start = start(newC.food.ts))
newC.food.ts.intervent2 <- newC.food.ts.intervent1 - ao_effect_ts
par(mfrow=c(1,1))
plot(cbind(newC.food.ts, newC.food.ts.intervent2, ao_effect_ts))
plot(newC.food.ts, type ='b', ylab = "")
lines(newC.food.ts.intervent2, col = 'red', lty = 3, type ='b')


### 3rd outlier AO
mo_ao <- outliers("AO", outliers_idx[3])
ao <- outliers.effects(mo_ao, n)
ao_effect <- coefhat[3]*ao
ao_effect_ts <- ts(ao_effect, frequency = frequency(newC.food.ts), start = start(newC.food.ts))
newC.food.ts.intervent3 <- newC.food.ts.intervent2 - ao_effect_ts
par(mfrow=c(1,1))
plot(cbind(newC.food.ts, newC.food.ts.intervent3, ao_effect_ts))
plot(newC.food.ts, type ='b', ylab = "")
lines(newC.food.ts.intervent3, col = 'red', lty = 3, type ='b')

### 4th outlier AO
mo_ao <- outliers("AO", outliers_idx[10])
ao <- outliers.effects(mo_ao, n)
ao_effect <- coefhat[10]*ao
ao_effect_ts <- ts(ao_effect, frequency = frequency(newC.food.ts), start = start(newC.food.ts))
newC.food.ts.intervent4 <- newC.food.ts.intervent3 - ao_effect_ts
par(mfrow=c(1,1))
plot(cbind(newC.food.ts, newC.food.ts.intervent4, ao_effect_ts))
plot(newC.food.ts, type ='b', ylab = "", ylim = c(-3000,8000))
lines(newC.food.ts.intervent4, col = 'red', lty = 3, type ='b')
adf.test((newC.food.ts.intervent4), alternative="stationary")  ## p-value = 0.01067 stataionary
#### var 
newC.food.var <- newC.food.ts.intervent4






###################
####   naver   ####
###################


############## 1 naver.fashion.clothes.diff #########
# library(data.table)
# setwd("C:/Users/doubley/Desktop/lpoint")
fashion.clothes <- fread("fashion_clothes.csv",data.table = FALSE, encoding = "UTF-8")
colnames(fashion.clothes) <- c("SESS_DT","naver")
fashion.clothes$SESS_DT <- gsub("-","",fashion.clothes$SESS_DT)
fashion.clothes$SESS_DT <- as.Date(fashion.clothes$SESS_DT, "%Y%m%d")
naver.fc.ts <- ts(fashion.clothes$naver, start=fashion.clothes$SESS_DT[1], end = fashion.clothes$SESS_DT[length(fashion.clothes$naver)], freq=1)
station0.naver.fc.ts <- (naver.fc.ts)^(-1)/(-1)
station0.naver.fc.ts <- diff(station0.naver.fc.ts)
# stationary
### var 
naver.fc.arima <- (naver.fc.ts)^(-1)/(-1)
naver.fc.arima <- diff(naver.fc.arima,1)   # revised
naver.FC.var <- naver.fc.ts

############## 2 naver.baby.diff #########
baby <- fread("baby.csv",data.table = FALSE, encoding = "UTF-8")
colnames(baby) <- c("SESS_DT","naver")
baby$SESS_DT <- gsub("-","",baby$SESS_DT)
baby$SESS_DT <- as.Date(baby$SESS_DT,"%Y%m%d")
naver.baby.ts <- ts(baby$naver, start=baby$SESS_DT[1], end = baby$SESS_DT[length(baby$naver)] ,freq=1)
outliers_naver.BAB <- tso(naver.baby.ts, types = c("TC", "AO", "LS", "IO", "SLS")) #no outlier
### var 
naver.baby.var <- naver.baby.ts
naver.baby.arima <- naver.baby.ts

############## 3 naver.beauty.diff #########
beauty <- fread("beauty.csv",data.table = FALSE, encoding = "UTF-8")
colnames(beauty) <- c("SESS_DT","naver")
beauty$SESS_DT <- gsub("-","",beauty$SESS_DT)
beauty$SESS_DT <- as.Date(beauty$SESS_DT,"%Y%m%d")
naver.beauty.ts <- ts(beauty$naver, start=beauty$SESS_DT[1], end = beauty$SESS_DT[length(beauty$naver)] ,freq=1)
### var 
naver.beauty.arima <- naver.beauty.ts # revised
naver.beauty.var <- naver.beauty.ts
############## 4 naver.machine.diff _ intervention #########
machine <- fread("digital_machine.csv",data.table = FALSE, encoding = "UTF-8")
colnames(machine) <- c("SESS_DT","naver")
machine$SESS_DT <- gsub("-","",machine$SESS_DT)
machine$SESS_DT <- as.Date(machine$SESS_DT,"%Y%m%d")
naver.machine.ts <- ts(machine$naver, start=machine$SESS_DT[1], end = machine$SESS_DT[length(machine$naver)] ,freq=1)
outliers_naver.MAC <- tso(naver.machine.ts, types = c("TC", "AO", "LS", "IO", "SLS")) 

#rm outliers
outliers_idx <- outliers_naver.MAC$outliers$ind
# machine
JB <- arima(naver.machine.ts , c(1,0,0), seasonal=list(order = c(1,0,0), period = 7))  #same model with ARIMA
pars <- coefs2poly(JB)
mo_tc <- outliers("IO", outliers_idx[1])
# pars?? ?V4? ??o?=o?= ???? arcoefs?? macoefs?? ??o?=o?=??????. ?W3? ??o?=o?=?O1? ?o?=o?=???.
n <- length(naver.machine.ts) 
tc <- outliers.effects(mo_tc, pars=list(arcoefs=pars$arcoefs, macoefs=pars$macoefs), n)
coefhat <- as.numeric(outliers_naver.MAC$outliers["coefhat"][[1]])
tc_effect <- coefhat[1]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(naver.machine.ts), start = start(naver.machine.ts))
intervent1 <- naver.machine.ts - tc_effect_ts
par(mfrow=c(1,1))
plot(cbind(naver.machine.ts, intervent1, tc_effect_ts))
plot(naver.machine.ts, type ='b')
lines(intervent1, col = 'red', lty = 3, type ='b')


# 2nd outlier
mo_tc <- outliers("TC", outliers_idx[2])
tc <- outliers.effects(mo_tc, n)
tc_effect <- coefhat[2]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(naver.machine.ts), start = start(naver.machine.ts))
intervent2 <- intervent1 - tc_effect_ts
plot(cbind(naver.machine.ts, intervent1, intervent2, tc_effect_ts))
plot(intervent1, type ='b', ylab = "excess birth ratio")
lines(intervent2, col = 'red', lty = 3, type ='b')

naver.machine.ts.inter <- intervent2
### var 
naver.machine.arima <- naver.machine.ts.inter
naver.machine.arima <- diff(naver.machine.arima,7)
naver.machine.var <- naver.machine.ts.inter
############## 5 naver.furi.diff _ intervention #########
furi <- fread("furni_interior.csv",data.table = FALSE, encoding = "UTF-8")
colnames(furi) <- c("SESS_DT","naver")
furi$SESS_DT <- gsub("-","",furi$SESS_DT)
furi$SESS_DT <- as.Date(furi$SESS_DT,"%Y%m%d")
naver.furi.ts <- ts(furi$naver, start=furi$SESS_DT[1], end = furi$SESS_DT[length(furi$naver)] ,freq=1)
outliers_naver.FURI <- tso(naver.furi.ts, types = c("TC", "AO", "LS", "IO", "SLS"))

#rm outliers
outliers_idx <- outliers_naver.FURI$outliers$ind

# 1st outlier
n <- length(naver.furi.ts)
mo_tc <- outliers("AO", outliers_idx[1])
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_naver.FURI$outliers["coefhat"][[1]])
tc_effect <- coefhat[1]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(naver.furi.ts), start = start(naver.furi.ts))
intervent1 <- naver.furi.ts - tc_effect_ts
plot(cbind(naver.furi.ts, intervent1, tc_effect_ts))
plot(naver.furi.ts, type ='b', ylab = "excess birth ratio")
lines(intervent1, col = 'red', lty = 3, type ='b')


# 2nd outlier
mo_tc <- outliers("TC", outliers_idx[2])
tc <- outliers.effects(mo_tc, n)
tc_effect <- coefhat[2]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(naver.furi.ts), start = start(naver.furi.ts))
intervent2 <- intervent1 - tc_effect_ts
plot(cbind(naver.furi.ts, intervent1, intervent2, tc_effect_ts))
plot(intervent1, type ='b', ylab = "excess birth ratio")
lines(intervent2, col = 'red', lty = 3, type ='b')

naver.furi.ts.inter <- intervent2
### var 
naver.furi.arima <- diff(naver.furi.ts.inter,7)

naver.furi.var <- naver.furi.ts.inter

############## 6 naver.food.diff _ intervention #########
food <- fread("food.csv",data.table = FALSE, encoding = "UTF-8")
colnames(food) <- c("SESS_DT","naver")
food$SESS_DT <- gsub("-","",food$SESS_DT)
food$SESS_DT <- as.Date(food$SESS_DT,"%Y%m%d")
naver.food.ts <- ts(food$naver, start=food$SESS_DT[1], end = food$SESS_DT[length(food$naver)] ,freq=1)
outliers_naver.food <- tso(naver.food.ts, types = c("TC", "AO", "LS", "IO", "SLS")) #no outlier

#rm outliers
outliers_idx <- outliers_naver.food$outliers$ind

# 1st outlier
n <- length(naver.food.ts)
mo_tc <- outliers("LS", outliers_idx[1])
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_naver.food$outliers["coefhat"][[1]])
tc_effect <- coefhat[1]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(naver.food.ts), start = start(naver.food.ts))
intervent1 <- naver.food.ts - tc_effect_ts
plot(cbind(naver.food.ts, intervent1, tc_effect_ts))
plot(naver.food.ts, type ='b', ylab = "food.naver")
lines(intervent1, col = 'red', lty = 3, type ='b')

naver.food.ts.inter <- intervent1
### var 
naver.food.arima <- diff(naver.food.ts.inter)
naver.food.var <- naver.food.ts.inter

############## 7 naver.daily.health.diff #########
life.health <- fread("daily_health.csv",data.table = FALSE, encoding = "UTF-8")
colnames(life.health) <- c("SESS_DT","naver")
life.health$SESS_DT <- gsub("-","",life.health$SESS_DT)
life.health$SESS_DT <- as.Date(life.health$SESS_DT,"%Y%m%d")
naver.LH.ts <- ts(life.health$naver, start=life.health$SESS_DT[1], end = life.health$SESS_DT[length(life.health$naver)] ,freq=1)

### var 
naver.LH.arima <- diff(naver.LH.ts,7)
naver.LH.var <- naver.LH.ts
############## 8 naver.fashion.zap.diff _ intervention #########
fashion.zap <- fread("fashion_etc.csv",data.table = FALSE, encoding = "UTF-8")
colnames(fashion.zap) <- c("SESS_DT","naver")
fashion.zap$SESS_DT <- gsub("-","",fashion.zap$SESS_DT)
fashion.zap$SESS_DT <- as.Date(fashion.zap$SESS_DT,"%Y%m%d")
naver.fz.ts <- ts(fashion.zap$naver, start=fashion.zap$SESS_DT[1], end = fashion.zap$SESS_DT[length(fashion.zap$naver)] ,freq=1)
outliers_naver.FZ <- tso(naver.fz.ts, types = c("TC", "AO", "LS", "IO", "SLS")) #no outlier

#rm outliers
outliers_idx <- outliers_naver.FZ$outliers$ind

# 1st outlier
n <- length(naver.fz.ts)
mo_tc <- outliers("TC", outliers_idx[1])
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_naver.FZ$outliers["coefhat"][[1]])
tc_effect <- coefhat[1]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(naver.fz.ts), start = start(naver.fz.ts))
intervent1 <- naver.fz.ts - tc_effect_ts
plot(cbind(naver.fz.ts, intervent1, tc_effect_ts))
plot(naver.fz.ts, type ='b', ylab = "food.naver")
lines(intervent1, col = 'red', lty = 3, type ='b')

naver.fz.ts.inter <- intervent1
### var 
naver.fz.arima <- diff(naver.fz.ts.inter,7)

naver.FZ.var <- intervent1
############## 9 naver.sports.diff  #########
sports <- fread("sport.csv",data.table = FALSE, encoding = "UTF-8")
colnames(sports) <- c("SESS_DT","naver")
sports$SESS_DT <- gsub("-","",sports$SESS_DT)
sports$SESS_DT <- as.Date(sports$SESS_DT,"%Y%m%d")
naver.sports.ts <- ts(sports$naver, start=sports$SESS_DT[1], end = sports$SESS_DT[length(sports$naver)] ,freq=1)
outliers_naver.SP <- tso(naver.sports.ts, types = c("TC", "AO", "LS", "IO", "SLS")) #no outlier
### var 
naver.sports.arima <- diff(naver.sports.ts,7)
naver.sports.var <- naver.sports.ts
############## 10 naver.coupon.diff _ intervention ########
coupon <- fread("coupon.csv",data.table = FALSE, encoding = "UTF-8")
colnames(coupon) <- c("SESS_DT","naver")

coupon$SESS_DT <- gsub("-","",coupon$SESS_DT)
coupon$SESS_DT <- as.Date(coupon$SESS_DT,"%Y%m%d")
naver.coupon.ts <- ts(coupon$naver, start=coupon$SESS_DT[1], end = coupon$SESS_DT[length(coupon$naver)] ,freq=1)

naver.coupon.ts<-(naver.coupon.ts)^(-1)/(-1)

outliers_naver.COUP <- tso(naver.coupon.ts, types = c("TC", "AO", "LS", "IO", "SLS")) 
#rm outliers
outliers_idx <- outliers_naver.COUP$outliers$ind
par(mfrow=c(1,1))

# 1st outlier
n <- length(naver.coupon.ts)
mo_tc <- outliers("TC", outliers_idx[1])
tc <- outliers.effects(mo_tc, n)
coefhat <- as.numeric(outliers_naver.COUP$outliers["coefhat"][[1]])
tc_effect <- coefhat[1]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(naver.coupon.ts), start = start(naver.coupon.ts))
intervent1 <- naver.coupon.ts - tc_effect_ts
plot(cbind(naver.coupon.ts, intervent1, tc_effect_ts))
plot(naver.coupon.ts, type ='b', ylab = "coupon.naver")
lines(intervent1, col = 'red', lty = 3, type ='b')

# 2nd outlier
mo_tc <- outliers("TC", outliers_idx[2])
tc <- outliers.effects(mo_tc, n)
tc_effect <- coefhat[2]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(naver.coupon.ts), start = start(naver.coupon.ts))
intervent2 <- intervent1 - tc_effect_ts
plot(cbind(naver.coupon.ts, intervent1, intervent2, tc_effect_ts))
plot(intervent1, type ='b', ylab = "coupon.naver")
lines(intervent2, col = 'red', lty = 3, type ='b')


# 3rd outlier
mo_tc <- outliers("LS", outliers_idx[3])
tc <- outliers.effects(mo_tc, n)
tc_effect <- coefhat[3]*tc
tc_effect_ts <- ts(tc_effect, frequency = frequency(naver.coupon.ts), start = start(naver.coupon.ts))
intervent3 <- intervent2 - tc_effect_ts
plot(cbind(naver.coupon.ts, intervent1, intervent2, intervent3,tc_effect_ts))
plot(intervent2, type ='b', ylab = "coupon.naver")
lines(intervent3, col = 'red', lty = 3, type ='b')
naver.coupon.ts.inter <- intervent3
### var 
naver.coupon.arima <- diff(naver.coupon.ts.inter,7)
naver.coupon.var <- naver.coupon.ts.inter




#################
#####  VAR  #####
#################


#### FC n = 182   #####
detach("package:dplyr", unload=TRUE)
# install.packages("tsDyn")
library(tsDyn)
# install.packages("vars")
library(vars)
# install.packages("dyn")
library(dyn)

newC.FC.var1 <- diff(newC.FC.var)
naver.FC.var1 <- diff(naver.FC.var)
FC.var <- cbind(y.FC.var, newC.FC.var1, naver.FC.var1)
FC.var.train <- subset(FC.var, end=nrow(FC.var)-28) 
FC.var.valid1 <- subset(FC.var, end=nrow(FC.var)-7) # train???? 21?? ??o?=o?=. # nrow(FC.var.valid) = 175
FC.var.valid2 <- subset(FC.var, start=nrow(FC.var)-27, end=nrow(FC.var)-7) # nrow(FC.var.valid) = 14
FC.var.rtrain <- subset(FC.var, end=nrow(FC.var)-7) # nrow(FC.var.valid) = 14
FC.var.test1 <- subset(FC.var, end=nrow(FC.var))
FC.var.test2 <- subset(FC.var, start=nrow(FC.var)-6, end=nrow(FC.var))
FC.var.w <- subset(FC.var, end=nrow(FC.var))

#### fitting var 

VARselect(FC.var.train, lag.max=9, type="const") # order selection gives VAR(7)
FC.var.train.fit <- VAR(FC.var.train, p=8, type="both")
# summary(FC.var.train.fit) 
# R2 0.611
acf(residuals(FC.var.train.fit), lag.max = 60) # o?=o?=?? 3?Y8? ???? ??!! ?????? ????.

#### removing insignificant coefficients 
#### 15 
a <- dyn$lm(y.FC.var ~ -1 + lag(y.FC.var, -1) + 
              lag(y.FC.var, -2) +
              lag(y.FC.var, -3) +
              lag(y.FC.var, -4) +
              # lag(y.FC.var, -5) +
              # lag(y.FC.var, -6) +
              # lag(y.FC.var, -7) +
              # lag(y.FC.var, -8) +
              # lag(newC.FC.var, -1) +
              # lag(newC.FC.var, -2) +
              # lag(newC.FC.var, -3) +
              # lag(newC.FC.var, -4) +
              # lag(newC.FC.var, -5) +
              # lag(newC.FC.var, -6) +
              # lag(newC.FC.var, -7) +
            # lag(newC.FC.var, -8) +
            lag(naver.FC.var, -1) + 
              lag(naver.FC.var, -2) +
              # lag(naver.FC.var, -3) +
              lag(naver.FC.var, -4) +
              # lag(naver.FC.var, -5) +
              lag(naver.FC.var, -6) +
              lag(naver.FC.var, -7)+
              lag(naver.FC.var, -8), data=FC.var.train)
summary(a) # 0.6099 ??. ???Z1? o?=o?=?????? ??o?=o?=?? H. ?C>???.
acf(residuals(a), lag.max = 60) 

# forecast RMSE = 0.001235936#####
Box.test(a$residuals, type="Ljung-Box")
JB <- predict(a, newdata=FC.var.valid1)
plot(JB, col=2, xlim=c(17620, 17820), ylim=c(-0.004, 0.004))
par(new=T)
plot(y.FC.var,  xlim=c(17620, 17820), ylim=c(-0.004, 0.004))
accuracy(JB[155:175], FC.var.valid2[,1])

# test error RMSE = 0.002162749#####
a <- dyn$lm(y.FC.var ~ -1 + lag(y.FC.var, -1) + 
              lag(y.FC.var, -2) +
              lag(y.FC.var, -3) +
              lag(y.FC.var, -4) +
              lag(naver.FC.var, -1) + 
              lag(naver.FC.var, -2) +
              lag(naver.FC.var, -4) +
              lag(naver.FC.var, -6) +
              lag(naver.FC.var, -7)+
              lag(naver.FC.var, -8), data=FC.var.rtrain)
summary(a) # 0.6365
acf(residuals(a), lag.max = 60) 
Box.test(a$residuals, type="Ljung-Box")
JB <- predict(a, newdata=FC.var.test1)
plot(JB, col=2, xlim=c(17620, 17820),ylim=c(-0.004, 0.004), main="Fashion Clothes", ylab="")
par(new=T)
plot(y.FC.var,  xlim=c(17620, 17820), ylim=c(-0.004, 0.004), main="", ylab="")
accuracy(JB[176:182], FC.var.test2[,1])
# final coefficient ####
a <- dyn$lm(y.FC.var ~ -1 + lag(y.FC.var, -1) + 
              lag(y.FC.var, -2) +
              lag(y.FC.var, -3) +
              lag(y.FC.var, -4) +
              lag(naver.FC.var, -1) + 
              lag(naver.FC.var, -2) +
              lag(naver.FC.var, -4) +
              lag(naver.FC.var, -6) +
              lag(naver.FC.var, -7)+
              lag(naver.FC.var, -8), data=FC.var.w)
summary(a) # 0.6365



#### baby n = 183  #####
#### creating variables 
baby.var <- cbind(y.baby.var, newC.baby.var, naver.baby.var)
baby.var.train <- subset(baby.var, end=nrow(baby.var)-28) 
baby.var.valid1 <- subset(baby.var, end=nrow(baby.var)-7) # train???? 21?? ??o?=o?=. # nrow(baby.var.valid) = 175
baby.var.valid2 <- subset(baby.var, start=nrow(baby.var)-27, end=nrow(baby.var)-7) # nrow(baby.var.valid) = 14
baby.var.test <- subset(baby.var, end=nrow(baby.var))

#### fitting var 
VARselect(baby.var.train, lag.max=9, type="const") # order selection gives VAR(7)
baby.var.train.fit <- VAR(baby.var.train, p=8, type="both")
# summary(baby.var.train.fit) 
# R2 0.5616
# baby.var.train.fit$varresult 
acf(residuals(baby.var.train.fit), lag.max = 60) # o?=o?=?? 3?Y8? ???? ??!! ?????? ????.

#### removing insignificant coefficients 
#### 15 
a <- dyn$lm(y.baby.var ~ lag(y.baby.var, -1) + 
              # lag(y.baby.var, -2) +
              lag(y.baby.var, -3) +
              # lag(y.baby.var, -4) +
              lag(y.baby.var, -5) +
              # lag(y.baby.var, -6) +
              # lag(y.baby.var, -7) +
              # lag(y.baby.var, -8) +
              lag(newC.baby.var, -1) +
              # lag(newC.baby.var, -2) +
              # lag(newC.baby.var, -3) +
              # lag(newC.baby.var, -4) +
              # lag(newC.baby.var, -5) +
              # lag(newC.baby.var, -6) +
              # lag(newC.baby.var, -7) +
              # lag(newC.baby.var, -8) +
              lag(naver.baby.var, -1) + 
              lag(naver.baby.var, -2) +
              # lag(naver.baby.var, -3) +
              # lag(naver.baby.var, -4) +
              lag(naver.baby.var, -5) +
              # lag(naver.baby.var, -6) +
              lag(naver.baby.var, -7), data=baby.var.train)
# lag(naver.baby.var, -8)
summary(a) # 0.5727 ??.
acf(residuals(a), lag.max = 60) # o?=o?=?? 3?Y8? ???? ??!! ?????? ????.

# forecast RMSE = 0.0001026343 #####
Box.test(a$residuals, type="Ljung-Box")
JB <- predict(a, newdata=baby.var.valid1)
plot(JB)
plot(JB, col=2, xlim=c(17620, 17820), ylim=c(-0.00055, -0.0001))
par(new=T)
plot(y.baby.var,  xlim=c(17620, 17820), ylim=c(-0.00055, -0.0001))
accuracy(JB[155:175], baby.var.valid2[,1])

#### FZ n = 176  #####
#### creating variables 
FZ.var <- cbind(y.FZ.var, diff(newC.FZ.var,7), diff(naver.FZ.var,7))
FZ.var.train <- subset(FZ.var, end=nrow(FZ.var)-28) 
FZ.var.valid1 <- subset(FZ.var, end=nrow(FZ.var)-7) # train???? 21?? ??o?=o?=. # nrow(FZ.var.valid) = 175
FZ.var.valid2 <- subset(FZ.var, start=nrow(FZ.var)-27, end=nrow(FZ.var)-7) # nrow(FZ.var.valid) = 14
FZ.var.test <- subset(FZ.var, end=nrow(FZ.var))

#### fitting var 
VARselect(FZ.var.train, lag.max=9, type="const") # order selection gives VAR(7)
FZ.var.train.fit <- VAR(FZ.var.train, p=1, type="both")
summary(FZ.var.train.fit) 
# R2 0.5616
acf(residuals(FZ.var.train.fit), lag.max = 60)
#### removing insignificant coefficients 
#### 5 
a <- dyn$lm(y.FZ.var ~ lag(y.FZ.var, -1) +
              lag(y.FZ.var, -7) +
              lag(y.FZ.var, -8) +
              lag(y.FZ.var, -14) +
              lag(y.FZ.var, -21) +
              lag(y.FZ.var, -24) +
              lag(newC.FZ.var, -7) +
              lag(naver.FZ.var, -1) #+  
            , data=FZ.var.train)
summary(a) # 0.3556 ??.

# forecast RMSE = 0.004572774 ####
acf(residuals(a), lag.max = 60) 
Box.test(a$residuals, type="Ljung-Box")
JB <- predict(a, newdata=FZ.var.valid1)
plot(JB, col=2, xlim=c(17620, 17820), ylim=c(-6000,4000))
par(new=T)
plot(y.FZ.var,  xlim=c(17620, 17820), ylim=c(-6000,4000))
accuracy(JB[155:175], FZ.var.valid2[,1])

#### sports n = 176  #####
#### creating variables 
sports.var <- cbind(y.sports.var, diff(newC.sports.var,7), diff(naver.sports.var,7))
sports.var.train <- subset(sports.var, end=nrow(sports.var)-28) 
sports.var.valid1 <- subset(sports.var, end=nrow(sports.var)-7) # train???? 21?? ??o?=o?=. 
sports.var.valid2 <- subset(sports.var, start=nrow(sports.var)-27, end=nrow(sports.var)-7) # nrow(sports.var.valid) = 14
sports.var.test <- subset(sports.var, end=nrow(sports.var))

#### fitting var 
VARselect(sports.var.train, lag.max=9, type="const") # order selection gives VAR(7)
sports.var.train.fit <- VAR(sports.var.train, p=1, type="both")
summary(sports.var.train.fit) 
# R2 0.2476
acf(residuals(sports.var.train.fit), lag.max = 60) # o?=o?=?? 3?Y8? ???? ??!! ?????? ????.

#### removing insignificant coefficients 

# 3 
a <- dyn$lm(y.sports.var ~ -1 + lag(y.sports.var, -1) + 
              # lag(y.sports.var, -2) +
              # lag(y.sports.var, -3) +
              # lag(y.sports.var, -4) +
              # lag(y.sports.var, -5) +
              # lag(y.sports.var, -6) +
              lag(y.sports.var, -7) +
              # lag(y.sports.var, -8) +
              lag(newC.sports.var, -1) +
              # lag(newC.sports.var, -2) +
              # lag(newC.sports.var, -3) +
              # lag(newC.sports.var, -4) +
              # lag(newC.sports.var, -5) +
              # lag(newC.sports.var, -6) +
              # lag(newC.sports.var, -7) +
              # lag(newC.sports.var, -8) +
              lag(naver.sports.var, -1) + 
              # lag(naver.sports.var, -2) +
              # lag(naver.sports.var, -3) +
              # lag(naver.sports.var, -4) +
              # lag(naver.sports.var, -5) +
              lag(naver.sports.var, -6) +
              # lag(naver.sports.var, -7)+
              lag(naver.sports.var, -8)
            , data=sports.var.train)
summary(a) # 0.3395 ??.
acf(residuals(a), lag.max = 60) # o?=o?=?? 3?Y8? ???? ??!! ?????? ????.

# forecast RMSE = 0.002464339  #####
Box.test(a$residuals, type="Ljung-Box")
JB <- predict(a, newdata=sports.var.valid1)
plot(JB, col=2, xlim=c(17620, 17820), ylim=c(-0.00015, 0.00015))
par(new=T)
plot(y.sports.var,  xlim=c(17620, 17820), ylim=c(-0.00015, 0.00015))
accuracy(JB[155:175], sports.var.valid2[,1])
#### machine n = 176  #####
#### creating variables 
newC.machine.var7 <- diff(newC.machine.var,7)
naver.machine.var7 <- diff(naver.machine.var,7)
machine.var <- cbind(y.machine.var, newC.machine.var7, naver.machine.var7)
machine.var.train <- subset(machine.var, end=nrow(machine.var)-28) 
machine.var.valid1 <- subset(machine.var, end=nrow(machine.var)-7) # train???? 21?? ??o?=o?=. # nrow(machine.var.valid) = 175
machine.var.valid2 <- subset(machine.var, start=nrow(machine.var)-27, end=nrow(machine.var)-7) # nrow(machine.var.valid) = 14
machine.var.test <- subset(machine.var, start=nrow(machine.var)-6, end=nrow(machine.var))

# fitting var ####
VARselect(machine.var.train, lag.max=9, type="const") # order selection gives VAR(7)
machine.var.train.fit <- VAR(machine.var.train, p=8, type="both")
summary(machine.var.train.fit) 
# R2 0.611
acf(residuals(machine.var.train.fit), lag.max = 60) # o?=o?=?? 3?Y8? ???? ??!! ?????? ????.



#### removing insignificant coefficients
#### 18  
a <- dyn$lm(y.machine.var ~ -1+lag(y.machine.var, -1) +
              # lag(y.machine.var, -2) +
              # lag(y.machine.var, -3) +
              # lag(y.machine.var, -4) +
              # lag(y.machine.var, -5) +
              # lag(y.machine.var, -6) +
              lag(y.machine.var, -7) +
              lag(y.machine.var, -8) +
              lag(y.machine.var, -14) +
              lag(y.machine.var, -15) +
              # lag(newC.machine.var, -1) +
              # lag(newC.machine.var, -2) +
              # lag(newC.machine.var, -3) +
              # lag(newC.machine.var, -4) +
              # lag(newC.machine.var, -5) +
              # lag(newC.machine.var, -6) +
              # lag(newC.machine.var, -7) +
              lag(naver.machine.var, -1) +
              # lag(naver.machine.var, -2) +
              # lag(naver.machine.var, -3) +
              # lag(naver.machine.var, -4) +
              # lag(naver.machine.var, -5) +
              # lag(naver.machine.var, -6) +
              lag(naver.machine.var, -7), data=machine.var.train)
summary(a) # 0.4322

# forecast RMSE = 153.9284 #### #####
acf(residuals(a), lag.max = 60) 
Box.test(a$residuals, type="Ljung-Box")
JB <- predict(a, newdata=machine.var.valid1)
plot(JB, col=2, xlim=c(17620, 17820), ylim=c(-550, 1000))
par(new=T)
plot(y.machine.var,  xlim=c(17620, 17820), ylim=c(-550, 1000))
accuracy(JB[155:175], machine.var.valid2[,1])
#### furi n = 176  #####
#### creating variables 
newC.furi.var7 <- diff(newC.furi.var,7)
naver.furi.var7 <- diff(naver.furi.var,7)
furi.var <- cbind(y.furi.var, newC.furi.var7, naver.furi.var7)
furi.var.train <- subset(furi.var, end=nrow(furi.var)-28) 
furi.var.valid1 <- subset(furi.var, end=nrow(furi.var)-7) # train???? 21?? ??o?=o?=. # nrow(furi.var.valid) = 175
furi.var.valid2 <- subset(furi.var, start=nrow(furi.var)-27, end=nrow(furi.var)-7) # nrow(furi.var.valid) = 14
furi.var.test <- subset(furi.var, start=nrow(furi.var)-6, end=nrow(furi.var))

#### fitting var 
VARselect(furi.var.train, lag.max=9, type="const") # order selection gives VAR(7)
furi.var.train.fit <- VAR(furi.var.train, p=8, type="both")
summary(furi.var.train.fit) 
# R2 0.611
acf(residuals(furi.var.train.fit), lag.max = 60) # o?=o?=?? 3?Y8? ???? ??!! ?????? ????.



#### removing insignificant coefficients 
a <- dyn$lm(y.furi.var ~ -1 + lag(y.furi.var, -2) +
              # lag(y.furi.var, -3) +
              lag(y.furi.var, -4) +
              # lag(y.furi.var, -5) +
              # lag(y.furi.var, -6) +
              lag(y.furi.var, -7) +
              lag(y.furi.var, -18) +
              lag(y.furi.var, -14) +
              lag(y.furi.var, -21) +
              lag(y.furi.var, -28) # +
            # lag(newC.furi.var, -1) +
            # lag(newC.furi.var, -2) +
            # lag(newC.furi.var, -3) +
            # lag(newC.furi.var, -4) +
            # lag(newC.furi.var, -5) +
            # lag(newC.furi.var, -6) +
            # lag(newC.furi.var, -7) +
            # lag(newC.furi.var, -8) +
            # lag(naver.furi.var, -1) +
            # lag(naver.furi.var, -2) +
            # lag(naver.furi.var, -3) +
            # lag(naver.furi.var, -4) +
            # lag(naver.furi.var, -5) +
            # lag(naver.furi.var, -6) +
            # lag(naver.furi.var, -7) #+
            # lag(naver.furi.var, -8)
            , data=furi.var.train)
summary(a) # 4376 --> 4243 ??. ???Z1? o?=o?=?????? ??o?=o?=?? H. ?C>???.



# forecast RMSE = 1.32781 #########
acf(residuals(a), lag.max = 60) 
Box.test(a$residuals, type="Ljung-Box")
JB <- predict(a, newdata=furi.var.valid1)
plot(JB, col=2, xlim=c(17620, 17820), ylim=c(-2, 2))
par(new=T)
plot(y.furi.var,  xlim=c(17620, 17820), ylim=c(-2, 2))
accuracy(JB[155:175], furi.var.valid2[,1])
#### coupon n = 176  #####
#### creating variables 
newC.coupon.var7 <- diff(newC.coupon.var,7)
naver.coupon.var7 <- diff(naver.coupon.var,7)
coupon.var <- cbind(y.coupon.var, newC.coupon.var7, naver.coupon.var7)
coupon.var.train <- subset(coupon.var, end=nrow(coupon.var)-28) 
coupon.var.valid1 <- subset(coupon.var, end=nrow(coupon.var)-7) # train???? 21?? ??o?=o?=. # nrow(coupon.var.valid) = 175
coupon.var.valid2 <- subset(coupon.var, start=nrow(coupon.var)-27, end=nrow(coupon.var)-7) # nrow(coupon.var.valid) = 14
coupon.var.test <- subset(coupon.var, start=nrow(coupon.var)-6, end=nrow(coupon.var))

#### fitting var 
VARselect(coupon.var.train, lag.max=9, type="const") # order selection gives VAR(7)
coupon.var.train.fit <- VAR(coupon.var.train, p=8, type="both")
summary(coupon.var.train.fit) 
# R2 0.611
acf(residuals(coupon.var.train.fit), lag.max = 60) # o?=o?=?? 3?Y8? ???? ??!! ?????? ????.

#### removing insignificant coefficients 
a <- dyn$lm(y.coupon.var ~ lag(y.coupon.var, -4) +
              lag(y.coupon.var, -3) +
              # lag(y.coupon.var, -5) +
              # lag(y.coupon.var, -6) +
              lag(y.coupon.var, -7) +
              # lag(y.coupon.var, -8) +
              # lag(y.coupon.var, -18) +
              lag(y.coupon.var, -14) +
              # lag(y.coupon.var, -21) +
              # lag(y.coupon.var, -28)  +
              # lag(newC.coupon.var, -1) +
              lag(newC.coupon.var, -2) +
              # lag(newC.coupon.var, -3) +
              # lag(newC.coupon.var, -4) +
              # lag(newC.coupon.var, -5) +
              # lag(newC.coupon.var, -6) +
              # lag(newC.coupon.var, -7) +
              lag(newC.coupon.var, -8) +
              lag(naver.coupon.var, -1) +
              # lag(naver.coupon.var, -2) +
              lag(naver.coupon.var, -3) +
              # lag(naver.coupon.var, -4) +
              # lag(naver.coupon.var, -5) +
              # lag(naver.coupon.var, -6) +
              # lag(naver.coupon.var, -7) +
              lag(naver.coupon.var, -8)
            , data=coupon.var.train)
summary(a) # 2452 --> 2496 ??. ???Z1? o?=o?=?????? ??o?=o?=?? H. ?C>???.



# forecast RMSE = 0.3252758  #####
acf(residuals(a), lag.max = 60) 
Box.test(a$residuals, type="Ljung-Box")
JB <- predict(a, newdata=coupon.var.valid1)
plot(JB, col=2, xlim=c(17620, 17820), ylim=c(-0.5, 1))
par(new=T)
plot(y.coupon.var,  xlim=c(17620, 17820), ylim=c(-0.5, 1))

accuracy(JB[155:175], coupon.var.valid2[,1])
#### LH n = 176  #####
#### creating variables 
newC.LH.var7 <- diff(newC.LH.var,7)
naver.LH.var7 <- diff(naver.LH.var,7)
LH.var <- cbind(y.LH.var, newC.LH.var7, naver.LH.var7)
LH.var.train <- subset(LH.var, end=nrow(LH.var)-28) 
LH.var.valid1 <- subset(LH.var, end=nrow(LH.var)-7) # train???? 21?? ??o?=o?=. # nrow(LH.var.valid) = 175
LH.var.valid2 <- subset(LH.var, start=nrow(LH.var)-27, end=nrow(LH.var)-7) # nrow(LH.var.valid) = 14
LH.var.rtrain <- subset(LH.var, end=nrow(LH.var)-7) # nrow(LH.var.valid) = 14
LH.var.test1 <- subset(LH.var, end=nrow(LH.var))
LH.var.test2 <- subset(LH.var, start=nrow(LH.var)-6, end=nrow(LH.var))
LH.var.w <- subset(LH.var, end=nrow(LH.var))

#### fitting var 
VARselect(LH.var.train, lag.max=9, type="const") # order selection gives VAR(7)
LH.var.train.fit <- VAR(LH.var.train, p=8, type="both")
summary(LH.var.train.fit) 
acf(residuals(LH.var.train.fit), lag.max = 60) # o?=o?=?? 3?Y8? ???? ??!! ?????? ????.
#### removing insignificant coefficients 
# 2 
a <- dyn$lm(y.LH.var ~ -1+ lag(y.LH.var, -1) +
              lag(y.LH.var, -2) +
              # lag(y.LH.var, -3) +
              # lag(y.LH.var, -4) +
              lag(y.LH.var, -5) +
              # lag(y.LH.var, -6) +
              lag(y.LH.var, -7) +
              lag(y.LH.var, -8) +
              lag(y.LH.var, -14) +
              lag(y.LH.var, -21) +
              lag(newC.LH.var, -1) +
              lag(newC.LH.var, -2) +
              lag(newC.LH.var, -3) +
              # lag(newC.LH.var, -4) +
              # lag(newC.LH.var, -5) +
              # lag(newC.LH.var, -6) +
              lag(newC.LH.var, -7) +
              # lag(newC.LH.var, -8) +
              lag(naver.LH.var, -1) + 
              # lag(naver.LH.var, -2) +
              lag(naver.LH.var, -3) +
              lag(naver.LH.var, -4) #+
            # lag(naver.LH.var, -5) +
            # lag(naver.LH.var, -6) +
            # lag(naver.LH.var, -7) #+
            # lag(naver.LH.var, -8)
            , data=LH.var.train)
summary(a) # 0.4802 --> 4759 ??. ???Z1? o?=o?=?????? ??o?=o?=?? H. ?C>???.



# forecast RMSE = 4906.968 #### #####
acf(residuals(a), lag.max = 60) 
Box.test(a$residuals, type="Ljung-Box")
JB <- predict(a, newdata=LH.var.valid1)
plot(JB, col=2, xlim=c(17620, 17820), ylim=c(-15000, 15000))
par(new=T)
plot(y.LH.var,  xlim=c(17620, 17820), ylim=c(-15000, 15000))
accuracy(JB[155:175], LH.var.valid2[,1])
# test error RMSE = 6583.097  #####
a <- dyn$lm(y.LH.var ~ -1+ lag(y.LH.var, -1) +
              lag(y.LH.var, -2) +
              lag(y.LH.var, -5) +
              lag(y.LH.var, -7) +
              lag(y.LH.var, -8) +
              lag(y.LH.var, -14) +
              lag(y.LH.var, -21) +
              lag(newC.LH.var, -1) +
              lag(newC.LH.var, -2) +
              lag(newC.LH.var, -3) +
              lag(newC.LH.var, -7) +
              lag(naver.LH.var, -1) + 
              lag(naver.LH.var, -3) +
              lag(naver.LH.var, -4) #+
            , data=LH.var.rtrain)
summary(a) # 0.4895 ??
acf(residuals(a), lag.max = 60) 

#### forecast 
Box.test(a$residuals, type="Ljung-Box")
JB <- predict(a, newdata=LH.var.test1)
plot(JB, col=2, xlim=c(17620, 17820), ylim=c(-20000, 20000), main="Life & Health", ylab="")
par(new=T)
plot(y.LH.var,  xlim=c(17620, 17820), ylim=c(-20000, 20000), main="", ylab="")
accuracy(JB[176:182], LH.var.test2[,1])
# final coefficient ####
a <- dyn$lm(y.LH.var ~ -1+ lag(y.LH.var, -1) +
              lag(y.LH.var, -2) +
              lag(y.LH.var, -5) +
              lag(y.LH.var, -7) +
              lag(y.LH.var, -8) +
              lag(y.LH.var, -14) +
              lag(y.LH.var, -21) +
              lag(newC.LH.var, -1) +
              lag(newC.LH.var, -2) +
              lag(newC.LH.var, -3) +
              lag(newC.LH.var, -7) +
              lag(naver.LH.var, -1) + 
              lag(naver.LH.var, -3) +
              lag(naver.LH.var, -4) #+
            , data=LH.var.w)
summary(a) # 0.4895 ??
acf(residuals(a), lag.max = 60) 


#### beauty n = 183  #####
#### creating variables 
beauty.var <- cbind(y.beauty.var, newC.beauty.var, naver.beauty.var)
beauty.var.train <- subset(beauty.var, end=nrow(beauty.var)-28) 
beauty.var.valid1 <- subset(beauty.var, end=nrow(beauty.var)-7) # train???? 21?? ??o?=o?=. # nrow(beauty.var.valid) = 175
beauty.var.valid2 <- subset(beauty.var, start=nrow(beauty.var)-27, end=nrow(beauty.var)-7) # nrow(beauty.var.valid) = 14
beauty.var.test <- subset(beauty.var, start=nrow(beauty.var)-6, end=nrow(beauty.var))

#### fitting var 
VARselect(beauty.var.train, lag.max=9, type="const") # order selection gives VAR(7)
beauty.var.train.fit <- VAR(beauty.var.train, p=8, type="both")
summary(beauty.var.train.fit) 
acf(residuals(beauty.var.train.fit), lag.max = 60) # o?=o?=?? 3?Y8? ???? ??!! ?????? ????.



#### removing insignificant coefficients 
a <- dyn$lm(y.beauty.var ~ lag(y.beauty.var, -1) +
              lag(y.beauty.var, -2) +
              # lag(y.beauty.var, -3) +
              # lag(y.beauty.var, -4) +
              # lag(y.beauty.var, -5) +
              # lag(y.beauty.var, -6) +
              # lag(y.beauty.var, -7) +
              # lag(y.beauty.var, -8) +
              # lag(y.beauty.var, -18) +
              # lag(y.beauty.var, -14) +
              # lag(y.beauty.var, -21) +
              # lag(y.beauty.var, -28)  +
              # lag(newC.beauty.var, -1) +
            lag(newC.beauty.var, -2) +
              # lag(newC.beauty.var, -3) +
              # lag(newC.beauty.var, -4) +
              lag(newC.beauty.var, -5) +
              # lag(newC.beauty.var, -6) +
              # lag(newC.beauty.var, -7) +
              lag(newC.beauty.var, -8) +
              lag(naver.beauty.var, -1) +
              lag(naver.beauty.var, -2) +
              # lag(naver.beauty.var, -3) +
              # lag(naver.beauty.var, -4) +
              # lag(naver.beauty.var, -5) +
              lag(naver.beauty.var, -6) #+
            # lag(naver.beauty.var, -7) +
            # lag(naver.beauty.var, -8)
            , data=beauty.var.train)
summary(a) # 0.3743 --> 2496 ??. ???Z1? o?=o?=?????? ??o?=o?=?? H. ?C>???.



# forecast RMSE = 2.48259e-05 ####
acf(residuals(a), lag.max = 60)  # ?H1W7???.
Box.test(a$residuals, type="Ljung-Box") # ?????L6??? ???o?=o?=? ??.
JB <- predict(a, newdata=beauty.var.valid1)
plot(JB, col=2, xlim=c(17620, 17820), ylim=c(-0.5, 1))
par(new=T)
plot(y.beauty.var,  xlim=c(17620, 17820), ylim=c(-0.5, 1))
accuracy(JB[155:175], beauty.var.valid2[,1])
#### food n = 182  #####
#### creating variables 
newC.food.var1 <- diff(newC.food.var)
naver.food.var1 <- diff(naver.food.var)
food.var <- cbind(y.food.var, newC.food.var1, naver.food.var1)
food.var.train <- subset(food.var, end=nrow(food.var)-28) 
food.var.valid1 <- subset(food.var, end=nrow(food.var)-7) # train???? 21?? ??o?=o?=. # nrow(food.var.valid) = 175
food.var.valid2 <- subset(food.var, start=nrow(food.var)-27, end=nrow(food.var)-7) # nrow(food.var.valid) = 14
food.var.test <- subset(food.var, start=nrow(food.var)-6, end=nrow(food.var))

#### fitting var 
VARselect(food.var.train, lag.max=9, type="const") # order selection gives VAR(7)
food.var.train.fit <- VAR(food.var.train, p=9, type="both")
summary(food.var.train.fit) 
acf(residuals(food.var.train.fit), lag.max = 60) # o?=o?=?? 3?Y8? ???? ??!! ?????? ????.

#### removing insignificant coefficients
# 2 
a <- dyn$lm(y.food.var ~ -1+ lag(y.food.var, -1) +
              lag(y.food.var, -2) +
              lag(y.food.var, -3) +
              # lag(y.food.var, -4) +
              # lag(y.food.var, -5) +
              lag(y.food.var, -6) +
              # lag(y.food.var, -7) +
              lag(y.food.var, -8) +
              # lag(y.food.var, -12) +
              # lag(y.food.var, -14) +
              # lag(y.food.var, -21) +
              # lag(newC.food.var, -1) +
              # lag(newC.food.var, -2) +
              # lag(newC.food.var, -3) +
              # lag(newC.food.var, -4) +
              # lag(newC.food.var, -5) +
              # lag(newC.food.var, -6) +
              # lag(newC.food.var, -7) +
              # lag(newC.food.var, -8) +
            # lag(newC.food.var, -9) +
            lag(naver.food.var, -1) + 
              lag(naver.food.var, -2) +
              # lag(naver.food.var, -3) +
              # lag(naver.food.var, -4) +
              # lag(naver.food.var, -5) +
              lag(naver.food.var, -6) #+
            # lag(naver.food.var, -7) #+
            # lag(naver.food.var, -8) +
            # lag(naver.food.var, -9)
            , data=food.var.train)
summary(a) # 0.5225 -->  5138 ??. ???Z1? o?=o?=?????? ??o?=o?=?? H. ?C>???.
str(a)


# forecast RMSE = 0.5838827 ####
acf(residuals(a), lag.max = 60) 
Box.test(a$residuals, type="Ljung-Box") # Box Ljung ??o?=o?= H0: ?????L4?.
JB <- predict(a, newdata=food.var.valid1)
plot(JB, col=2, xlim=c(17620, 17820), ylim=c(-1, 1))
par(new=T)
plot(y.food.var,  xlim=c(17620, 17820), ylim=c(-1, 1))
accuracy(JB[155:175], food.var.valid2[,1])



###########################
####    ARIMA X N Y    ####
###########################


### [NAVER] code for train and test subsets ########################
#### naver food ####

naver.food.ts.inter <- diff(intervent1)
naver.food.train <- subset(naver.food.arima, end=length(naver.food.arima)-28)
naver.food.valid <- subset(naver.food.arima, start = length(naver.food.arima)-27, end = length(naver.food.arima)-7)
naver.food.test <- subset(naver.food.arima, start = length(naver.food.arima)-6, end = length(naver.food.arima))
naver.food.arima.train <- arima(naver.food.train, c(1,0,1), seasonal=list(order = c(1,0,0), period=7), include.mean = F)
naver.food.test.arima <- Arima(naver.food.valid, model=naver.food.arima.train)

#### naver beauty ####
naver.beauty.train <- subset(naver.beauty.arima, end=length(naver.beauty.arima)-28)
naver.beauty.valid <- subset(naver.beauty.arima, start = length(naver.beauty.arima)-27, end = length(naver.beauty.arima)-7)
naver.beauty.test <- subset(naver.beauty.arima, start = length(naver.beauty.arima)-6, end = length(naver.beauty.arima))
naver.beauty.arima.train <- arima(naver.beauty.train, order = c(2,0,0), include.mean = T)
naver.beauty.test.arima <- Arima(naver.beauty.valid, model=naver.beauty.arima.train)



#### naver LH ####
naver.LH.train <- subset(naver.LH.arima, end=length(naver.LH.arima)-28)
naver.LH.valid <- subset(naver.LH.arima, start = length(naver.LH.arima)-27, end = length(naver.LH.arima)-7)
naver.LH.test <- subset(naver.LH.arima, start = length(naver.LH.arima)-6, end = length(naver.LH.arima))
naver.LH.arima.train <-  arima(naver.LH.train, c(1,1,0), seasonal=list(order = c(1,0,1), period=7), include.mean = T)
naver.LH.test.arima <- Arima(naver.LH.valid, model=naver.LH.arima.train)

#### naver coupon #### 
naver.coupon.train <- subset(naver.coupon.arima, end=length(naver.coupon.arima)-28)
naver.coupon.valid <- subset(naver.coupon.arima, start = length(naver.coupon.arima)-27, end = length(naver.coupon.arima)-7)
naver.coupon.test <- subset(naver.coupon.arima, start = length(naver.coupon.arima)-6, end = length(naver.coupon.arima))
naver.coupon.arima.train <- arima(naver.coupon.train, c(1,0,2), seasonal=list(order = c(1,0,1), period = 7),include.mean = T)
naver.coupon.test.arima <- Arima(naver.coupon.valid, model=naver.coupon.arima.train)



#### naver Furi ####
naver.furi.train <- subset(naver.furi.arima, end=length(naver.furi.arima)-28)
naver.furi.valid <- subset(naver.furi.arima, start = length(naver.furi.arima)-27, end = length(naver.furi.arima)-7)
naver.furi.test <- subset(naver.furi.arima, start = length(naver.furi.arima)-6, end = length(naver.furi.arima))
naver.furi.arima.train <- arima(naver.furi.train, c(1,0,1), seasonal=list(order = c(0,0,1), period = 7), include.mean = F)
naver.furi.test.arima <- Arima(naver.furi.valid, model=naver.furi.arima.train)

#### naver machine ####
naver.machine.train <- subset(naver.machine.arima, end=length(naver.machine.arima)-28)
naver.machine.valid <- subset(naver.machine.arima, start = length(naver.machine.arima)-27, end = length(naver.machine.arima)-7)
naver.machine.test <- subset(naver.machine.arima, start = length(naver.machine.arima)-6, end = length(naver.machine.arima))
naver.machine.arima.train <- arima(naver.machine.train, c(1,0,0), seasonal=list(order = c(1,0,0), period = 7))
naver.machine.test.arima <- Arima(naver.machine.valid, model=naver.machine.arima.train)

#### naver sports ####
naver.sports.train <- subset(naver.sports.arima, end=length(naver.sports.arima)-28)
naver.sports.valid <- subset(naver.sports.arima, start = length(naver.sports.arima)-27, end = length(naver.sports.arima)-7)
naver.sports.test <- subset(naver.sports.arima, start = length(naver.sports.arima)-6, end = length(naver.sports.arima))
naver.sports.arima.train <- arima(naver.sports.train, c(1,0,0), seasonal=list(order = c(0,0,1), period = 7), include.mean = F)
naver.sports.test.arima <- Arima(naver.sports.valid, model=naver.sports.arima.train)

#### naver FZ ####
naver.FZ.train <- subset(naver.fz.arima, end=length(naver.fz.arima)-28)
naver.FZ.valid <- subset(naver.fz.arima, start = length(naver.fz.arima)-27, end = length(naver.fz.arima)-7)
naver.FZ.test <- subset(naver.fz.arima, start = length(naver.fz.arima)-6, end = length(naver.fz.arima))
naver.fz.arima.train <- arima(naver.FZ.train, c(2,0,1), seasonal=list(order = c(1,0,1), period = 7),include.mean = T)
naver.FZ.test.arima <- Arima(naver.FZ.valid, model=naver.fz.arima.train)

#### naver baby ####
naver.baby.train <- subset(naver.baby.arima, end=length(naver.baby.arima)-28)
naver.baby.valid <- subset(naver.baby.arima, start = length(naver.baby.arima)-27, end = length(naver.baby.arima)-7)
naver.baby.test <- subset(naver.baby.arima, start = length(naver.baby.arima)-6, end = length(naver.baby.arima))
naver.baby.arima.train <- arima(naver.baby.train, c(0,0,1), seasonal=list(order = c(1,0,0), period=7), include.mean = T)
naver.baby.test.arima <- Arima(naver.baby.valid, model=naver.baby.arima.train)  # revised

#### naver fc ####
naver.fc.train <- subset(naver.fc.arima, end=length(naver.fc.arima)-28)
naver.fc.valid <- subset(naver.fc.arima, start = length(naver.fc.arima)-27, end = length(naver.fc.arima)-7)
naver.fc.test <- subset(naver.fc.arima, start = length(naver.fc.arima)-6, end = length(naver.fc.arima))
naver.fc.arima.train <-arima(naver.fc.train, c(2,0,2), seasonal=list(order = c(0,0,1), period=7), include.mean = F)
naver.fc.test.arima <- Arima(naver.fc.valid, model=naver.fc.arima.train)

### [NewC] code for train and test subsets ##########################
#### newC food ####
newC.food.ts.intervent4 <- diff(newC.food.ts.intervent4)
newC.food.train <- subset(newC.food.ts.intervent4, end=length(newC.food.ts.intervent4)-28)
newC.food.valid <- subset(newC.food.ts.intervent4, start = length(newC.food.ts.intervent4)-27, end = length(newC.food.ts.intervent4)-7)
newC.food.test <- subset(newC.food.ts.intervent4, start = length(newC.food.ts.intervent4)-6, end = length(newC.food.ts.intervent4))
newC.food.arima.train <- arima(newC.food.train, c(1,0,1), seasonal=list(order = c(1,0,0), period=7), include.mean = F)
newC.food.test.arima <- Arima(newC.food.valid, model=newC.food.arima.train)

#### newC beauty ####
newC.beauty.train <- subset(newC.beauty.ts.intervent1, end=length(newC.beauty.ts.intervent1)-28)
newC.beauty.valid <- subset(newC.beauty.ts.intervent1, start = length(newC.beauty.ts.intervent1)-27, end = length(newC.beauty.ts.intervent1)-7)
newC.beauty.test <- subset(newC.beauty.ts.intervent1, start = length(newC.beauty.ts.intervent1)-6, end = length(newC.beauty.ts.intervent1))
newC.beauty.arima.train <- arima(newC.beauty.train, order = c(2,0,0), include.mean = T)
newC.beauty.test.arima <- Arima(newC.beauty.valid, model=newC.beauty.arima.train)

#### newC  LH ####
station.newC.LH.ts <- diff(newC.LH.ts,7)
newC.LH.train <- subset(station.newC.LH.ts, end=length(station.newC.LH.ts)-28)
newC.LH.valid <- subset(station.newC.LH.ts, start = length(station.newC.LH.ts)-27, end = length(station.newC.LH.ts)-7)
newC.LH.test <- subset(station.newC.LH.ts, start = length(station.newC.LH.ts)-6, end = length(station.newC.LH.ts))
newC.LH.arima.train <-  arima(newC.LH.train, c(1,1,0), seasonal=list(order = c(1,0,1), period=7), include.mean = T)
newC.LH.test.arima <- Arima(newC.LH.valid, model=newC.LH.arima.train)

#### newC coupon #### 
newC.coupon.ts <- diff(station0.newC.coupon.ts,7)
newC.coupon.train <- subset(newC.coupon.ts, end=length(newC.coupon.ts)-28)
newC.coupon.valid <- subset(newC.coupon.ts, start = length(newC.coupon.ts)-27, end = length(newC.coupon.ts)-7)
newC.coupon.test <- subset(newC.coupon.ts, start = length(newC.coupon.ts)-6, end = length(newC.coupon.ts))
newC.coupon.arima.train <- arima(newC.coupon.train, c(1,0,2), seasonal=list(order = c(0,0,1), period = 7),include.mean = T)
newC.coupon.test.arima <- Arima(newC.coupon.valid, model=newC.coupon.arima.train)


#### newC  Furi ####
station.newC.furi.ts <- diff(station0.newC.furi.ts,7)
newC.furi.train <- subset(station.newC.furi.ts, end=length(station.newC.furi.ts)-28)
newC.furi.valid <- subset(station.newC.furi.ts, start = length(station.newC.furi.ts)-27, end = length(station.newC.furi.ts)-7)
newC.furi.test <- subset(station.newC.furi.ts, start = length(station.newC.furi.ts)-6, end = length(station.newC.furi.ts))
newC.furi.arima.train <- arima(newC.furi.train, c(1,0,1), seasonal=list(order = c(1,0,1), period = 7), include.mean = F)
newC.furi.test.arima <- Arima(newC.furi.valid, model=newC.furi.arima.train)

#### newC  machine ####
newC.machine.ts <- diff(newC.machine.ts,7)
newC.machine.train <- subset(newC.machine.ts, end=length(newC.machine.ts)-28)
newC.machine.valid <- subset(newC.machine.ts, start = length(newC.machine.ts)-27, end = length(newC.machine.ts)-7)
newC.machine.test <- subset(newC.machine.ts, start = length(newC.machine.ts)-6, end = length(newC.machine.ts))
newC.machine.arima.train <- arima(newC.machine.train, c(1,0,0), seasonal=list(order = c(1,0,0), period = 7))
naver.machine.test.arima <- Arima(newC.machine.valid, model=naver.machine.arima.train)

#### newC sports ####
station7.sports.newC <- diff(newC.sports.ts.intervent1,7)     
newC.sports.train <- subset(station7.sports.newC, end=length(station7.sports.newC)-28)
newC.sports.valid <- subset(station7.sports.newC, start = length(station7.sports.newC)-27, end = length(station7.sports.newC)-7)
newC.sports.test <- subset(station7.sports.newC, start = length(station7.sports.newC)-6, end = length(station7.sports.newC))
newC.sports.arima.train <- arima(newC.sports.train, c(1,0,0), seasonal=list(order = c(0,0,1), period = 7), include.mean = F)
newC.sports.test.arima <- Arima(newC.sports.valid, model=newC.sports.arima.train)

#### newC  FZ ####

newC.FZ.ts.intervent3 <- diff(newC.FZ.ts.intervent3,7)
newC.FZ.train <- subset(newC.FZ.ts.intervent3, end=length(newC.FZ.ts.intervent3)-28)
newC.FZ.valid <- subset(newC.FZ.ts.intervent3, start = length(newC.FZ.ts.intervent3)-27, end = length(newC.FZ.ts.intervent3)-7)
newC.FZ.test <- subset(newC.FZ.ts.intervent3, start = length(newC.FZ.ts.intervent3)-6, end = length(newC.FZ.ts.intervent3))
newC.FZ.arima.train <- arima(newC.FZ.train, c(2,0,1), seasonal=list(order = c(1,0,1), period = 7),include.mean = T)
newC.FZ.test.arima <- Arima(newC.FZ.valid, model=newC.FZ.arima.train)

#### newC baby ####
newC.baby.train <- subset(newC.baby.ts.intervent2, end=length(newC.baby.ts.intervent2)-28)
newC.baby.valid <- subset(newC.baby.ts.intervent2, start = length(newC.baby.ts.intervent2)-27, end = length(newC.baby.ts.intervent2)-7)
newC.baby.test <- subset(newC.baby.ts.intervent2, start = length(newC.baby.ts.intervent2)-6, end = length(newC.baby.ts.intervent2))
newC.baby.arima.train <- arima(newC.baby.train, c(0,0,1), seasonal=list(order = c(1,0,0), period=7), include.mean = T)
newC.baby.test.arima <- Arima(newC.baby.valid, model=newC.baby.arima.train)

#### newC fc ####
newC.FC.ts.intervent2 <- diff(newC.FC.ts.intervent1,1)  # revised 
newC.FC.train <- subset(newC.FC.ts.intervent2, end=length(newC.FC.ts.intervent2)-28)
newC.FC.valid <- subset(newC.FC.ts.intervent2, start = length(newC.FC.ts.intervent2)-27, end = length(newC.FC.ts.intervent2)-7)
newC.FC.test <- subset(newC.FC.ts.intervent2, start = length(newC.FC.ts.intervent2)-6, end = length(newC.FC.ts.intervent2))
newC.FC.arima.train <-arima(newC.FC.train, c(2,0,2), seasonal=list(order = c(1,0,1), period=7), include.mean = F)
newC.FC.test.arima <- Arima(newC.FC.valid, model = newC.FC.arima.train)



##### cbind
food_train <- cbind(y.food.train, naver.food.train, newC.food.train)
food_test <- cbind(y.food.test, naver.food.test, newC.food.test)
food_valid <- cbind(y.food.valid, naver.food.valid, newC.food.valid)

# beauty
beauty_train <- cbind(y.beauty.train, naver.beauty.train, newC.beauty.train)
beauty_test <- cbind(y.beauty.test, naver.beauty.test, newC.beauty.test)
beauty_valid <- cbind(y.beauty.valid, naver.beauty.valid, newC.beauty.valid)

# LH
LH_train <- cbind(y.LH.train, naver.LH.train, newC.LH.train)
LH_test <- cbind(y.LH.test, naver.LH.test, newC.LH.test)
LH_valid <- cbind(y.LH.valid, naver.LH.valid, newC.LH.valid)

# coupon
coupon_train <- cbind(y.coupon.train, naver.coupon.train, newC.coupon.train)
coupon_test <- cbind(y.coupon.test, naver.coupon.test, newC.coupon.test)
coupon_valid <- cbind(y.coupon.valid, naver.coupon.valid, newC.coupon.valid)

# Furi
furi_train <- cbind(y.furi.train, naver.furi.train, newC.furi.train)
furi_test <- cbind(y.furi.test, naver.furi.test, newC.furi.test)
furi_valid <- cbind(y.furi.valid, naver.furi.valid, newC.furi.valid)

# machine
machine_train <- cbind(y.machine.train, naver.machine.train, newC.machine.train)
machine_test <- cbind(y.machine.test, naver.machine.test, newC.machine.test)
machine_valid <- cbind(y.machine.valid, naver.machine.valid, newC.machine.valid)

# sports
sports_train <- cbind(y.sports.train, naver.sports.train, newC.sports.train)
sports_test <- cbind(y.sports.test, naver.sports.test, newC.sports.test)
sports_valid <- cbind(y.sports.valid, naver.sports.valid, newC.sports.valid)

# FZ
FZ_train <- cbind(y.FZ.train, naver.FZ.train, newC.FZ.train)
FZ_test <- cbind(y.FZ.test, naver.FZ.test, newC.FZ.test)
FZ_valid <- cbind(y.FZ.valid, naver.FZ.valid, newC.FZ.valid)

# baby
baby_train <- cbind(y.baby.train, naver.baby.train, newC.baby.train)
baby_test <- cbind(y.baby.test, naver.baby.test, newC.baby.test)
baby_valid <- cbind(y.baby.valid, naver.baby.valid, newC.baby.valid)

# fc
FC_test <- cbind(y.FC.test, naver.fc.test, newC.FC.test)
FC_valid <- cbind(y.FC.valid, naver.fc.valid, newC.FC.valid)
FC_train <- cbind(y.FC.train, naver.fc.train, newC.FC.train)

############ forecast using X and Y ############
# 1. FC 
FC.arima.train <- arima(y.FC.train, c(2,0,2), seasonal=list(order = c(1,0,1), period=7), include.mean = F,
                        xreg = cbind(as.data.frame(FC_train)$naver.fc.train,
                                     as.data.frame(FC_train)$newC.FC.train))

FC.arima.fc <- forecast(FC.arima.train, xreg = cbind(as.data.frame(FC_train)$naver.fc.train,
                                                     as.data.frame(FC_train)$newC.FC.train))

accuracy(FC.arima.fc, y.FC.valid)  # RMSE 0.0010368966  # revised

# 2. baby 
baby.arima.train <- arima(y.baby.train, c(0,0,1), seasonal=list(order = c(1,0,1), period=7), include.mean = T,
                          xreg = cbind(as.data.frame(baby_train)$naver.baby.train,
                                       as.data.frame(baby_train)$newC.baby.train))

baby.arima.baby <- forecast(baby.arima.train, xreg = cbind(as.data.frame(baby_train)$naver.baby.train,
                                                           as.data.frame(baby_train)$newC.baby.train))

accuracy(baby.arima.baby, y.baby.valid)  # RMSE 1.847151e-04  # revised 

# 3. FZ
FZ.arima.train <- arima(y.FZ.train, c(2,0,1), seasonal=list(order = c(1,0,1), period=7), include.mean = F,
                        xreg = cbind(as.data.frame(FZ_train)$naver.FZ.train,
                                     as.data.frame(FZ_train)$newC.FZ.train))

FZ.arima.FZ <- forecast(FZ.arima.train, xreg = cbind(as.data.frame(FZ_train)$naver.FZ.train,
                                                     as.data.frame(FZ_train)$newC.FZ.train))

accuracy(FZ.arima.FZ, y.FZ.valid)  # RMSE  # revised  2167.764

# 4. sports
sports.arima.train <- arima(y.sports.train, c(1,0,0), seasonal=list(order = c(0,0,1), period=7), include.mean = F,
                            xreg = cbind(as.data.frame(sports_train)$naver.sports.train,
                                         as.data.frame(sports_train)$newC.sports.train))

sports.arima.fc <- forecast(sports.arima.train, xreg = cbind(as.data.frame(sports_train)$naver.sports.train,
                                                             as.data.frame(sports_train)$newC.sports.train))

accuracy(sports.arima.fc, y.sports.valid)  # revised 4.691494e-05


# 5. machine
machine.arima.train <- arima(y.machine.train, c(1,0,0), seasonal=list(order = c(1,0,0), period=7), include.mean = F,
                             xreg = cbind(as.data.frame(machine_train)$naver.machine.train,
                                          as.data.frame(machine_train)$newC.machine.train))

machine.arima.fc <- forecast(machine.arima.train, xreg = cbind(as.data.frame(machine_train)$naver.machine.train,
                                                               as.data.frame(machine_train)$newC.machine.train))

accuracy(machine.arima.fc, y.machine.valid)  # revised 192.2668


# 6. furi
furi.arima.train <- arima(y.furi.train, c(1,0,1), seasonal=list(order = c(0,0,1), period=7), include.mean = F,
                          xreg = cbind(as.data.frame(furi_train)$naver.furi.train,
                                       as.data.frame(furi_train)$newC.furi.train))

furi.arima.fc <- forecast(furi.arima.train, xreg = cbind(as.data.frame(furi_train)$naver.furi.train,
                                                         as.data.frame(furi_train)$newC.furi.train))

accuracy(furi.arima.fc, y.furi.valid)  # RMSE 0.9388238

# 7. coupon
coupon.arima.train <- arima(y.coupon.train, c(1,0,2), seasonal=list(order = c(1,0,0), period=7), include.mean = F,
                            xreg = cbind(as.data.frame(coupon_train)$naver.coupon.train,
                                         as.data.frame(coupon_train)$newC.coupon.train))

coupon.arima.fc <- forecast(coupon.arima.train, xreg = cbind(as.data.frame(coupon_train)$naver.coupon.train,
                                                             as.data.frame(coupon_train)$newC.coupon.train))

accuracy(coupon.arima.fc, y.coupon.valid)  # RMSE 0.2899465

# 8. LH
LH.arima.train <- arima(y.LH.train, c(1,1,0), seasonal=list(order = c(1,0,1), period=7), include.mean = F,
                        xreg = cbind(as.data.frame(LH_train)$naver.LH.train,
                                     as.data.frame(LH_train)$newC.LH.train))

LH.arima.fc <- forecast(LH.arima.train, xreg = cbind(as.data.frame(LH_train)$naver.LH.train,
                                                     as.data.frame(LH_train)$newC.LH.train))

accuracy(LH.arima.fc, y.LH.valid)  # RMSE 6928.854

# 9. beauty
beauty.arima.train <- arima(y.beauty.train, order = c(2,0,0), include.mean = F,
                            xreg = cbind(as.data.frame(beauty_train)$naver.beauty.train,
                                         as.data.frame(beauty_train)$newC.beauty.train))

beauty.arima.fc <- forecast(beauty.arima.train, xreg = cbind(as.data.frame(beauty_train)$naver.beauty.train,
                                                             as.data.frame(beauty_train)$newC.beauty.train))

accuracy(beauty.arima.fc, y.beauty.valid)  # RMSE  revised 4.084971e-05

# 10.food

food.arima.train <- arima(y.food.train, c(1,0,1), seasonal=list(order = c(1,0,0), period=7), include.mean = F,
                          xreg = cbind(as.data.frame(food_train)$naver.food.train,
                                       as.data.frame(food_train)$newC.food.train))

food.arima.fc <- forecast(food.arima.train, xreg = cbind(as.data.frame(food_train)$naver.food.train,
                                                         as.data.frame(food_train)$newC.food.train))

accuracy(food.arima.fc, y.food.valid)  # RMSE  0.5065308


###########################
####       tslm        ####
###########################

######## FC  #####
#naver.fc
naver.FC.train <- subset(station0.naver.fc.ts, end=length(station0.naver.fc.ts)-28)
naver.FC.valid <- subset(station0.naver.fc.ts, start=length(station0.naver.fc.ts)-27, end=length(naver.fc.ts)-7)

#### fit.fc
fit.fc <- tslm(y.FC.train ~ -1 + newC.FC.train + naver.FC.train)
summary(fit.fc) #0.5623

valid.fc <- cbind(newC.FC.valid, naver.FC.valid)
colnames(valid.fc) <- names(coef(fit.fc))
accuracy(predict(fit.fc,valid.fc),y.FC.valid )


######## BABY ####
y.baby.rtrain <- subset(station0.baby.y.intervent2, end=length(station0.baby.y.intervent2)-7)
#
naver.baby.train <- subset(naver.baby.ts, end=length(naver.baby.ts)-28)
naver.baby.valid <- subset(naver.baby.ts, start =length(naver.baby.ts)-27, end=length(naver.baby.ts)-7)
naver.baby.rtrain <- subset(naver.baby.ts, end=length(naver.baby.ts)-7)
naver.baby.test <- subset(naver.baby.ts, start =length(naver.baby.ts)-6, end=length(naver.baby.ts))
#
newC.baby.train <- subset(newC.baby.ts, end=length(newC.baby.ts)-28)
newC.baby.valid <- subset(newC.baby.ts, start =length(newC.baby.ts)-27, end=length(newC.baby.ts)-7)
newC.baby.rtrain <- subset(newC.baby.ts, end=length(naver.baby.ts)-7)
newC.baby.test <- subset(newC.baby.ts, start =length(newC.baby.ts)-6, end=length(newC.baby.ts))
#
fit.baby <- tslm(y.baby.train ~ newC.baby.train + naver.baby.train)
#summary(fit.baby)
inter <- rep(1,21)
valid.baby <- cbind(inter, newC.baby.valid, naver.baby.valid)
colnames(valid.baby) <- names(coef(fit.baby))
accuracy(predict(fit.baby,valid.baby), y.baby.valid )  # RMSE 0.000109496


####### test error ####
fit.baby.test <- tslm(y.baby.rtrain ~ newC.baby.rtrain + naver.baby.rtrain)
summary(fit.baby.test)

inter.test <- rep(1,7)
test.baby <- cbind(inter.test, newC.baby.test, naver.baby.test)
colnames(test.baby) <- names(coef(fit.baby.test))
accuracy(predict(fit.baby.test,test.baby), y.baby.test ) # RMSE 0.0001134885

######## BEAUTY #######
y.beauty.rtrain <- subset(station0.beauty.y, end=length(station0.beauty.y)-7)
#
naver.beauty.train <- subset(naver.beauty.ts , end=length(naver.beauty.ts )-28)
naver.beauty.valid <- subset(naver.beauty.ts , start =length(naver.beauty.ts )-27, end=length(naver.beauty.ts )-7)
naver.beauty.rtrain <- subset(naver.beauty.ts , end=length(naver.beauty.ts )-7)
naver.beauty.test <- subset(naver.beauty.ts , start =length(naver.beauty.ts )-6, end=length(naver.beauty.ts ))
#
newC.beauty.train <- subset(newC.beauty.ts.intervent1, end=length(newC.beauty.ts.intervent1)-28)
newC.beauty.valid <- subset(newC.beauty.ts.intervent1, start =length(newC.beauty.ts.intervent1)-27, end=length(newC.beauty.ts.intervent1)-7)
newC.beauty.rtrain <- subset(newC.beauty.ts.intervent1, end=length(newC.beauty.ts.intervent1)-7)
newC.beauty.test <- subset(newC.beauty.ts.intervent1, start =length(newC.beauty.ts.intervent1)-6, end=length(newC.beauty.ts.intervent1))

###fit
fit.beauty <- tslm(y.beauty.train ~ newC.beauty.train + naver.beauty.train)
#summary(fit.beauty) #0.4004

inter <- rep(1,21)
valid.beauty <- cbind(inter, newC.beauty.valid, naver.beauty.valid)
colnames(valid.beauty) <- names(coef(fit.beauty))
accuracy(predict(fit.beauty,valid.beauty), y.beauty.valid)

### test error ######
fit.beauty.test <- tslm(y.beauty.rtrain ~ newC.beauty.rtrain + naver.beauty.rtrain)
summary(fit.beauty.test) #0.4004

inter.test <- rep(1,7)
test.beauty <- cbind(inter.test, newC.beauty.test, naver.beauty.test)
colnames(test.beauty) <- names(coef(fit.beauty.test)) 
accuracy(predict(fit.beauty.test,test.beauty), y.beauty.test)   # RMSE 2.207473e-05


######## food #######
y.food.rtrain <- subset(station1.food.y, end=length(station1.food.y)-7)

naver.food.ts.inter <- diff(intervent1)
naver.food.train <- subset(naver.food.ts.inter, end=nrow(naver.food.ts.inter)-28)
naver.food.valid <- subset(naver.food.ts.inter, start=nrow(naver.food.ts.inter)-27, end=nrow(naver.food.ts.inter)-7)
naver.food.rtrain <- subset(naver.food.ts.inter, end=nrow(naver.food.ts.inter)-7)
naver.food.test <- subset(naver.food.ts.inter, start=nrow(naver.food.ts.inter)-6, end=nrow(naver.food.ts.inter))
#
newC.food.train <- subset(newC.food.ts.intervent4, end=length(newC.food.ts.intervent4)-28)
newC.food.valid <- subset(newC.food.ts.intervent4, start=length(newC.food.ts.intervent4)-27, end=length(newC.food.ts.intervent4)-7)
newC.food.rtrain <- subset(newC.food.ts.intervent4, end=length(newC.food.ts.intervent4)-7)
newC.food.test <- subset(newC.food.ts.intervent4, start=length(newC.food.ts.intervent4)-6, end=length(newC.food.ts.intervent4))

###fit
fit.food <- tslm(y.food.train ~ -1 + newC.food.train + naver.food.train)
summary(fit.food) #0.188
length(y.food.train)
length(newC.food.train)
length(naver.food.train)

valid.food <- cbind(newC.food.valid, naver.food.valid)
colnames(valid.food) <- names(coef(fit.food))
accuracy(predict(fit.food,valid.food), y.food.valid) # RMSE 0.513739

### test error #### 
fit.food.test <- tslm(y.food.rtrain ~ -1 + newC.food.rtrain + naver.food.rtrain)
summary(fit.food.test) 

test.food <- cbind(newC.food.test, naver.food.test)
colnames(test.food) <- names(coef(fit.food.test))
accuracy(predict(fit.food.test,test.food), y.food.test)   # RMSE 0.3921434



########## COUPON ########
station7.coupon.y.intervent3 <- station7.coupon.y.intervent2 - tc_effect_ts
y.coupon.train <- subset(station7.coupon.y.intervent3, end=length(station7.coupon.y.intervent3)-28)
y.coupon.valid <- subset(station7.coupon.y.intervent3, start=length(station7.coupon.y.intervent3)-27, end=length(station7.coupon.y.intervent3)-7)

##naver
naver.coupon.ts.inter <- diff(intervent3,7)
naver.coupon.train <- subset(naver.coupon.ts.inter, end=length(naver.coupon.ts.inter)-28)
naver.coupon.valid <- subset(naver.coupon.ts.inter, start=length(naver.coupon.ts.inter)-27, end=length(naver.coupon.ts.inter)-7)

####fit.coupon
fit.coupon <- tslm(y.coupon.train ~ -1 + newC.coupon.train + naver.coupon.train)
#summary(fit.coupon) #0.02489

valid.coupon <- cbind(newC.coupon.valid, naver.coupon.valid)
colnames(valid.coupon) <- names(coef(fit.coupon))
accuracy(predict(fit.coupon,valid.coupon), y.coupon.valid )  # RMSE 0.4301258


########## FZ #####
y.fz.rtrain <-  subset(station7.FZ.y, end=length(station7.FZ.y)-7)
## naver
station7.naver.fz.ts <- diff(naver.fz.ts.inter,7)
naver.fz.train <- subset(station7.naver.fz.ts, end=length(station7.naver.fz.ts)-28)
naver.fz.valid <- subset(station7.naver.fz.ts, start=length(station7.naver.fz.ts)-27, end=length(station7.naver.fz.ts)-7)
naver.fz.rtrain <- subset(station7.naver.fz.ts, end=length(station7.naver.fz.ts)-7)
naver.fz.test <- subset(station7.naver.fz.ts, start=length(station7.naver.fz.ts)-6, end=length(station7.naver.fz.ts))
#### newC FZ
newC.fz.rtrain <- subset(newC.FZ.ts.intervent3, end=length(newC.FZ.ts.intervent3)-7)

###fit.fz
fit.fz <- tslm(y.FZ.train ~ -1 + newC.FZ.train + naver.fz.train)
#summary(fit.fz) #0.2193

valid.fz <- cbind(newC.FZ.valid, naver.fz.valid)
colnames(valid.fz) <- names(coef(fit.fz))
accuracy(predict(fit.fz,valid.fz), y.FZ.valid )

###### test error #####
fit.fz.test <- tslm(y.fz.rtrain ~ -1 + newC.fz.rtrain + naver.fz.rtrain)
#summary(fit.fz.test) 

test.fz <- cbind(newC.FZ.test, naver.fz.test)
colnames(test.fz) <- names(coef(fit.fz.test))
accuracy(predict(fit.fz.test,test.fz), y.FZ.test)
predict(fit.fz.test,test.fz)


######### MACHINE ######
#naver
naver.machine.ts.inter <- diff(intervent2,7)
naver.machine.train <- subset(naver.machine.ts.inter, end=length(naver.machine.ts.inter)-28)
naver.machine.valid <- subset(naver.machine.ts.inter, start=length(naver.machine.ts.inter)-27, end=length(naver.machine.ts.inter)-7)

###fit.machine
fit.machine <- tslm(y.machine.train ~ -1 + newC.machine.train + naver.machine.train)
#summary(fit.machine) #0.1426

valid.machine <- cbind(newC.machine.valid, naver.machine.valid)
colnames(valid.machine) <- names(coef(fit.machine))
accuracy(predict(fit.machine,valid.machine), y.machine.valid )

######### FURI  #######
######fit.furi
fit.furi <- tslm(y.furi.train ~ -1 + newC.furi.train + naver.furi.train)
#summary(fit.furi) #-0.01375

valid.furi <- cbind(newC.furi.valid, naver.furi.valid)
colnames(valid.furi) <- names(coef(fit.furi))
accuracy(predict(fit.furi,valid.furi), y.furi.valid )



######## SPORTS ####
y.sports.rtrain <- subset(station7.sports.y, end=length(station7.sports.y)-7)
### naver
station7.sports.naver <- diff(naver.sports.ts,7)
naver.sports.train <- subset(station7.sports.naver, end=length(station7.sports.naver)-28)
naver.sports.valid <- subset(station7.sports.naver, start=length(station7.sports.naver)-27, end=length(station7.sports.naver)-7)
naver.sports.rtrain <- subset(station7.sports.naver, end=length(station7.sports.naver)-7)
naver.sports.test <- subset(station7.sports.naver, start=length(station7.sports.naver)-6, end=length(station7.sports.naver))

#### 4. newC sports 
newC.sports.rtrain <- subset(station7.sports.newC, end=length(station7.sports.newC)-7)
#####fit.sports
fit.sports <- tslm(y.sports.train ~ newC.sports.train + naver.sports.train)
#summary(fit.sports) #0.3344

inter <- rep(1,21)
valid.sports <- cbind(inter,newC.sports.valid, naver.sports.valid)
colnames(valid.sports) <- names(coef(fit.sports))
accuracy(predict(fit.sports,valid.sports), y.sports.valid )

#### test error ########
fit.sports.test <- tslm(y.sports.rtrain ~ -1+newC.sports.rtrain + naver.sports.rtrain)
summary(fit.sports.test) 

test.sports <- cbind(newC.sports.test, naver.sports.test)
colnames(test.sports) <- names(coef(fit.sports.test))
accuracy(predict(fit.sports.test,test.sports), y.sports.test)




######## LIFE.HEALTH #####
#####fit
fit.LH <- tslm(y.LH.train ~ -1 + newC.LH.train + naver.LH.train)
#summary(fit.LH) #4295

valid.LH <- cbind(newC.LH.valid, naver.LH.valid)
colnames(valid.LH) <- names(coef(fit.LH))
accuracy(predict(fit.LH,valid.LH), y.LH.valid)









