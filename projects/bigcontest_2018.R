##### data preprocessing #####
#1. Data set 1-1. PMM으로 man7 data 만들기
#17,000개 pmm으로 채우기
#데이터 불러오기
#setwd("C:/Users/MYCOM/Desktop/bigcon")
#raw_man7 is raw data
raw_man7 <- read.csv("raw_man7.csv")
str(raw_man7)

#결측치 확인
na.count <- apply(raw_man7,2,function(x) sum(is.na(x)))
na.count[na.count>0]

#결측값 많은 순서 TOT_JEOK<TOT_YEA<RETIRE_NEED<TOT_FUND<TOT_ELS_ETE
order(na.count[na.count>0])

#성별 2를 
#0으로 바꾸기
for(i in 1:length(raw_man7$SEX_GBN)){
  if (raw_man7$SEX_GBN[i]==2){
    raw_man7$SEX_GBN[i]<-0
  }
}

#맞벌이 부부, 자녀수 결측치 처리
raw_man7[raw_man7$MARRY_Y==1,]$DOUBLE_IN <- 3
raw_man7[raw_man7$MARRY_Y==1,]$NUMCHILD <- 4
raw_man7[raw_man7$MARRY_Y==2 & is.na(raw_man7$NUMCHILD),]$NUMCHILD <- 0
sum(is.na(raw_man7[,1:8]))

#CHUNG_Y 결측치 처리, 5는 1로 바꾸기
raw_man7[!is.na(raw_man7$CHUNG_Y),]$CHUNG_Y <- 1
raw_man7[is.na(raw_man7$CHUNG_Y),]$CHUNG_Y <- 0

#TOT_CHUNG 결측치 처리
raw_man7[raw_man7$CHUNG_Y==0,]$TOT_CHUNG <- 0


#범주형 변수들 Factorizing
for (i in c(2,5,4,7,8,9,16)){
  raw_man7[,i] <- factor(raw_man7[,i])}
raw_man7[,3] <- ordered(raw_man7[,3])
raw_man7[,6] <- ordered(raw_man7[,6])
str(raw_man7)
# write.csv(raw_man7, "raw_man7.csv")

#결측치 확인
na.count <- apply(raw_man7,2,function(x) sum(is.na(x)))
na.count[na.count>0]

#pmm 순서 TOT_JEOK<TOT_YEA<RETIRE_NEED<TOT_FUND<TOT_ELS_ETE
order(na.count[na.count>0])

######Testing for Normality######

temp_man7<-raw_man7

#log transformaion을 위해 0에 0.00000000001 넣기
for( i in c(12,13,15,17:26,28,31) ) {
  temp_man7[,i]<-as.numeric(temp_man7[,i])
  for( j in 1:nrow(temp_man7) ) {
    if (temp_man7[j,i]==0) temp_man7[j,i]<-0.00000000001
  }
}


#Data processing for normality

temp_man7$TOT_ASSET<-(temp_man7$TOT_ASSET)^0.25
temp_man7$ASS_FIN<-(temp_man7$ASS_FIN)^0.2
temp_man7$ASS_REAL<-(temp_man7$ASS_REAL)^0.45
temp_man7$ASS_ETC<-(temp_man7$ASS_ETC)^0.4
temp_man7$M_TOT_SAVING<-(temp_man7$M_TOT_SAVING)^0.3
temp_man7$M_JEOK<-(temp_man7$M_JEOK)^0.2
temp_man7$RETIRE_NEED<-(temp_man7$RETIRE_NEED)^0.09
temp_man7$TOT_YEA<-(temp_man7$TOT_YEA)^0.05
temp_man7$TOT_FUND<-(temp_man7$TOT_FUND)^0.05
temp_man7$TOT_ELS_ETE<-log(temp_man7$TOT_ELS_ETE)
temp_man7$TOT_SOBI<-(temp_man7$TOT_SOBI)^0.1

#######PMM########
#순서 TOT_JEOK<TOT_YEA<RETIRE_NEED<TOT_FUND<TOT_ELS_ETE
#install.packages("mice")
library(mice)
raw_man7<-temp_man7

#TOT_JEOK 30
imp.totjeok<-mice(raw_man7[,-c(1,12,17,24,25,27,29,32,33,35)],m=5,maxit=30,seed=831)
raw_man7[,30] <- apply(data.frame(complete(imp.totjeok,1)$TOT_JEOK, complete(imp.totjeok,2)$TOT_JEOK, complete(imp.totjeok,3)$TOT_JEOK, 
                                  complete(imp.totjeok,4)$TOT_JEOK, complete(imp.totjeok,5)$TOT_JEOK),1,mean)

#TOT_YEA 29
imp.totyea<-mice(raw_man7[,-c(1,12,17,24,25,27,32,33,35)], m=5, maxit=30, seed=826)
raw_man7[,29] <- apply(data.frame(complete(imp.totyea,1)$TOT_YEA, complete(imp.totyea,2)$TOT_YEA, complete(imp.totyea,3)$TOT_YEA, 
                                  complete(imp.totyea,4)$TOT_YEA, complete(imp.totyea,5)$TOT_YEA),1,mean)
#RETIRE_NEED 27
imp.retireneed<-mice(raw_man7[,-c(1,12,17,24,25,32,33,35)], m=5, maxit=30, seed=831)
raw_man7[,27] <- apply(data.frame(complete(imp.retireneed,1)$RETIRE_NEED, complete(imp.retireneed,2)$RETIRE_NEED, complete(imp.retireneed,3)$RETIRE_NEED, 
                                  complete(imp.retireneed,4)$RETIRE_NEED, complete(imp.retireneed,5)$RETIRE_NEED),1,mean)

#TOT_FUND 32
imp.totfund<-mice(raw_man7[,-c(1,12,17,24,25,33,35)], m=5, maxit=30, seed=831)
raw_man7[,32] <- apply(data.frame(complete(imp.totfund,1)$TOT_FUND, complete(imp.totfund,2)$TOT_FUND, complete(imp.totfund,3)$TOT_FUND, 
                                  complete(imp.totfund,4)$TOT_FUND, complete(imp.totfund,5)$TOT_FUND),1,mean)

#TOT_ELS_ETE
imp.totelsete<-mice(raw_man7[,-c(1,12,17,24,25,35)], m=5, maxit=30, seed=831)
raw_man7[,33] <- apply(data.frame(complete(imp.totelsete,1)$TOT_ELS_ETE, complete(imp.totelsete,2)$TOT_ELS_ETE, complete(imp.totelsete,3)$TOT_ELS_ETE, 
                                  complete(imp.totelsete,4)$TOT_ELS_ETE, complete(imp.totelsete,5)$TOT_ELS_ETE),1,mean)
write.csv(raw_man7, "man7.csv")


############1-2. Data set: 변수 순서 정하기 ############
###14만개 pmm
man7 <- read.csv("man7.csv") #mice_pmm_normal은 17,000개 채운 데이터

#########맨앞열x포함한 전처리 #########

#changing CHUNG_Y to dummy and factorizing
man7$CHUNG_Y <- factor(man7$CHUNG_Y)
for (i in c(3,6,5,8,9,10)){
  man7[,i] <- factor(man7[,i])}
man7[,4] <- ordered(man7[,4])
man7[,7] <- ordered(man7[,7])
str(man7)
#########xgboost 변수 순서 정하기#########
rsq <- vector()
for (i in c(11:16,18:35)){
  lm <- lm(man7[,i]~SEX_GBN+AGE_GBN+JOB_GBN+ADD_GBN+INCOME_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD, data=man7)
  sum<-summary(lm)
  cont <- sum$adj.r.squared
  rsq<-c(rsq,cont)
}
rsq
glm <- glm(man7$CHUNG_Y~SEX_GBN+AGE_GBN+JOB_GBN+ADD_GBN+INCOME_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD, data=man7, family="binomial")
glm2 <- glm(man7$CHUNG_Y~1, data=man7, family="binomial")
1-logLik(glm)/logLik(glm2)
#TOT_SOBI

#2번째 변수는? 
rsq <- vector()
for (i in c(11:16,18:34)){
  lm <- lm(man7[,i]~SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD+TOT_SOBI,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq

glm <- glm(man7$CHUNG_Y~SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD+TOT_SOBI, data=man7, family="binomial")
glm2 <- glm(man7$CHUNG_Y~1, data=man7, family="binomial")
1-logLik(glm)/logLik(glm2)
#TOT_ASSET

#3번째 변수는? ASS_REAL
colnames(man7)
rsq <- vector()
for (i in c(12:16,18:34)){
  lm <- lm(man7[,i]~TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq

glm <- glm(man7$CHUNG_Y~TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD, data=man7, family="binomial")
glm2 <- glm(man7$CHUNG_Y~1, data=man7, family="binomial")
1-logLik(glm)/logLik(glm2)
#ASS_REAL

#4번째 변수는?#ASS_REAL빼고 because of colinearity
rsq <- vector()
colnames(man7)
for (i in c(12,14:16,18:34)){
  lm <- lm(man7[,i]~TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq

glm <- glm(man7$CHUNG_Y~TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD, data=man7, family="binomial")
glm2 <- glm(man7$CHUNG_Y~1, data=man7, family="binomial")
1-logLik(glm)/logLik(glm2)
#RETIRE_NEED

#5번째 변수는?
rsq <- vector()
for (i in c(12,14:16,18:27,29:34)){
  lm <- lm(man7[,i]~RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq

glm <- glm(man7$CHUNG_Y~RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD, data=man7, family="binomial")
glm2 <- glm(man7$CHUNG_Y~1, data=man7, family="binomial")
1-logLik(glm)/logLik(glm2)
#TOT_ELS_ETE

#6번째 변수는?
colnames(man7)
rsq <- vector()
for (i in c(12,14:16,18:27,29:33)){
  lm <- lm(man7[,i]~TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq

glm <- glm(man7$CHUNG_Y~TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD, data=man7, family="binomial")
glm2 <- glm(man7$CHUNG_Y~1, data=man7, family="binomial")
1-logLik(glm)/logLik(glm2)
#ASS_FIN 12

#7번째 변수는?#ASS_ETC 추가
#colnames(man7)
rsq <- vector()
for (i in c(14:16,18:27,29:33)){
  lm <- lm(man7[,i]~ASS_ETC+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq

glm <- glm(man7$CHUNG_Y~ASS_ETC+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD, data=man7, family="binomial")
glm2 <- glm(man7$CHUNG_Y~1, data=man7, family="binomial")
1-logLik(glm)/logLik(glm2)
#TOT_YEA

#8번째 변수는?
rsq <- vector()
for (i in c(14:16,18:27,29,31:33)){
  lm <- lm(man7[,i]~ASS_ETC+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq

glm <- glm(man7$CHUNG_Y~ASS_ETC+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD, data=man7, family="binomial")
glm2 <- glm(man7$CHUNG_Y~1, data=man7, family="binomial")
1-logLik(glm)/logLik(glm2)
#TOT_FUND

#9번째 변수는?
#colnames(man7)
rsq <- vector()
for (i in c(14:16,18:27,29,31:32)){
  lm <- lm(man7[,i]~ASS_ETC+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq

glm <- glm(man7$CHUNG_Y~ASS_ETC+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD, data=man7, family="binomial")
glm2 <- glm(man7$CHUNG_Y~1, data=man7, family="binomial")
1-logLik(glm)/logLik(glm2)
#M_TOT_SAVING

#10번째 변수는?
rsq <- vector()
for (i in c(14,16,18:27,29,31:32)){
  lm <- lm(man7[,i]~ASS_ETC+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq

glm <- glm(man7$CHUNG_Y~ASS_ETC+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD, data=man7, family="binomial")
glm2 <- glm(man7$CHUNG_Y~1, data=man7, family="binomial")
1-logLik(glm)/logLik(glm2)
#M_JEOK

#11번째 변수는?
rsq <- vector()
for (i in c(14,18:27,29,31:32)){
  lm <- lm(man7[,i]~ASS_ETC+M_JEOK+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq

glm <- glm(man7$CHUNG_Y~ASS_ETC+M_JEOK+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD, data=man7, family="binomial")
glm2 <- glm(man7$CHUNG_Y~1, data=man7, family="binomial")
1-logLik(glm)/logLik(glm2)
#TOT_JEOK

#12번째 변수는?
rsq <- vector()
for (i in c(14,18:27,29,32)){
  lm <- lm(man7[,i]~ASS_ETC+TOT_JEOK+M_JEOK+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq

glm <- glm(man7$CHUNG_Y~ASS_ETC+TOT_JEOK+M_JEOK+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD, data=man7, family="binomial")
glm2 <- glm(man7$CHUNG_Y~1, data=man7, family="binomial")
1-logLik(glm)/logLik(glm2)
#CHUNG_Y
#colnames(man7)

#13번째 변수는?
rsq <- vector()
for (i in c(14,18:27,29,32)){
  lm <- lm(man7[,i]~ASS_ETC+CHUNG_Y+TOT_JEOK+M_JEOK+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq
#M_SAVING_INSUR

#14번째 변수는?
rsq <- vector()
for (i in c(14,18:20,22:27,29,32)){
  lm <- lm(man7[,i]~ASS_ETC+M_SAVING_INSUR+CHUNG_Y+TOT_JEOK+M_JEOK+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq
#FOR_RETIRE

#15번째 변수는?
rsq <- vector()
for (i in c(14,18:20,22:27,32)){
  lm <- lm(man7[,i]~ASS_ETC+FOR_RETIRE+M_SAVING_INSUR+CHUNG_Y+TOT_JEOK+M_JEOK+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq
#M_CHUNG

#16번째 변수는?
rsq <- vector()
for (i in c(18:20,23:27,32)){
  lm <- lm(man7[,i]~ASS_ETC+M_CHUNG+FOR_RETIRE+M_SAVING_INSUR+CHUNG_Y+TOT_JEOK+M_JEOK+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq
#TOT_CHUNG
#colnames(man7)

#17번째 변수는?
rsq <- vector()
for (i in c(18:20,23:27)){
  lm <- lm(man7[,i]~ASS_ETC+TOT_CHUNG+M_CHUNG+FOR_RETIRE+M_SAVING_INSUR+CHUNG_Y+TOT_JEOK+M_JEOK+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq
#TOT_DEBT

#18번째 변수는?
rsq <- vector()
for (i in c(18:20,24:27)){
  lm <- lm(man7[,i]~TOT_DEBT+ASS_ETC+TOT_CHUNG+M_CHUNG+FOR_RETIRE+M_SAVING_INSUR+CHUNG_Y+TOT_JEOK+M_JEOK+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq
#D_DAMBO
#colnames(man7)

#19번째 변수는?#D_DAMBO는 TOT_DEBT때문에 포함안함 because of colinearity
rsq <- vector()
for (i in c(18:20,24,26:27)){
  lm <- lm(man7[,i]~ASS_ETC+TOT_DEBT+TOT_CHUNG+M_CHUNG+FOR_RETIRE+M_SAVING_INSUR+CHUNG_Y+TOT_JEOK+M_JEOK+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq
#D_JUTEAKDAMBO

#20번째 변수는?
rsq <- vector()
for (i in c(18:20,24,27)){
  lm <- lm(man7[,i]~ASS_ETC+TOT_DEBT+TOT_CHUNG+M_CHUNG+FOR_RETIRE+M_SAVING_INSUR+CHUNG_Y+TOT_JEOK+M_JEOK+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq
#M_FUND_STOCK
#colnames(man7)

#21번째 변수는?
rsq <- vector()
for (i in c(19:20,24,27)){
  lm <- lm(man7[,i]~ASS_ETC+M_FUND_STOCK+TOT_DEBT+TOT_CHUNG+M_CHUNG+FOR_RETIRE+M_SAVING_INSUR+CHUNG_Y+TOT_JEOK+M_JEOK+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq
#M_FUND

#22번째 변수는?#M_FUND_STOCK제외 because of  colinearity
rsq <- vector()
for (i in c(20,24,27)){
  lm <- lm(man7[,i]~ASS_ETC+M_FUND+TOT_DEBT+TOT_CHUNG+M_CHUNG+FOR_RETIRE+M_SAVING_INSUR+CHUNG_Y+TOT_JEOK+M_JEOK+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq
#D_SHONYONG

#23번째 변수는?
rsq <- vector()
for (i in c(20,27)){
  lm <- lm(man7[,i]~ASS_ETC+M_FUND+D_SHINYONG+TOT_DEBT+TOT_CHUNG+M_CHUNG+FOR_RETIRE+M_SAVING_INSUR+CHUNG_Y+TOT_JEOK+M_JEOK+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq
#D_JEONSEA

#24번째 변수는?
rsq <- vector()
for (i in 20){
  lm <- lm(man7[,i]~ASS_ETC+D_SHINYONG+M_FUND+D_JEONSEA+TOT_DEBT+TOT_CHUNG+M_CHUNG+FOR_RETIRE+M_SAVING_INSUR+CHUNG_Y+TOT_JEOK+M_JEOK+M_TOT_SAVING+TOT_FUND+TOT_YEA+ASS_FIN+TOT_ELS_ETE+RETIRE_NEED+TOT_ASSET+TOT_SOBI+SEX_GBN+AGE_GBN+INCOME_GBN+JOB_GBN+ADD_GBN+MARRY_Y+DOUBLE_IN+NUMCHILD,data=man7)
  sum <- summary(lm)
  Cont <- sum$adj.r.squared
  rsq <- c(rsq, cont)
}
rsq
#################### 1-3. Data set: Xboost ###############
###### man7 ######
man7<-read.csv("man7.csv")
attach(man7)
str(man7)
man7<-man7[,-1]


man7$AGE_GBN<-as.factor(AGE_GBN)
man7$JOB_GBN<-as.factor(JOB_GBN)
man7$ADD_GBN<-as.factor(ADD_GBN)
man7$INCOME_GBN<-as.factor(INCOME_GBN)
man7$MARRY_Y<-as.factor(MARRY_Y)
man7$DOUBLE_IN<-factor(DOUBLE_IN)
man7$NUMCHILD<-factor(NUMCHILD) 


#giving levels -> MARRY_Y & DOUBLE_IN & NUMCHILD
levels(man7$MARRY_Y)<-c(1:3)

#making dummies
age<-model.matrix(~AGE_GBN-1,man7)
job<-model.matrix(~JOB_GBN-1,man7)
add<-model.matrix(~ADD_GBN-1,man7)
income<-model.matrix(~INCOME_GBN-1,man7)
marry<-model.matrix(~MARRY_Y-1,man7)
double<-model.matrix(~DOUBLE_IN-1,man7)
numchild<-model.matrix(~NUMCHILD-1,man7)

#Dummy Variable
MARRY_Y3<-rep(0,17076)
man7.dummy1<-cbind(age,job)
man7.dummy1<-cbind(man7.dummy1,add)
man7.dummy1<-cbind(man7.dummy1,income)
man7.dummy1<-cbind(man7.dummy1,marry)
man7.dummy1<-cbind(man7.dummy1,MARRY_Y3)
man7.dummy1<-cbind(man7.dummy1,double)
man7.dummy1<-cbind(man7.dummy1,numchild)
man7.dummy1<-cbind(SEX_GBN,man7.dummy1)
colnames(man7.dummy1)
man7.dummy2<-man7[,c(10:35)]
#man7 MARRY_Y3 & NUMCHILD4
man7.dummy<-cbind(man7.dummy1,man7.dummy2)






###### sip4#######
sip4<- read.csv("raw_sip4.csv")
detach(man7)
attach(sip4)
str(sip4)

for(i in 1:length(sip4$SEX_GBN)){
  if (sip4$SEX_GBN[i]==2){
    sip4$SEX_GBN[i]<-0
  }
}
sip4$AGE_GBN<-as.factor(AGE_GBN)
sip4$JOB_GBN<-as.factor(JOB_GBN)
sip4$ADD_GBN<-as.factor(ADD_GBN)
sip4$INCOME_GBN<-as.factor(INCOME_GBN)
sip4$MARRY_Y<-as.factor(MARRY_Y)
sip4$DOUBLE_IN<-factor(DOUBLE_IN)
sip4$NUMCHILD<-factor(NUMCHILD) 

#giving levels -> MARRY_Y & DOUBLE_IN & NUMCHILD
levels(sip4$MARRY_Y)<-c(1:3)
levels(sip4$DOUBLE_IN)<-c(1:3)
levels(sip4$NUMCHILD)<-c(0:4) 

#changing personal data's NULL
sip4[is.na(sip4$MARRY_Y),7] <- 3    
sip4[is.na(sip4$DOUBLE_IN),8] <- 3
sip4[is.na(sip4$NUMCHILD),9] <- 4

#####소비데이터 채우기
library("dplyr")
temp_man7_2<-man7[,c(2,3,5,35)]
unique<-unique(temp_man7_2)
z<-left_join(sip4,unique,by=c("SEX_GBN"="SEX_GBN","AGE_GBN"="AGE_GBN","ADD_GBN"="ADD_GBN"),all=TRUE)
sum(complete.cases(z$M_CRD_SPD.y))
sip4$M_CRD_SPD<-z$M_CRD_SPD.y

#sip4<-sip4_1
#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)

sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)
str(sip4.dummy)

###### Matirx ######
#bind.dummy<-rbind(man7.dummy,sip4.dummy)
#bind.mat<-data.matrix(bind.dummy)
man7.mat<-data.matrix(man7.dummy)
sip4.mat<-data.matrix(sip4.dummy)
#이하 각 변수들에 대한 xgboost 시행 시 최적의 parameter 값을 시행착오법을 통해 구함
#이하의 코드들은 최적 parameter에 해당하는 값만 남겨둔 것
##################    TOT_SOBI TRAIN#########################
#install.packages("caret")
#install.packages("plyr")
#install.packages("xgboost")
#install.packages("Metrics")
library(caret)
library(plyr)
library(xgboost)
library(Metrics)
#library(tidyverse)

# training data for TOT_SOBI  
train_data99 <- man7.mat[,c(1:38,63)]    
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]


##################    TOT_SOBI TEST ##################
# Creating test data set                 
test_data99 <- sip4.mat[,c(1:38,63)] 
test_labels <- test_data99[,39]
test_data<-test_data99[,-39]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_TOT_SOBI <- xgboost(data = dtrain, 
                          subsample = 1,     nround = 500,
                          eval_metric = "rmse",
                          eta = 0.025,
                          max_depth = 4,
                          gamma = 0.025,
                          colsample_bytree=0.7,
                          min_child_weight = 0.6,
                          seed=101,
                          objective = "reg:linear")
pred_TOT_SOBI <- predict(model_TOT_SOBI, dtest)  
detach(sip4)
sip4_1<-sip4
sip4_1[,34]<-pred_TOT_SOBI 



##################    TOT_ASSET TRAIN ##################
# training data for TOT_ASSET  
train_data99 <- man7.mat[,c(1:38,39)]   
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]


##################    TOT_ASSET TEST ##################
sip4<-sip4_1
#making dummies
attach(sip4)
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)


###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set    
test_data99 <- sip4.mat[,c(1:38,39)] 
test_labels <- test_data99[,39]
test_data<-test_data99[,-39]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_TOT_ASSET <- xgboost(data = dtrain,
                           subsample = 1,     nround = 500,
                           eval_metric = "rmse",
                           eta = 0.02,
                           max_depth = 7,
                           gamma = 6,
                           colsample_bytree=0.7,
                           min_child_weight = 0.6,
                           seed=101,
                           objective = "reg:linear")
pred_TOT_ASSET <- predict(model_TOT_ASSET, dtest)  
sip4_1[,10]<-pred_TOT_ASSET 



##################    ASS_REAL TRAIN ############
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI
train_data22<- man7.mat[,c(1:38,39,41,63)] 
train_labels <- train_data22[,40]
train_data<-train_data22[,-40]


##################    ASS_REAL TEST ###############
sip4<-sip4_1
#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set 
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI
# sip4.mat[,64]   M_CRD_SPD  
test_data99 <- sip4.mat[,c(1:38,39,41,63)] 
test_labels <- test_data99[,40]
test_data<-test_data99[,-40]


# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_ASS_REAL <- xgboost(data = dtrain, # the data
                          subsample = 1,     nround = 600, 
                          eval_metric = "rmse",
                          eta = 0.025,
                          max_depth = 8,
                          gamma =0.01 ,
                          colsample_bytree= 0.6,
                          min_child_weight = 1,
                          seed=101,
                          objective = "reg:linear")

pred_ASS_REAL <- predict(model_ASS_REAL, dtest)  
sip4_1[,12]<-pred_ASS_REAL 
ass_real_test<-pred_ASS_REAL


##################    RETIRE_NEED TRAIN ###############
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL
train_data22<- man7.mat[,c(1:38,39,56,63)] 
train_labels <- train_data22[,40]
train_data<-train_data22[,-40]


##################    RETIRE_NEED TEST##############
sip4<-sip4_1
#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set 
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL         
# sip4.mat[,64]
test_data99 <- sip4.mat[,c(1:38,39,56,63)] 
test_labels <- test_data99[,40]
test_data<-test_data99[,-40]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_RETIRE_NEED <- xgboost(data = dtrain, # the data
                             subsample = 1,     nround = 700, 
                             eval_metric = "rmse",
                             eta = 0.025,
                             max_depth = 4,
                             gamma =0 ,
                             colsample_bytree= 1,
                             min_child_weight = 1,
                             seed=101,
                             objective = "reg:linear")

pred_RETIRE_NEED <- predict(model_RETIRE_NEED, dtest)  
sip4_1[,27]<-pred_RETIRE_NEED 

################## 35 TOT_ELS_ETE TRAIN ##################
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL+ 62TOT_ELS_ETE
train_data22<- man7.mat[,c(1:38,39,56,62,63)] 
train_labels <- train_data22[,41]
train_data<-train_data22[,-41]

##################    TOT_ELS_ETE TEST ###############
sip4<-sip4_1
#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set 
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL+ 62TOT_ELS_ETE
# sip4.mat[,64]   M_CRD_SPD  
test_data99 <- sip4.mat[,c(1:38,39,56,62,63)] 
test_labels <- test_data99[,41]
test_data<-test_data99[,-41]


# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_TOT_ELS_ETE <- xgboost(data = dtrain, # the data
                             subsample = 1,     nround = 500, 
                             eval_metric = "rmse",
                             eta = 0.025,
                             max_depth = 8,
                             gamma =0.5 ,
                             colsample_bytree= 0.8,
                             min_child_weight = 1,
                             seed=101,
                             objective = "reg:linear")

pred_TOT_ELS_ETE <- predict(model_TOT_ELS_ETE, dtest)  
sip4_1[,33]<-pred_TOT_ELS_ETE 

##################    ASS_FIN TRIAN #############
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL+ 62TOT_ELS_ETE+40ASS_FIN
train_data22<- man7.mat[,c(1:38,39,40,56,62,63)] 
train_labels <- train_data22[,40]
train_data<-train_data22[,-40]

##################    ASS_FIN TEST##############
sip4<-sip4_1
#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set 
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL + 62TOT_ELS_ETE
# sip4.mat[,64]   M_CRD_SPD  
test_data99 <- sip4.mat[,c(1:38,39,40,56,62,63)] 
test_labels <- test_data99[,40]
test_data<-test_data99[,-40]


# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_ASS_FIN <- xgboost(data = dtrain, # the data
                         subsample = 1,     nround = 700, 
                         eval_metric = "rmse",
                         eta = 0.025,
                         max_depth = 4,
                         gamma =1 ,
                         colsample_bytree= 1,
                         min_child_weight = 1,
                         seed=101,
                         objective = "reg:linear")

pred_ASS_FIN <- predict(model_ASS_FIN, dtest)  
sip4_1[,11]<-pred_ASS_FIN 

#########ASS_ETC by subtraction###########
totasset<-(pred_TOT_ASSET)^4
assfin<-(pred_ASS_FIN)^5
assreal<-(pred_ASS_REAL)^(1/0.45)

#if pred_ASS_REAL<0, replace them to 0  
ass_real_test_zero<-pred_ASS_REAL

for (i in 1:length(pred_ASS_REAL)){
  if (pred_ASS_REAL[i] < 0){
    ass_real_test_zero[i]<-0
  }
}

#assreal ^(1/0.45)
assreal<-(ass_real_test_zero)^(1/0.45)
#assetc 계산
assetc<-totasset-assfin-assreal

assetc_zero<-assetc
for (i in 1:length(assetc)){
  if (assetc[i] < 0){
    assetc_zero[i]<-0
  }
}
#fill them! 
sip4_1[,13]<-assetc_zero#transform안한ㄱ
sip4_1[,12]<-ass_real_test_zero#transform한값


################## 8  TOT_YEA TRAIN ##############

#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL + 62TOT_ELS_ETE+40ASS_FIN+42ASS_ETC+58TOT_YEA
train_data22<- man7.mat[,c(1:38,39,40,42,56,58,62,63)] 
train_labels <- train_data22[,43]
train_data<-train_data22[,-43]


##################    TOT_YEA TEST##############
sip4<-sip4_1
#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set 
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL  + 62TOT_ELS_ETE
# sip4.mat[,64]   M_CRD_SPD  
test_data99 <- sip4.mat[,c(1:38,39,40,42,56,58,62,63)] 
test_labels <- test_data99[,43]
test_data<-test_data99[,-43]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_TOT_YEA <- xgboost(data = dtrain, # the data
                         subsample = 1,     nround = 500, 
                         eval_metric = "rmse",
                         eta = 0.025,
                         max_depth = 8,
                         gamma =0 ,
                         colsample_bytree= 0.8,
                         min_child_weight = 1,
                         seed=101,
                         objective = "reg:linear")

pred_TOT_YEA <- predict(model_TOT_YEA, dtest)  
sip4_1[,29]<-pred_TOT_YEA 


################## 9  TOT_FUND TRIAN ####################
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL          + 62TOT_ELS_ETE+40ASS_FIN+58TOT_YEA
#61TOT_FUND
train_data22<- man7.mat[,c(1:38,39,40,42,56,58,61,62,63)] 
train_labels <- train_data22[,44]
train_data<-train_data22[,-44]


##################    TOT_FUND TEST##############
sip4<-sip4_1
#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set 
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL          + 62TOT_ELS_ETE+40ASS_FIN+58TOT_YEA
# sip4.mat[,64]   M_CRD_SPD  
test_data99 <- sip4.mat[,c(1:38,39,40,42,56,58,61,62,63)] 
test_labels <- test_data99[,44]
test_data<-test_data99[,-44]


# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_TOT_FUND <- xgboost(data = dtrain, # the data
                          subsample = 1,     nround = 500, 
                          eval_metric = "rmse",
                          eta = 0.025,
                          max_depth = 6,
                          gamma = 0.01,
                          colsample_bytree=0.8,
                          min_child_weight = 1,
                          seed=101,
                          objective = "reg:linear")

pred_TOT_FUND <- predict(model_TOT_FUND, dtest)  
sip4_1[,32]<-pred_TOT_FUND




################## 10 M_TOT_SAVING train##################
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL          + 62TOT_ELS_ETE+40ASS_FIN+58TOT_YEA
#61TOT_FUND+43M_TOT_SAVING
train_data22<- man7.mat[,c(1:38,39,40,42,43,56,58,61,62,63)] 
train_labels <- train_data22[,42]
train_data<-train_data22[,-42]

##################    M_TOT_SAVING TEST #################
sip4<-sip4_1#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set 
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL          + 62TOT_ELS_ETE+40ASS_FIN+58TOT_YEA
test_data99 <- sip4.mat[,c(1:38,39,40,42,43,56,58,61,62,63)] 
test_labels <- test_data99[,42]
test_data<-test_data99[,-42]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_M_TOT_SAVING <- xgboost(data = dtrain, # the data
                              subsample = 1,     nround = 500, 
                              eval_metric = "rmse",
                              eta = 0.025,
                              max_depth = 4,
                              gamma = 0.5,
                              colsample_bytree=1,
                              min_child_weight = 1,
                              seed=101,
                              objective = "reg:linear")

pred_M_TOT_SAVING <- predict(model_M_TOT_SAVING, dtest)  
sip4_1[,14]<-pred_M_TOT_SAVING 

##################    M_JEOK TRAIN ##################
train_data44<- man7.mat[,c(1:38,44,42,43,61,58,40,62,56,39,63)] 
train_labels <- train_data44[,39]
train_data<-train_data44[,-39]


##################    M_JEOK TEST##############
sip4<-sip4_1#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set 

test_data44<- sip4.mat[,c(1:38,44,42,43,61,58,40,62,56,39,63)] 
test_labels <- test_data44[,39]
test_data<-test_data44[,-39]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_M_JEOK <- xgboost(data = dtrain, # the data
                        eval_metric = "rmse",
                        eval_metric = "rmse",
                        min_child_weight = 1, max_depth = 6,
                        nround = 500,  eta = 0.025, gamma =  0.1, 
                        colsample_bytree=.9, subsample =0.6, seed=101,
                        objective = "reg:linear")

pred_M_JEOK <- predict(model_M_JEOK, dtest)  
sip4_1[,15]<-pred_M_JEOK 


##################    TOT_JEOK TRAIN ############
train_data59<- man7.mat[,c(1:38,59,44,42,43,61,58,40,62,56,39,63)] 
train_labels <- train_data59[,39]
train_data<-train_data59[,-39]

##################    TOT_JEOK TEST##############
sip4<-sip4_1#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)


# Creating test data set 
test_data59<- sip4.mat[,c(1:38,59,44,42,43,61,58,40,62,56,39,63)] 
test_labels <- test_data59[,39]
test_data<-test_data59[,-39]


# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_TOT_JEOK <- xgboost(data = dtrain, # the data
                          eval_metric = "rmse",
                          min_child_weight = 1, max_depth = 4,
                          nround = 500,  eta = 0.025, gamma = 0, 
                          colsample_bytree=1, subsample = 0.7, 
                          seed=101,           objective = "reg:linear")

pred_TOT_JEOK <- predict(model_TOT_JEOK, dtest)  
sip4_1[,30]<-pred_TOT_JEOK 


##################    CHUNG_Y TRAIN #############
train_data45<- man7.mat[,c(1:38,45,42,59,44,43,61,58,40,62,56,39,63)] 
train_labels <- train_data45[,39]
train_data<-train_data45[,-39]


##################    CHUNG_Y TEST############## 
sip4<-sip4_1#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set 
test_data45<- sip4.mat[,c(1:38,45,42,59,44,43,61,58,40,62,56,39,63)] 
test_labels <- test_data45[,39]
test_data<-test_data45[,-39]
# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_CHUNG_Y <- xgboost(data = dtrain, # the data
                         eval_metric = "error",
                         min_child_weight = 5, max_depth =  8,
                         nround = 500,  eta = 0.025, gamma =  0, 
                         colsample_bytree=0.7, subsample = 0.6, 
                         seed=101,
                         objective = "binary:logistic")

pred_CHUNG_Y <- predict(model_CHUNG_Y, dtest)  

sip4_1[,16]<-pred_CHUNG_Y 

##################    M_SAVING_INSUR ############
train_data49<- man7.mat[,c(1:38,49,42,45,59,44,43,61,58,40,62,56,39,63)] 
train_labels <- train_data49[,39]
train_data<-train_data49[,-39]


##################    M_SAVING_INSUR TEST############## 
sip4<-sip4_1#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set 
test_data57<- sip4.mat[,c(1:38,49,42,45,59,44,43,61,58,40,62,56,39,63)] 
test_labels <- test_data57[,39]
test_data<-test_data57[,-39]

colnames(test_data)==colnames(train_data)
# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_M_SAVING_INSUR <- xgboost(data = dtrain, # the data
                                eval_metric = "rmse",
                                min_child_weight =5, max_depth =  6,
                                nround = 500,  eta = 0.025, gamma =  5, 
                                colsample_bytree=0.9, subsample = 0.9, seed=101,
                                objective = "reg:linear")


pred_M_SAVING_INSUR <- predict(model_M_SAVING_INSUR, dtest)  
sip4_1[,20]<-pred_M_SAVING_INSUR 

########## 그냥 불러왔더니 오류. M_SAVING.INSUR 수정 #############
colnames(sip4_1)[20]="M_SAVING_INSUR"

##################    FOR_RETIRE TRAIN ############
train_data57<- man7.mat[,c(1:38,57,42,49,45,59,44,43,61,58,40,62,56,39,63)] 
train_labels <- train_data57[,39]
train_data<-train_data57[,-39]


##################    FOR_RETIRE TEST##############
sip4<-sip4_1#making dummies
detach(sip4)
attach(sip4)
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set
test_data57<- sip4.mat[,c(1:38,57,42,49,45,59,44,43,61,58,40,62,56,39,63)] 
test_labels <- test_data57[,39]
test_data<-test_data57[,-39]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_FOR_RETIRE <- xgboost(data = dtrain, # the data
                            eval_metric = "rmse",
                            min_child_weight =3, max_depth = 6,
                            nround = 500,  eta = 0.025, gamma = 0, 
                            colsample_bytree=0.9, subsample = 0.6, seed=101,
                            objective = "reg:linear")

pred_FOR_RETIRE <- predict(model_FOR_RETIRE, dtest)  
sip4_1[,28]<-pred_FOR_RETIRE 
##################    M_CHUNG ##############
train_data50<- man7.mat[,c(1:38,50,42,57,49,45,59,44,43,61,58,40,62,56,39,63)] 
train_labels <- train_data50[,39]
train_data<-train_data50[,-39]


##################    M_CHUNG TEST##############
sip4<-sip4_1#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set 
test_data50<-sip4.mat[,c(1:38,50,42,57,49,45,59,44,43,61,58,40,62,56,39,63)] 
test_labels <- test_data50[,39]
test_data<-test_data50[,-39]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_M_CHUNG <- xgboost(data = dtrain, # the data
                         eval_metric = "rmse",
                         min_child_weight =1, max_depth = 8,
                         nround = 500,  eta = 0.025, gamma = 0.4, 
                         colsample_bytree=0.8, subsample = 0.9, seed=101,
                         objective = "reg:linear")


pred_M_CHUNG <- predict(model_M_CHUNG, dtest)  
sip4_1[,21]<-pred_M_CHUNG 

##################    TOT_CHUNG TRAIN ##############
train_data60<- man7.mat[,c(1:38,60,42,50,57,49,45,59,44,43,61,58,40,62,56,39,63)] 
train_labels <- train_data60[,39]
train_data<-train_data60[,-39]


##################    TOT_CHUNG TEST##############
sip4<-sip4_1#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set 
test_data60<- sip4.mat[,c(1:38,60,42,50,57,49,45,59,44,43,61,58,40,62,56,39,63)] 
test_labels <- test_data60[,39]
test_data<-test_data60[,-39]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_TOT_CHUNG <- xgboost(data = dtrain, # the data
                           eval_metric = "rmse",
                           min_child_weight = 1, max_depth = 4,
                           nround = 500,  eta = 0.025, gamma =  0, 
                           colsample_bytree=1, subsample=1, 
                           seed=101,
                           objective = "reg:linear")

pred_TOT_CHUNG <- predict(model_TOT_CHUNG, dtest)  
sip4_1[,31]<-pred_TOT_CHUNG 

##################    TOT_DEBT TRAIN #####
train_data51<- man7.mat[,c(1:38,51,42,60,50,57,49,45,59,44,43,61,58,40,62,56,39,63)] 
train_labels <- train_data51[,39]
train_data<-train_data51[,-39]

##################    TOT_DEBT TEST##############
sip4<-sip4_1#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set 
test_data51<-sip4.mat[,c(1:38,51,42,60,50,57,49,45,59,44,43,61,58,40,62,56,39,63)] 
test_labels <- test_data51[,39]
test_data<-test_data51[,-39]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_TOT_DEBT <- xgboost(data = dtrain, # the data
                          eval_metric = "rmse",
                          min_child_weight = 3, max_depth =  6,
                          nround = 500,  eta = 0.025, gamma = 0, 
                          colsample_bytree=0.6, subsample = 0.7, 
                          seed=101,
                          objective = "reg:linear")

pred_TOT_DEBT <- predict(model_TOT_DEBT, dtest)  
sip4_1[,22]<-pred_TOT_DEBT 


##################    D_DAMBO TRAIN ##################
#TOT_SOBI+TOT_ASSET+RETIRE_NEED+TOT_ELS_ETE+ASS_FIN+TOT_YEA+TOT_FUND+M_TOT_SAVING+M_JEOK+TOT_JEOK+CHUNG_Y+M_SAVING_INSUR+FOR_RETIRE+M_CHUNG+TOT_CHUNG+TOT_DEBT

train_data99<- man7.mat[,c(1:38,53,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]

##################    D_DAMBO TEST #########################
sip4<-sip4_1#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set                 
test_data99 <- sip4.mat[,c(1:38,53,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
test_labels <- test_data99[,39]
test_data<-test_data99[,-39]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_D_DAMBO <- xgboost(data = dtrain, 
                         subsample = 1,     
                         nround = 200, 
                         eval_metric = "rmse",
                         eta = 0.025,
                         max_depth = 4,
                         gamma = 1,
                         colsample_bytree=1,
                         min_child_weight = 1,
                         seed=101,
                         objective = "reg:linear")
pred_D_DAMBO <- predict(model_D_DAMBO, dtest)  
sip4_1[,24]<-pred_D_DAMBO 


##################    D_JUTEAKDAMBO TRAIN ##################
#TOT_SOBI+TOT_ASSET+RETIRE_NEED+TOT_ELS_ETE+ASS_FIN+TOT_YEA+TOT_FUND+M_TOT_SAVING+M_JEOK+TOT_JEOK+CHUNG_Y+M_SAVING_INSUR+FOR_RETIRE+M_CHUNG+TOT_CHUNG+TOT_DEBT
train_data99<- man7.mat[,c(1:38,54,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]

##################    D_JUTEAKDAMBO TEST #########################
sip4<-sip4_1#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set                 
test_data99 <- sip4.mat[,c(1:38,54,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
test_labels <- test_data99[,39]
test_data<-test_data99[,-39]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_D_JUTEAKDAMBO <- xgboost(data = dtrain, 
                               subsample = 1,     
                               nround = 200, 
                               eval_metric = "rmse",
                               eta = 0.025,
                               max_depth = 2,
                               gamma = 1,
                               colsample_bytree=1,
                               min_child_weight = 1,
                               seed=101,
                               objective = "reg:linear")
pred_D_JUTEAKDAMBO <- predict(model_D_JUTEAKDAMBO, dtest)  
sip4_1[,25]<-pred_D_JUTEAKDAMBO 

##################    M_FUND_STOCK TRAIN ##################
train_data99<- man7.mat[,c(1:38,46,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]

##################    M_FUND_STOCK TEST #########################
sip4<-sip4_1#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set                 
test_data99 <- sip4.mat[,c(1:38,46,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
test_labels <- test_data99[,39]
test_data<-test_data99[,-39]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_M_FUND_STOCK <- xgboost(data = dtrain, 
                              subsample = 1,     
                              nround = 1000, 
                              eval_metric = "rmse",
                              eta = 0.025,
                              max_depth = 7,
                              gamma = 5,
                              colsample_bytree=0.5,
                              min_child_weight = 0.3,
                              seed=101,
                              objective = "reg:linear")
pred_M_FUND_STOCK <- predict(model_M_FUND_STOCK, dtest)  
sip4_1[,17]<-pred_M_FUND_STOCK 

##################    M_FUND TRAIN ##################
train_data99<- man7.mat[,c(1:38,47,46,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]

##################    M_FUND TEST #########################
sip4<-sip4_1#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set           
test_data99 <- sip4.mat[,c(1:38,47,46,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
test_labels <- test_data99[,39]
test_data<-test_data99[,-39]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_M_FUND <- xgboost(data = dtrain, 
                        subsample = 1,     
                        nround = 200, 
                        eval_metric = "rmse",
                        eta = 0.025,
                        max_depth = 4,
                        gamma = 1,
                        colsample_bytree=1,
                        min_child_weight = 1,
                        seed=101,
                        objective = "reg:linear")
pred_M_FUND <- predict(model_M_FUND, dtest)  
sip4_1[,18]<-pred_M_FUND 


##################    D_SHINYONG TRAIN ##################
train_data99<- man7.mat[,c(1:38,52,47,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]

##################    D_SHINYONG TEST #########################
sip4<-sip4_1#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set           
test_data99 <- sip4.mat[,c(1:38,52,47,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
test_labels <- test_data99[,39]
test_data<-test_data99[,-39]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_D_SHINYONG <- xgboost(data = dtrain, 
                            subsample = 1,     
                            nround = 400, 
                            eval_metric = "rmse",
                            eta = 0.02,
                            max_depth = 4,
                            gamma = 0.5,
                            colsample_bytree=0.8,
                            min_child_weight = 0.5,
                            seed=101,
                            objective = "reg:linear")
pred_D_SHINYONG <- predict(model_D_SHINYONG, dtest)  
sip4_1[,23]<-pred_D_SHINYONG 
##################    D_JEONSEA TRAIN ##################
train_data99<- man7.mat[,c(1:38,55,52,47,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]
##################    D_JEONSEA TEST #########################
sip4<-sip4_1#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
str(sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set                 
test_data99 <- sip4.mat[,c(1:38,55,52,47,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
test_labels <- test_data99[,39]
test_data<-test_data99[,-39]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_D_JEONSEA <- xgboost(data = dtrain, 
                           subsample = 1,     
                           nround = 850, 
                           early_stopping_rounds = 850,
                           eval_metric = "rmse",
                           eta = 0.01,
                           max_depth = 7,
                           gamma = 5.2,
                           colsample_bytree=0.5,
                           min_child_weight = 2,
                           seed=101,
                           objective = "reg:linear")
pred_D_JEONSEA <- predict(model_D_JEONSEA, dtest)  
sip4_1[,26]<-pred_D_JEONSEA 

##################    M_STOCK TRAIN ##################
train_data99<- man7.mat[,c(1:38,48,46,55,52,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]

##################    M_STOCK TEST #########################
sip4<-sip4_1#making dummies
age.t<-model.matrix(~AGE_GBN-1,sip4)
job.t<-model.matrix(~JOB_GBN-1,sip4)
add.t<-model.matrix(~ADD_GBN-1,sip4)
income.t<-model.matrix(~INCOME_GBN-1,sip4)
marry.t<-model.matrix(~MARRY_Y-1,sip4)
double.t<-model.matrix(~DOUBLE_IN-1,sip4)
numchild.t<-model.matrix(~NUMCHILD-1,sip4)

#Dummy Variable
sip4.dummy1<-cbind(age.t,job.t)
sip4.dummy1<-cbind(sip4.dummy1,add.t)
sip4.dummy1<-cbind(sip4.dummy1,income.t)
sip4.dummy1<-cbind(sip4.dummy1,marry.t)
sip4.dummy1<-cbind(sip4.dummy1,double.t)
sip4.dummy1<-cbind(sip4.dummy1,numchild.t)
sip4.dummy1<-cbind(SEX_GBN,sip4.dummy1)
sip4.dummy2<-sip4[,c(10:35)]
#sip4 MARRY_Y3 & NUMCHILD4
sip4.dummy<-cbind(sip4.dummy1,sip4.dummy2)

###### Matirx 
sip4.mat<-data.matrix(sip4.dummy)

# Creating test data set                 
test_data99 <- sip4.mat[,c(1:38,48,46,55,52,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
test_labels <- test_data99[,39]
test_data<-test_data99[,-39]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)
# train a model using our training data
model_M_STOCK <- xgboost(data = dtrain, 
                         subsample = 1,     
                         nround = 300, 
                         early_stopping_rounds = 100,
                         eval_metric = "rmse",
                         eta = 0.025,
                         max_depth = 4,
                         gamma = 1.5,
                         colsample_bytree=1,
                         min_child_weight = 0.8,
                         seed=101,
                         objective = "reg:linear")
pred_M_STOCK <- predict(model_M_STOCK, dtest)  
sip4_1[,19]<-pred_M_STOCK 

############ Finale:back transformation ###########

for(i in c(15,17,19:35)){
  for(j in 1:141750)
    if(sip4_1[j,i]<0) sip4_1[j,i]<-0}

for(j in c(1:141750)){
  if(sip4_1[j,16]>0.5) sip4_1[j,16]=1 else sip4_1[j,16]<-0 }


sip4_final<-sip4_1
sip4_final$TOT_ASSET<-(sip4_1$TOT_ASSET)^4
sip4_final$ASS_FIN<-(sip4_1$ASS_FIN)^5
sip4_final$ASS_REAL<-(sip4_1$ASS_REAL)^(1/0.45)
#sip4_final$ASS_ETC<-(sip4_1$ASS_ETC)^0.4
sip4_final$M_TOT_SAVING<-(sip4_1$M_TOT_SAVING)^(1/0.3)
sip4_final$M_JEOK<-(sip4_1$M_JEOK)^(1/0.2)
sip4_final$RETIRE_NEED<-(sip4_1$RETIRE_NEED)^(1/0.09)
sip4_final$TOT_YEA<-(sip4_1$TOT_YEA)^(1/0.05)
sip4_final$TOT_FUND<-(sip4_1$TOT_FUND)^(1/0.05)
sip4_final$TOT_ELS_ETE<-exp(sip4_1$TOT_ELS_ETE)
sip4_final$TOT_SOBI<-(sip4_1$TOT_SOBI)^(1/0.1)

write.csv(sip4_final,"Full.csv")




################ 2. Mapping Table: SOM #################
sip4<-sip4_final
sip4_f<-sip4[,-c(1:9,16)] #we're using only financial variables except chung_y

# scaling
sip4_f.mat<- scale(sip4_f)

# cluster group 숫자 정하기 by wss plot
wss<-vector()
for (i in seq(50,1000,by=50)){set.seed(123) 
  wss[i] <- sum(kmeans(sip4_f.mat, centers=i,iter.max=60)$withinss)}
wss<-wss[!is.na(wss)]
plot(seq(50,1000,by=50), wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Elbow Method")

#Making a SOM for sip4_f 
#install.packages("kohonen")
require(kohonen)
som_grid1<-somgrid(xdim=30,ydim=30, topo="hexagonal")
set.seed(101)
som_model1<-som(sip4_f.mat,grid=som_grid1,keep.data=TRUE,rlen=1000) #rlen=# of steps, (iterations)
#Training Progress: If the curve is continually decreasing, more iterations are required.
plot(som_model1, type="changes")

#Node Counts:. Aim for at least 5-10 samples per node when choosing map size. 
plot(som_model1, type="count")

#Codes / Weight vectors
plot(som_model1, type="codes")

## use hierarchical clustering to cluster the codebook vectors
som_cluster1<-cutree(hclust(dist(som_model1$codes[[1]])),250)

# plot these results:
plot(som_model1, type="mapping", bgcol=som_cluster1, main="Clusters")
add.cluster.boundaries(som_model1, som_cluster1)

str(sip4)
sip4_som_mapping <-cbind(sip4,group=som_cluster1[som_model1$unit.classif])

#mapping table 형성
mapping_table<-sip4_som_mapping[,c(1,36)]
head(mapping_table)
write.csv(mapping_table,"mapping_table.csv")




############## #3. Quantiles per group###########3

#백분위 분포표 형성
##ass_fin11, M-tot_saving14, tot_sobi34
#quantiles

final<-data.frame(rep(0,102))
ccc<-data.frame()
for(i in 1:250 ){
  sub<-subset(sip4_som_mapping,group==i)
  quan=as.numeric(quantile(sub$ASS_FIN,seq(0,1,by=0.01)[-1])[1:100])    
  ccc<-c(i, "ASS_FIN", quan)
  final<-cbind(final,ccc)
}
for(i in 1:250 ){
  sub<-subset(sip4_som_mapping,group==i)
  quan=as.numeric(quantile(sub$M_TOT_SAVING,seq(0,1,by=0.01)[-1])[1:100])    
  ccc<-c(i, "M_TOT_SAVING", quan)
  final<-cbind(final,ccc)
}
for(i in 1:250 ){
  sub<-subset(sip4_som_mapping,group==i)
  quan=as.numeric(quantile(sub$TOT_SOBI,seq(0,1,by=0.01)[-1])[1:100])    
  ccc<-c(i, "TOT_SOBI", quan)
  final<-cbind(final,ccc)
}
final<-as.data.frame(t(as.matrix(final[,-1])))
colnames(final)<-c("group","type",paste0("quan",1:100))
rownames(final)<-1:750
#write.table(final, "C:/Users/yoonyoung.chang/Desktop/final_quan_table.csv")
write.csv(final,"quantile.csv")

############## 4. Extra Credit ###############
mapping_table<-read.csv("mapping_table.csv")
sip4<-read.csv("Full.csv")
sip4<-sip4[,-1]
extra<-cbind(sip4,group=mapping_table[,3])
extra<-extra[,-c(1,10:35)]

# categorical variables
attach(extra)
extra$AGE_GBN<-as.numeric(AGE_GBN)
extra$JOB_GBN<-as.factor(JOB_GBN)
extra$ADD_GBN<-as.factor(ADD_GBN)
extra$INCOME_GBN<-as.numeric(INCOME_GBN)
extra$MARRY_Y<-as.factor(MARRY_Y)
extra$DOUBLE_IN<-factor(DOUBLE_IN)
extra$NUMCHILD<-factor(NUMCHILD) 
#extra$node<-factor(node) 
#AGE & INCOME --> no dummy
job.e<-model.matrix(~JOB_GBN-1,extra)
add.e<-model.matrix(~ADD_GBN-1,extra)
marry.e<-model.matrix(~MARRY_Y-1,extra)
double.e<-model.matrix(~DOUBLE_IN-1,extra)
numchild.e<-model.matrix(~NUMCHILD-1,extra)

extra.dummy<-cbind(SEX_GBN=extra[,1],AGE_GBN=extra[,2],job.e,add.e,INCOME_GBN=extra[,5],marry.e,double.e,numchild.e,group=mapping_table[,3])
extra.mat<-extra.dummy

#install.packages("caret")
#install.packages("plyr")
#install.packages("xgboost")
#install.packages("Metrics")
library(caret)
library(plyr)
library(xgboost)
#library(Metrics)
#library(tidyverse)

# training data for TOT_SOBI  
data99 <- extra.mat 
labels <- data99[,29]
data<-data99[,-29]
#   
length(labels) #training 17076

#Training
numberOfTrainingSamples <- round(length(labels) * .7)
set.seed(130)
samp<-sample(length(labels),numberOfTrainingSamples)

# training data
train_data <- data[samp,]
train_labels <- labels[samp]

# training data
test_data <- data[-samp,]
test_labels <- labels[-samp]
# make matrix
length(train_labels)
length(test_labels)
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

# train a model using our training data

model1 <- xgboost(data = dtrain, 
                  subsample = 1,
                  nround = 10,
                  eval_metric = "merror",
                  eta = 0.025,
                  max_depth = 4,
                  gamma = 0.025,
                  colsample_bytree=0.7,
                  min_child_weight = 0.6,
                  seed=101,
                  num_class=251,
                  objective = "multi:softmax")

trainpred <- predict(model1, dtest)
postResample(trainpred,test_labels) #R^2=0.6


#2nd try
model2 <- xgboost(data = dtrain, 
                  subsample = 1,
                  nround = 20,
                  eval_metric = "merror",
                  eta = 0.025,
                  max_depth = 4,
                  gamma = 0.01,
                  colsample_bytree=0.7,
                  min_child_weight = 0.6,
                  seed=101,
                  num_class=251,
                  objective = "multi:softmax")

importance_matrix <- xgb.importance(names(data), model = model2)
#R^2 0.65

#3rd try
model3 <- xgboost(data = dtrain, 
                  subsample = 1,
                  nround = 100,
                  eval_metric = "merror",
                  eta = 0.025,
                  max_depth = 4,
                  gamma = 0.01,
                  colsample_bytree=0.7,
                  min_child_weight = 0.6,
                  seed=101,
                  num_class=251,
                  objective = "multi:softmax")

trainpred <- predict(model3, dtest)
postResample(trainpred,test_labels)
importance_matrix <- xgb.importance(names(data), model = model3)

# and plot it!
xgb.plot.importance(importance_matrix)


############## cf) xgboost training code#############33
###### man7 ######
man7<-read.csv("man7.csv")
attach(man7)
str(man7)
man7<-man7[,-1]


man7$AGE_GBN<-as.factor(AGE_GBN)
man7$JOB_GBN<-as.factor(JOB_GBN)
man7$ADD_GBN<-as.factor(ADD_GBN)
man7$INCOME_GBN<-as.factor(INCOME_GBN)
man7$MARRY_Y<-as.factor(MARRY_Y)
man7$DOUBLE_IN<-factor(DOUBLE_IN)
man7$NUMCHILD<-factor(NUMCHILD) 


#giving levels -> MARRY_Y & DOUBLE_IN & NUMCHILD
levels(man7$MARRY_Y)<-c(1:3)

#making dummies
age<-model.matrix(~AGE_GBN-1,man7)
job<-model.matrix(~JOB_GBN-1,man7)
add<-model.matrix(~ADD_GBN-1,man7)
income<-model.matrix(~INCOME_GBN-1,man7)
marry<-model.matrix(~MARRY_Y-1,man7)
double<-model.matrix(~DOUBLE_IN-1,man7)
numchild<-model.matrix(~NUMCHILD-1,man7)

#Dummy Variable
#MARRY_Y3<-rep(0,17076)
man7.dummy1<-cbind(age,job)
man7.dummy1<-cbind(man7.dummy1,add)
man7.dummy1<-cbind(man7.dummy1,income)
man7.dummy1<-cbind(man7.dummy1,marry)
#man7.dummy1<-cbind(man7.dummy1,MARRY_Y3)
man7.dummy1<-cbind(man7.dummy1,double)
man7.dummy1<-cbind(man7.dummy1,numchild)
man7.dummy1<-cbind(SEX_GBN,man7.dummy1)
colnames(man7.dummy1)
man7.dummy2<-man7[,c(10:35)]
#man7 MARRY_Y3 & NUMCHILD4
man7.dummy<-cbind(man7.dummy1,man7.dummy2)

###### Matirx ######
man7.mat<-data.matrix(man7.dummy)

##################    TOT_SOBI TRAIN#########################
#install.packages("caret")
#install.packages("plyr")
#install.packages("xgboost")
#install.packages("Metrics")
library(caret)
library(plyr)
library(xgboost)
library(Metrics)

# training data for TOT_SOBI  
train_data99 <- man7.mat[,c(1:38,63)]    
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]

#Training
numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# make matrix
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

# train a model using our training data
model <- xgboost(data = dtrain1, 
                 subsample = 1,     nround = 500,
                 eval_metric = "rmse",
                 eta = 0.025,
                 max_depth = 4,
                 gamma = 0.025,
                 colsample_bytree=0.7,
                 min_child_weight = 0.6,
                 seed=101,
                 objective = "reg:linear")
trainpred <- predict(model, dtrain2)
postResample(trainpred,train_labels2)
boxplot(trainpred,main="TOT_SOBI TRAIN")

plot(man7$TOT_SOBI,col=2,main="SOBI",ylim=c(1,2.4))
par(new=T)
plot(trainpred,ylim=c(1,2.4))
summary(trainpred)
summary(man7$TOT_SOBI)
length(subset(man7, TOT_SOBI<1.360)[,34])/length(man7[,34])
length(subset(man7, TOT_SOBI>1.919)[,34])/length(man7[,34])

##################    TOT_ASSET TRAIN ##################
# training data for TOT_ASSET  
train_data99 <- man7.mat[,c(1:38,39)]   
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]

#Training
numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

#check length, matrix
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

model <- xgboost(data = dtrain1,
                 subsample = 1,     nround = 500,
                 eval_metric = "rmse",
                 eta = 0.02,
                 max_depth = 7,
                 gamma = 6,
                 colsample_bytree=0.7,
                 min_child_weight = 0.6,
                 seed=101,
                 objective = "reg:linear")
trainpred <- predict(model, dtrain2)
postResample(trainpred,train_labels2)

plot(man7$TOT_ASSET,col=2,main="TOT_ASSET",ylim=c(1,25))
par(new=T)
plot(trainpred,ylim=c(1,25))
length(subset(man7, TOT_ASSET<summary(trainpred)[1])[,1])/length(man7[,1])
length(subset(man7, TOT_ASSET>summary(trainpred)[6])[,1])/length(man7[,1])

##################    ASS_REAL TRAIN ############
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI
train_data22<- man7.mat[,c(1:38,39,41,63)] 
train_labels <- train_data22[,40]
train_data<-train_data22[,-40]

numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

# train a model using our training data
model2 <- xgboost(data = dtrain1, # the data
                  subsample = 1,     nround = 600, 
                  eval_metric = "rmse",
                  eta = 0.025,
                  max_depth = 8,
                  gamma =0.01 ,
                  colsample_bytree= 0.6,
                  min_child_weight = 1,
                  seed=101,
                  objective = "reg:linear")

trainpred <- predict(model2, dtrain2)
postResample(trainpred,train_labels2)

summary(trainpred)
summary(man7$ASS_REAL)
plot(man7$ASS_REAL,col=2,main="ASS_REAL",ylim=c(-10,310))
par(new=T)
plot(trainpred,ylim=c(-10,310))
length(subset(man7, ASS_REAL<0.0001)[,12])
sum(trainpred<0.0001)
length(subset(man7, ASS_REAL<summary(trainpred)[1])[,10])/length(man7[,10]) 
length(subset(man7, ASS_REAL>summary(trainpred)[6])[,10])/length(man7[,10]) 

ass_real_train<-trainpred

##################    RETIRE_NEED TRAIN ###############
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL
train_data22<- man7.mat[,c(1:38,39,56,63)] 
train_labels <- train_data22[,40]
train_data<-train_data22[,-40]


numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

#write.csv(samp, "samp.csv")

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

# train a model using our training data
model2 <- xgboost(data = dtrain1, # the data
                  subsample = 1,     nround = 700, 
                  eval_metric = "rmse",
                  eta = 0.025,
                  max_depth = 4,
                  gamma = 0,
                  colsample_bytree=1,
                  min_child_weight = 1,
                  seed=101,
                  objective = "reg:linear")


trainpred <- predict(model2, dtrain2)
postResample(trainpred,train_labels2)
plot(man7$RETIRE_NEED,col=2,main="RETIRE_NEED",ylim=c(1.2,2))
par(new=T)
plot(trainpred,ylim=c(1.2,2))

summary(trainpred)
summary(man7$RETIRE_NEED)
length(subset(man7, RETIRE_NEED>1.743)[,27])/length(man7[,27])
length(subset(man7, RETIRE_NEED<1.439)[,27])/length(man7[,27])

################## 35 TOT_ELS_ETE TRAIN ##################
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL+ 62TOT_ELS_ETE
train_data22<- man7.mat[,c(1:38,39,56,62,63)] 
train_labels <- train_data22[,41]
train_data<-train_data22[,-41]


numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

#write.csv(samp, "samp.csv")

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

# train a model using our training data
model2 <- xgboost(data = dtrain1, # the data
                  subsample = 1,     nround = 500, 
                  eval_metric = "rmse",
                  eta = 0.025,
                  max_depth = 8,
                  gamma = 0.5,
                  colsample_bytree=0.8,
                  min_child_weight = 1,
                  seed=101,
                  objective = "reg:linear")

trainpred <- predict(model2, dtrain2)
postResample(trainpred,train_labels2)

summary(trainpred)
summary(man7$TOT_ELS_ETE)
length(subset(man7, TOT_ELS_ETE>summary(trainpred)[6])[,33])/length(man7[,33])
length(subset(man7, TOT_ELS_ETE<summary(trainpred)[1])[,33])/length(man7[,33])
colnames(man7)
plot(man7$TOT_ELS_ETE,col=2,main="TOT_ELS_ETE",ylim=c(3.5,10.5))
par(new=T)
plot(trainpred,ylim=c(3.5,10.5))


##################    ASS_FIN TRIAN #############
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL+ 62TOT_ELS_ETE+40ASS_FIN
train_data22<- man7.mat[,c(1:38,39,40,56,62,63)] 
train_labels <- train_data22[,40]
train_data<-train_data22[,-40]

numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

#write.csv(samp, "samp.csv")

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)

dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

# train a model using our training data
model2 <- xgboost(data = dtrain1, # the data
                  subsample = 1,     nround = 700, 
                  eval_metric = "rmse",
                  eta = 0.025,
                  max_depth = 4,
                  gamma = 1,
                  colsample_bytree=1,
                  min_child_weight = 1,
                  seed=101,
                  objective = "reg:linear")


trainpred <- predict(model2, dtrain2)
postResample(trainpred,train_labels2)

summary(trainpred)
summary(man7$ASS_FIN)
length(subset(man7, ASS_FIN>summary(trainpred)[6])[,11])/length(man7[,11])
length(subset(man7, ASS_FIN<summary(trainpred)[1])[,11])/length(man7[,11])
plot(man7$ASS_FIN,col=2,main="ASS_FIN",ylim=c(1.5,12))
par(new=T)
plot(trainpred,ylim=c(1.5,12))

################## 8  TOT_YEA TRAIN ##############

#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL + 62TOT_ELS_ETE+40ASS_FIN+42ASS_ETC+58TOT_YEA
train_data22<- man7.mat[,c(1:38,39,40,42,56,58,62,63)] 
train_labels <- train_data22[,43]
train_data<-train_data22[,-43]


numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

#write.csv(samp, "samp.csv")

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

# train a model using our training data
model2 <- xgboost(data = dtrain1, # the data
                  subsample = 1,     nround = 500, 
                  eval_metric = "rmse",
                  eta = 0.025,
                  max_depth = 8,
                  gamma = 0,
                  colsample_bytree=0.8,
                  min_child_weight = 1,
                  seed=101,
                  objective = "reg:linear")

trainpred <- predict(model2, dtrain2)
postResample(trainpred,train_labels2)

summary(trainpred)
summary(man7$TOT_YEA)
length(subset(man7, TOT_YEA>summary(trainpred)[6])[,30])/length(man7[,30])
length(subset(man7, TOT_YEA<summary(trainpred)[1])[,30])/length(man7[,30])
plot(man7$TOT_YEA,col=2,main="TOT_YEA",ylim=c(1.2,1.8))
par(new=T)
plot(trainpred,ylim=c(1.2,1.8))


################## 9  TOT_FUND TRIAN ####################
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL          + 62TOT_ELS_ETE+40ASS_FIN+58TOT_YEA
#61TOT_FUND
train_data22<- man7.mat[,c(1:38,39,40,42,56,58,61,62,63)] 
train_labels <- train_data22[,44]
train_data<-train_data22[,-44]


numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

#write.csv(samp, "samp.csv")

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

# train a model using our training data
model2 <- xgboost(data = dtrain1, # the data
                  subsample = 1,     nround = 500, 
                  eval_metric = "rmse",
                  eta = 0.025,
                  max_depth = 6,
                  gamma = 0.01,
                  colsample_bytree=0.8,
                  min_child_weight = 1,
                  seed=101,
                  objective = "reg:linear")

trainpred <- predict(model2, dtrain2)
postResample(trainpred,train_labels2)

summary(trainpred)
summary(man7$TOT_FUND)
length(subset(man7, TOT_FUND>summary(trainpred)[6])[,32])/length(man7[,32])
length(subset(man7, TOT_FUND<summary(trainpred))[,32])/length(man7[,32])
sum(man7$TOT_FUND<1.234)
sum(trainpred)
plot(man7$TOT_FUND,col=2,main="TOT_FUND",ylim=c(1.2,1.8))
par(new=T)
plot(trainpred,ylim=c(1.2,1.8))


################## 10 M_TOT_SAVING train##################
#39TOT_ASSET,41ASS_REAL,63TOT_SOBI,56RETIRE_NEED-ASS_REAL          + 62TOT_ELS_ETE+40ASS_FIN+58TOT_YEA
#61TOT_FUND+43M_TOT_SAVING
train_data22<- man7.mat[,c(1:38,39,40,42,43,56,58,61,62,63)] 
train_labels <- train_data22[,42]
train_data<-train_data22[,-42]


numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

#write.csv(samp, "samp.csv")

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

# train a model using our training data
model2 <- xgboost(data = dtrain1, # the data
                  subsample = 1,     nround = 500, 
                  eval_metric = "rmse",
                  eta = 0.025,
                  max_depth = 4,
                  gamma = 0.5,
                  colsample_bytree=1,
                  min_child_weight = 1,
                  seed=101,
                  objective = "reg:linear")


trainpred <- predict(model2, dtrain2)
postResample(trainpred,train_labels2)

summary(trainpred)
summary(man7$M_TOT_SAVING)
length(subset(man7, M_TOT_SAVING>summary(trainpred)[6])[,14])/length(man7[,14])
length(subset(man7, M_TOT_SAVING<summary(trainpred)[1])[,14])/length(man7[,14])
sum(man7$M_TOT_SAVING<1.234)
sum(trainpred)
plot(man7$M_TOT_SAVING,col=2,main="M_TOT_SAVING",ylim=c(0,11))#ylim  ??    summary    
par(new=T)
plot(trainpred,ylim=c(0,11))



##################    M_JEOK TRAIN ##################
train_data44<- man7.mat[,c(1:38,44,42,43,61,58,40,62,56,39,63)] 
train_labels <- train_data44[,39]
train_data<-train_data44[,-39]

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)
model <- xgboost(data = dtrain1,
                 eval_metric = "rmse",
                 min_child_weight = 1, max_depth = 6,
                 nround = 500,  eta = 0.025, gamma =  0.1, 
                 colsample_bytree=.9, subsample =0.6, 
                 seed=101,
                 objective = "reg:linear")
trainpred <- predict(model, dtrain2)
result<-postResample(trainpred,train_labels2)

summary(trainpred)
summary(man7$M_JEOK)
length(subset(man7, M_YEA>summary(trainpred)[6])[,30])/length(man7[,30])
length(subset(man7, M_JEOK<summary(trainpred)[1])[,30])/length(man7[,30])


##################    TOT_JEOK TRAIN ############
train_data59<- man7.mat[,c(1:38,59,44,42,43,61,58,40,62,56,39,63)] 
train_labels <- train_data59[,39]
train_data<-train_data59[,-39]


numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

# training data

train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]


# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

model <- xgboost(data = dtrain1,
                 eval_metric = "rmse",
                 min_child_weight = 1, max_depth = 4,
                 nround = 500,  eta = 0.025, gamma = 0, 
                 colsample_bytree=1, subsample = 0.7, 
                 seed=101,
                 objective = "reg:linear")
trainpred <- predict(model, dtrain2)
postResample(trainpred,train_labels2)
summary(trainpred)
summary(man7$TOT_JEOK)

over_max=length(subset(man7, TOT_JEOK>summary(trainpred)[6])[,1])/length(man7[,27])
below_min=length(subset(man7, TOT_JEOK<summary(trainpred)[1])[,1])/length(man7[,27])
over_max; below_min;


##################    CHUNG_Y TRAIN #############
train_data45<- man7.mat[,c(1:38,45,42,59,44,43,61,58,40,62,56,39,63)] 
train_labels <- train_data45[,39]
train_data<-train_data45[,-39]

# training data

train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)


model <- xgboost(data = dtrain1,
                 eval_metric = "error",
                 min_child_weight = 5, max_depth =  8,
                 nround = 500,  eta = 0.025, gamma =  0, 
                 colsample_bytree=0.7, subsample = 0.6, 
                 seed=101,
                 objective = "binary:logistic")
trainpred <- predict(model, dtrain2)
result<-postResample(trainpred,train_labels2)
err <- mean(as.numeric(pred > 0.5) != train_labels2)
print(paste("test-error=", err))


##################    M_SAVING_INSUR ############
train_data49<- man7.mat[,c(1:38,49,42,45,59,44,43,61,58,40,62,56,39,63)] 
train_labels <- train_data49[,39]
train_data<-train_data49[,-39]

# training data

train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]


# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)
model <- xgboost(data = dtrain1,
                 eval_metric = "rmse",
                 min_child_weight = 5, max_depth =  6,
                 nround = 500,  eta = 0.025, gamma =  5, 
                 colsample_bytree=0.9, subsample = 0.9, 
                 seed=101,
                 objective = "reg:linear")
trainpred <- predict(model, dtrain2)
result<-postResample(trainpred,train_labels2)
sm<-summary(trainpred)

below_min=length(subset(man7,  M_SAVING_INSUR>sm[6])[,1])/length(man7[,27])
over_max=length(subset(man7,M_SAVING_INSUR<sm[1])[,1])/length(man7[,27])
below_min; over_max;



##################    FOR_RETIRE TRAIN ############
train_data57<- man7.mat[,c(1:38,57,42,49,45,59,44,43,61,58,40,62,56,39,63)] 
train_labels <- train_data57[,39]
train_data<-train_data57[,-39]

# training data

train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

model <- xgboost(data = dtrain1,
                 eval_metric = "rmse",
                 min_child_weight = 3, max_depth = 6,
                 nround = 500,  eta = 0.025, gamma = 0, 
                 colsample_bytree=0.9, subsample = 0.6, 
                 seed=101,
                 objective = "reg:linear")
trainpred <- predict(model, dtrain2)
result<-postResample(trainpred,train_labels2)
sm<-summary(trainpred)

below_min=length(subset(man7, FOR_RETIRE>sm[6])[,1])/length(man7[,27])
over_max=length(subset(man7, FOR_RETIRE<sm[1])[,1])/length(man7[,27])


##################    M_CHUNG TRAIN ##############
train_data50<- man7.mat[,c(1:38,50,42,57,49,45,59,44,43,61,58,40,62,56,39,63)] 
train_labels <- train_data50[,39]
train_data<-train_data50[,-39]

# training data

train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]


# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

model <- xgboost(data = dtrain1,
                 eval_metric = "rmse",
                 min_child_weight = 1, max_depth = 8,
                 nround = 500,  eta = 0.025, gamma = 0.4, 
                 colsample_bytree=0.8, subsample = 0.9, 
                 seed=101,
                 objective = "reg:linear")
trainpred <- predict(model, dtrain2)
result<-postResample(trainpred,train_labels2)
sm<-summary(trainpred)

below_min=length(subset(man7, M_CHUNG>sm[6])[,1])/length(man7[,27])
over_max=length(subset(man7, M_CHUNG<sm[1])[,1])/length(man7[,27])
#plot(man7$M_CHUNG,col=2,ylim=c(0,260))
par(new=T)
#plot(trainpred,ylim=c(0,260))


##################    TOT_CHUNG TRAIN ##############
train_data60<- man7.mat[,c(1:38,60,42,50,57,49,45,59,44,43,61,58,40,62,56,39,63)] 
train_labels <- train_data60[,39]
train_data<-train_data60[,-39]

# training data

train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]


# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

model <- xgboost(data = dtrain1,
                 eval_metric = "rmse",
                 min_child_weight = 1, max_depth = 4,
                 nround = 500,  eta = 0.025, gamma =  0, 
                 colsample_bytree=1, subsample=1, 
                 seed=101,
                 objective = "reg:linear")
trainpred <- predict(model, dtrain2)
result<-postResample(trainpred,train_labels2)
sm<-summary(trainpred)

below_min=length(subset(man7, TOT_CHUNG>sm[6])[,1])/length(man7[,27])
over_max=length(subset(man7, TOT_CHUNG<sm[1])[,1])/length(man7[,27])


#plot(man7$TOT_CHUNG,col=2,ylim=c(-500,12000))
par(new=T)
#plot(trainpred,ylim=c(-500,12000))


##################    TOT_DEBT TRAIN #####
train_data51<- man7.mat[,c(1:38,51,42,60,50,57,49,45,59,44,43,61,58,40,62,56,39,63)] 
train_labels <- train_data51[,39]
train_data<-train_data51[,-39]
# training data

train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]


# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)


model <- xgboost(data = dtrain1,
                 eval_metric = "rmse",
                 min_child_weight = 3, max_depth =  6,
                 nround = 500,  eta = 0.025, gamma = 0, 
                 colsample_bytree=0.6, subsample = 0.7, 
                 seed=101,
                 objective = "reg:linear")
trainpred <- predict(model, dtrain2)
result<-postResample(trainpred,train_labels2)
sm<-summary(trainpred)

below_min=length(subset(man7, TOT_DEBT>sm[6])[,1])/length(man7[,27])
over_max=length(subset(man7, TOT_DEBT<sm[1])[,1])/length(man7[,27])



##################    D_DAMBO TRAIN ##################
#TOT_SOBI+TOT_ASSET+RETIRE_NEED+TOT_ELS_ETE+ASS_FIN+TOT_YEA+TOT_FUND+M_TOT_SAVING+M_JEOK+TOT_JEOK+CHUNG_Y+M_SAVING_INSUR+FOR_RETIRE+M_CHUNG+TOT_CHUNG+TOT_DEBT

train_data99<- man7.mat[,c(1:38,53,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]


numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

model <- xgboost(data = dtrain1, 
                 subsample = 1,     
                 nround = 200, 
                 eval_metric = "rmse",
                 eta = 0.025,
                 max_depth = 4,
                 gamma = 1,
                 colsample_bytree=1,
                 min_child_weight = 1,
                 seed=101,
                 objective = "reg:linear")

trainpred <- predict(model, dtrain2)
postResample(trainpred,train_labels2)
summary(trainpred)
summary(man7$D_DAMBO)
length(subset(man7, round(D_DAMBO)==0)[,1])/length(man7[,1])
length(subset(man7, round(D_DAMBO)>39293)[,1])/length(man7[,1])
length(subset(trainpred, trainpred<0))/length(trainpred)
length(subset(trainpred, trainpred<0))
length(subset(trainpred, trainpred<300))/length(trainpred)
abline(h=300,col=5)


plot(man7$D_DAMBO,col=2,main="D_DAMBO",ylim=c(-7400,60000))
par(new=T)
plot(trainpred,ylim=c(-7400,60000))


##################    D_JUTEAKDAMBO TRAIN ##################
#TOT_SOBI+TOT_ASSET+RETIRE_NEED+TOT_ELS_ETE+ASS_FIN+TOT_YEA+TOT_FUND+M_TOT_SAVING+M_JEOK+TOT_JEOK+CHUNG_Y+M_SAVING_INSUR+FOR_RETIRE+M_CHUNG+TOT_CHUNG+TOT_DEBT
train_data99<- man7.mat[,c(1:38,54,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]


numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

model <- xgboost(data = dtrain1, 
                 subsample = 1,     
                 nround = 200, 
                 eval_metric = "rmse",
                 eta = 0.025,
                 max_depth = 2,
                 gamma = 1,
                 colsample_bytree=1,
                 min_child_weight = 1,
                 seed=101,
                 objective = "reg:linear")

trainpred <- predict(model, dtrain2)
postResample(trainpred,train_labels2)

summary(trainpred)
summary(man7$D_JUTEAKDAMBO)
length(subset(man7, round(D_JUTEAKDAMBO)==0)[,1])/length(man7[,1])
length(subset(man7, round(D_JUTEAKDAMBO)>26264)[,1])/length(man7[,1])
length(subset(trainpred, trainpred<0))/length(trainpred)
length(subset(trainpred, trainpred<0))
length(subset(trainpred, trainpred<350))/length(trainpred)
abline(h=350,col=5)


plot(man7$D_JUTEAKDAMBO,col=2,main="D_JUTEAKDAMBO",ylim=c(-2400,30000))
par(new=T)
plot(trainpred,ylim=c(-2400,30000))


##################    M_FUND_STOCK TRAIN ##################
train_data99<- man7.mat[,c(1:38,46,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]

numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

model <- xgboost(data = dtrain1, 
                 subsample = 1,     
                 nround = 1000, 
                 eval_metric = "rmse",
                 eta = 0.025,
                 max_depth = 7,
                 gamma = 5,
                 colsample_bytree=0.5,
                 min_child_weight = 0.3,
                 seed=101,
                 objective = "reg:linear")

trainpred <- predict(model, dtrain2)
postResample(trainpred,train_labels2)

summary(trainpred)
summary(man7$M_FUND_STOCK)
length(subset(man7, round(M_FUND_STOCK)==0)[,1])/length(man7[,1])
length(subset(man7, round(M_FUND_STOCK)>179.5)[,1])/length(man7[,1])
length(subset(trainpred, trainpred<0))/length(trainpred)
length(subset(trainpred, trainpred<0))
length(subset(trainpred, trainpred<10))/length(trainpred)
abline(h=10,col=5)


plot(man7$M_FUND_STOCK,col=2,main="M_FUND_STOCK",ylim=c(-8,500))
par(new=T)
plot(trainpred,ylim=c(-8,500))


##################    M_FUND TRAIN ##################
train_data99<- man7.mat[,c(1:38,47,46,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]

numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

model <- xgboost(data = dtrain1, 
                 subsample = 1,     
                 nround = 200, 
                 eval_metric = "rmse",
                 eta = 0.025,
                 max_depth = 4,
                 gamma = 1,
                 colsample_bytree=1,
                 min_child_weight = 1,
                 seed=101,
                 objective = "reg:linear")

trainpred <- predict(model, dtrain2)
postResample(trainpred,train_labels2)

summary(trainpred)
table(trainpred)
length(trainpred)
summary(man7$M_FUND)
length(subset(man7, round(M_FUND)==0)[,1])/length(man7[,1])
length(subset(man7, round(M_FUND)>0)[,1])/length(man7[,1])
table(subset(man7, round(M_FUND)>-0.0000000001)[,18])
length(subset(trainpred, trainpred>200))/length(trainpred)
length(subset(trainpred, trainpred<0))/length(trainpred)
length(subset(trainpred, trainpred<0))
length(subset(trainpred, trainpred<11))
length(subset(trainpred, trainpred>9))
length(subset(trainpred, trainpred<1))/length(trainpred)
abline(h=1,col=5)


plot(man7$M_FUND,col=2,main="M_FUND",ylim=c(-8,500))
par(new=T)
plot(trainpred,ylim=c(-8,500))

##################    D_SHINYONG TRAIN ##################
train_data99<- man7.mat[,c(1:38,52,47,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]

numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

model <- xgboost(data = dtrain1, 
                 subsample = 1,     
                 nround = 400, 
                 eval_metric = "rmse",
                 eta = 0.02,
                 max_depth = 4,
                 gamma = 0.5,
                 colsample_bytree=0.8,
                 min_child_weight = 0.5,
                 seed=101,
                 objective = "reg:linear")

trainpred <- predict(model, dtrain2)
postResample(trainpred,train_labels2)

summary(trainpred)
summary(man7$D_SHINYONG)
length(subset(man7, round(D_SHINYONG)==0)[,1])/length(man7[,1])
length(subset(man7, round(D_SHINYONG)>4138.197)[,1])/length(man7[,1])
length(subset(trainpred, trainpred<0))/length(trainpred)
length(subset(trainpred, trainpred<0))
#abline(h=1,col=5)


plot(man7$D_SHINYONG,col=2,main="D_SHINYONG",ylim=c(-140,10000))
par(new=T)
plot(trainpred,ylim=c(-140,10000))


#plot(man7$D_SHINYONG,col=2,main="D_SHINYONG",ylim=c(-24,200))
#par(new=T)
#plot(trainpred,ylim=c(-24,200))


##################    D_JEONSEA TRAIN ##################
train_data99<- man7.mat[,c(1:38,55,52,47,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]

numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

model <- xgboost(data = dtrain1, 
                 subsample = 1,     
                 nround = 850, 
                 early_stopping_rounds = 850,
                 eval_metric = "rmse",
                 eta = 0.01,
                 max_depth = 7,
                 gamma = 5.2,
                 colsample_bytree=0.5,
                 min_child_weight = 2,
                 seed=101,
                 objective = "reg:linear")

trainpred <- predict(model, dtrain2)
postResample(trainpred,train_labels2)

summary(trainpred)
summary(man7$D_JEONSEA)
length(subset(man7, round(D_JEONSEA)==0)[,1])/length(man7[,1])
length(subset(man7, round(D_JEONSEA)>5869)[,1])/length(man7[,1])
length(subset(trainpred, trainpred<0))/length(trainpred)
length(subset(trainpred, trainpred<0))
length(subset(trainpred, trainpred<300))/length(trainpred)
#abline(h=300,col=5)


plot(man7$D_JEONSEA,col=2,main="D_JEONSEA",ylim=c(-400,20000))
par(new=T)
plot(trainpred,ylim=c(-400,20000))



##################    M_STOCK TRAIN ##################
train_data99<- man7.mat[,c(1:38,48,46,55,52,39,40,42,43,44,45,49,50,51,56,57,58,59,60,61,62,63)] 
train_labels <- train_data99[,39]
train_data<-train_data99[,-39]

numberOfTrainingSamples <- round(length(train_labels) * .7)
set.seed(130)
samp<-sample(length(train_labels),numberOfTrainingSamples)

# training data
train_data1 <- train_data[samp,]
train_labels1 <- train_labels[samp]

# training data
train_data2 <- train_data[-samp,]
train_labels2 <- train_labels[-samp]

# put our testing & training data into two seperates Dmatrixs objects
dtrain1 <- xgb.DMatrix(data = train_data1, label= train_labels1)
dtrain2 <- xgb.DMatrix(data =train_data2, label= train_labels2)

model <- xgboost(data = dtrain1, 
                 subsample = 1,     
                 nround = 300, 
                 early_stopping_rounds = 100,
                 eval_metric = "rmse",
                 eta = 0.025,
                 max_depth = 4,
                 gamma = 1.5,
                 colsample_bytree=1,
                 min_child_weight = 0.8,
                 seed=101,
                 objective = "reg:linear")

trainpred <- predict(model, dtrain2)
postResample(trainpred,train_labels2)

summary(trainpred)
summary(man7$M_STOCK)
length(subset(man7, round(M_STOCK)==0)[,1])/length(man7[,1])
length(subset(trainpred, trainpred<0))/length(trainpred)
length(subset(trainpred, trainpred<0))
length(subset(trainpred, trainpred<1))/length(trainpred)
length(subset(trainpred, trainpred<300))/length(trainpred)
#abline(h=300,col=5)


plot(man7$M_STOCK,col=2,main="M_STOCK",ylim=c(-10,330))
par(new=T)
plot(trainpred,ylim=c(-10,330))

################ cf. xgboost bestune code##############
#custom_summary = function(data, lev = NULL, model = NULL){
#  out = rmsle(data[, "obs"], data[, "pred"])
#  names(out) = c("rmsle") 
#  out
#}
#control = trainControl(method = "cv",  # Use cross validation
#                       number = 5,     # 5-folds
#                       summaryFunction = custom_summary                      
#)
#grid = expand.grid(nrounds=c(500,600,800), 
#                   max_depth= c(4, 6,8),            
#                   eta=c(0.01,0.025),       
#                   gamma= c(1,0.8,0.5,0.1,0.05), 
#                   colsample_bytree = c(0.8,1), 
#                   min_child_weight = c(0.7,1),
#                   subsample = 0.7)
#grid_Y = expand.grid(nrounds=c(1000), 
#                     max_depth= c(16),            
#                     eta=c(0.1,0.05,0.01),       
#                     gamma= c(0.1,0.05,0.025), 
#                     colsample_bytree = c(0.7), 
#                     min_child_weight = c(5),
#                     subsample = 0.8)
#xgb_tree_model =  train(D_SHINYONG ~.,     
#                        data=train_data99,
#                        method="xgbTree",
#                        trControl=control, 
#                        tuneGrid=grid, 
#                        metric="rmse",     
#                         maximize = FALSE)   
#xgb_tree_model$results
#xgb_tree_model$bestTune






