##### NN : data preprocessing #####
#전처리 부터
london_raw<-read.csv("C:/Users/USER/Desktop/data.csv")
str(london_raw)
london<-london_raw[,4:23] #remove X.1, X, id

#factorizing
london$host_is_superhost <-as.factor(london$host_is_superhost)
london$host_has_profile_pic <- as.factor(london$host_has_profile_pic)
london$host_identity_verified <- as.factor(london$host_identity_verified)
london$instant_bookable <- as.factor(london$instant_bookable)
london$year <- as.factor(london$year)
summary(london$review_scores_rating)
#separate number_of_reviews
london_pred<-london[london$number_of_reviews<=3,]
london.1<-london[london$number_of_reviews>3,]

#####RATING score
#review개수 낮은거 제외
summary(london.1$review_scores_rating)
london.1$score<-ifelse(london.1$review_scores_rating>=95,1,0)
london.1$score<-as.factor(london.1$score)
colnames(london.1)
colnames(london_pred)

#remove year(년도), number of reviews(3이상분류마침),review_scores_rating(score로 대체했으니까)
london.2<-subset(london.1, select=-c(year,number_of_reviews,review_scores_rating))

#remove cancellation policy=="strict"
london.2[london.2$cancellation_policy =="strict",]
london.3<-london.2[!(london.2$cancellation_policy =="strict"),]
london.3$cancellation_policy = droplevels(london.3$cancellation_policy)

#bnb를 center,out으로 나누기
bnb<-london.3
bnb_center<-bnb[bnb$binary=="1",]
bnb_out <- bnb[bnb$binary=="0",]
colnames(bnb_center)
#remove binary
bnb_center<-subset(bnb_center,select=-binary)
bnb_out<-subset(bnb_out, select=-binary)

colnames(bnb_center)

##### NN : bnb_center #####
library(neuralnet) #to fit neural networks
library(dummy) #to creat dummay variables
str(bnb_center)
summary(bnb_center)
str(bnb_center)
dim(bnb_center)
head(bnb_center)
barplot(table(bnb_center$score), col = "blue", xlab = "GOODBAD", ylab = "Frequency", main="0:BAD, 1:GOOD")

### Data Handling for Neural Networks
colnames(bnb_center)
str(bnb_center)
dvar = c(1,5,10,15) #find nominal variables
bnb2 = dummy(bnb_center[,dvar]) # transform nominal variables into dummy variables
colnames(bnb2)
bnb2 = bnb2[,-c(5,8,13,18)] # delete redundant dummy variables
bnb2 = cbind(bnb_center[,-dvar], bnb2) # combine them
for(i in 1: ncol(bnb2)) if(!is.numeric(bnb2[,i])) bnb2[,i] = as.numeric(bnb2[,i])
#change all factor variable into numeric
str(bnb2)
max1 = apply(bnb2, 2, max) 
min1 = apply(bnb2, 2, min)

gdat = scale(bnb2, center = min1, scale = max1 - min1) #Standaization
gdat = as.data.frame(gdat)

##### Computing the test error by paritioning
### Data partition
set.seed(123)
V = 2
n =  NROW(gdat)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
bnb2.train = gdat[ii,]
bnb2.test  = gdat[-ii,]


### Model fit best : (2,2,0.09)
gn = names(bnb2.train)
f = as.formula(paste("score ~", paste(gn[!gn %in% "score"], collapse = " + ")))
fit.nn = neuralnet(f, data = bnb2.train, hidden=c(2,2), linear.output=F,threshold=0.09)

#fit neural networks with 2 hidden layers consisting of 2 and 2 hidden neurons, respectively
plot(fit.nn)


### Prediction
cutoff = 0.5
p.test.nn = compute(fit.nn, bnb2.test[,-13])$net.result  #13th is the target variable
yhat.test.nn = ifelse(p.test.nn > cutoff, 1, 0)

ctable = table(bnb2.test$score, yhat.test.nn, dnn=c("Actual","Predicted"))
print(ctable) # classification table  


### Errors
miss.err = 1-sum(diag(ctable))/sum(ctable) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  

diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


### ROC and AUC
library(ROCR)
cutoff = 0.5
par(mfrow = c(1,1))

p.test.nn = compute(fit.nn, bnb2.test[,-13])$net.result 
yhat.test.nn = ifelse(p.test.nn > cutoff, 1, 0)
pred = prediction(p.test.nn, bnb2.test$score)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.4, 0.3, legend = c("Neural Networks","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC

### Computing the CV errors : test error
cutoff = 0.5
V = 5 #V-fold CV
miss.err.train = 0
miss.err.test = 0

set.seed(1234)
id = sample(1:V, nrow(bnb2), replace = T)

for(i in 1:V) {
  
  print(i)
  
  bnb22.train = gdat[id != i,] 
  bnb22.test  = gdat[id == i,] 
  
  gn = names(bnb22.train)
  f = as.formula(paste("score ~", paste(gn[!gn %in% "score"], collapse = " + ")))
  fit.nn = neuralnet(f, data = bnb22.train, hidden=c(2), linear.output=F, threshold=0.09) 
  
  p.train.nn = compute(fit.nn, bnb22.train[,-13])$net.result  
  yhat.train.nn = ifelse(p.train.nn > cutoff, 1, 0)
  miss.err.train = miss.err.train + mean(bnb22.train$score != yhat.train.nn)
  
  p.test.nn = compute(fit.nn, bnb22.test[,-13])$net.result  
  yhat.test.nn = ifelse(p.test.nn > cutoff, 1, 0)
  miss.err.test = miss.err.test + mean(bnb22.test$score != yhat.test.nn)
  
}

cv.err.train = miss.err.train/ V # CV training error
cv.err.train

cv.err.test = miss.err.test/ V # CV test error
cv.err.test


### END
