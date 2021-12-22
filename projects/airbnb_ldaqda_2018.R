library(MASS)

##### London data Jeonchuri #####
setwd("C:/Users/USER/Desktop")
london<-read.csv("data.csv")
str(london)
#id,x,x.1 빼기
london<-london[,-c(1,2,3)]
colnames(london)
cor(a)
colnames(a)
a2<-a[,c(18,14)]
cor(a2)
#data processing
london$host_is_superhost <-as.factor(london$host_is_superhost)
london$host_has_profile_pic <- as.factor(london$host_has_profile_pic)
london$host_identity_verified <- as.factor(london$host_identity_verified)
london$instant_bookable <- as.factor(london$instant_bookable)
london$year <- as.factor(london$year)
str(london)
sum(is.na(london))



##### Center ######
### number of review > 3
### review score > 98
summary(london$number_of_reviews)
london.more <- london[london$number_of_reviews>3,] # #of review more than 3
london.more.center<-london.more[london.more$binary==1,]
london.more.center$score<-ifelse(london.more.center$review_scores_rating>=98,1,0) #if review score>98, then good
colnames(london.more.center)
bnb.more <- london.more.center[,-c(1,5,9,10,14,15,17,19,20)] 
colnames(bnb.more)
# remove "beds", "#of reviews", "review_scores_ratings", "year", "binary"
# beds correlated with --> accomodate

### Data partition
set.seed(123)
V = 2
n =  NROW(bnb_center)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
bnb.more.train = bnb_center[ii,]
bnb.more.test  = bnb_center[-ii,]

#### center LDA ####
fit.lda = lda(score ~., data=bnb.more.train)
plot(fit.lda)

cutoff = 0.5
pred = predict(fit.lda, newdata=bnb.more.test)$posterior
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(bnb.more.test$score, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity



#AUC
pred2 = predict(fit.lda, newdata=bnb.more.test)$posterior
pred = prediction(pred2[,2], bnb.more.test$score)
perf.lda = performance(pred, "tpr","fpr")

plot(perf, col = "#ff545a", lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve_LDA/QDA") #ROC
lines(x = c(0,1), y = c(0,1), col = "gray", lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("LDA","Random"), col = c("#ff545a","gray"), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC




#### center QDA ####
#remove "cancellation policy"
fit.qda = qda(score ~ host_is_superhost+host_has_profile_pic
         +host_identity_verified+ accommodates
         + bathrooms  + bedrooms+  price + minimum_nights 
         + availability_365 + instant_bookable+ reviews_per_month, data=bnb.more.train)

cutoff = 0.5
pred = predict(fit.qda, newdata=bnb.more.test)$posterior
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(bnb.more.test$score, yhat, dnn=c("Actual", "Predicted")); 
ctable #classification table


### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


#AUC
pred2 = predict(fit.qda, newdata=bnb.more.test)$posterior
pred = prediction(pred2[,2], bnb.more.test$score)
perf.qda = performance(pred, "tpr","fpr")

plot(perf, col = "#ff545a", lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve_LDA/QDA") #ROC
lines(x = c(0,1), y = c(0,1), col = "gray", lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("QDA","Random"), col = c("#ff545a","gray"), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


##그래프 겹쳐그리기
library(ROCR)
plot(perf.lda, col="#6D75DB", lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve_LDA/QDA") 
lines(x = c(0,1), y = c(0,1), col = "gray", lty = 2, lwd = 2)

#lda/qda
plot(perf.qda,add= TRUE, col="#D8D056", lwd = 2)
legend(0.5, 0.3, legend = c("lda","qda","random"), col = c("#6D75DB","#D8D056","gray"), lty = c(1,1,2), lwd = 2)


#### LDA is better when center ####
### number of review < 3
### review score > 98
london.less <- london[london$number_of_reviews<=3,] # # of review less than 3
london.less.center<-london.less[london.less$binary==1,]
london.less.center$score<-rep(0,nrow(london.less.center))
colnames(london.less.center)
bnb.less <- london.less.center[,-c(9,14,15,19,20)] 

fit.lda2 = lda(score ~., data=bnb.more) #review 개수가 3보다 많은 것 전부를 train으로 씀.
plot(fit.lda2)


cutoff = 0.5
pred = predict(fit.lda2, newdata=bnb.less)$posterior
head(pred)
yhat = ifelse(pred[,2] > cutoff, 1, 0)
table(yhat)
london.less.center$score<-yhat




colnames(bnb.more)
num<-bnb.more[,c(6,7,8,10,11,12,15)]
cor(num)
#bnb.more2<-bnb.more[,-10]#cor high remove
##############CV of center
# Computing the CV error


V = 5 #V-fold CV
miss.err.train = 0
miss.err.test = 0
cutoff = 0.5

set.seed(123)
id = sample(1:V, nrow(bnb.more), replace = T)

for(i in 1:V) {
  
  print(i)
  
  bnb.train = bnb.more[id != i,] 
  bnb.test = bnb.more[id == i,] 
  
  #fit = lda(score ~., data=bnb.train)
  fit = qda(score ~., data=bnb.train)
  
  pred = predict(fit, newdata=bnb.train)$posterior
  yhat = ifelse(pred[,2] > cutoff, 1, 0)
  miss.err.train = miss.err.train + mean(bnb.train$score != yhat) 
  
  pred = predict(fit, newdata=bnb.test)$posterior
  yhat = ifelse(pred[,2] > cutoff, 1, 0)
  miss.err.test = miss.err.test + mean(bnb.test$score != yhat) 
  
}

cv.err.train = miss.err.train/ V; cv.err.train # CV training error
cv.err.test = miss.err.test/ V;cv.err.test # CV test error


colnames(bnb.more)
num_center<-bnb.more[,c(6,7,8,10,11,12,15)]
cor(num_center)

########################
########################
##### Out ######
### number of review > 3
### review score > 98
summary(london$number_of_reviews)
london.more <- london[london$number_of_reviews>3,] # #of review more than 3
london.more.out<-london.more[london.more$binary==0,]
london.more.out$score<-ifelse(london.more.out$review_scores_rating>=98,1,0) #if review score>98, then good
colnames(london.more.out)
bnb.more <- london.more.out[,-c(1,5,9,10,14,15,17,19,20)]
colnames(bnb.more)

# remove "beds", "#of reviews", "review_scores_ratings", "year", "binary"
# beds correlated with --> accomodate

### Data partition
set.seed(123)
V = 2
n =  NROW(bnb.more)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
bnb.more.train = bnb.more[ii,]
bnb.more.test  = bnb.more[-ii,]

#### Out LDA ####
fit.lda = lda(score ~., data=bnb.more.train)
plot(fit.lda)

cutoff = 0.5
pred = predict(fit.lda, newdata=bnb.more.test)$posterior
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(bnb.more.test$score, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity

colnames(bnb.more)


library(ROCR)

pred2 = predict(fit.lda, newdata=bnb.more.test)$posterior
pred = prediction(pred2[,2], bnb.more.test$score)
perf = performance(pred, "tpr","fpr")

plot(perf, col = "#ff545a", lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve_outer (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = "gray", lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("LDA/QDA","Random"), col = c("#ff545a","gray"), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


#### Out QDA ####
#remove "cancellation policy"
fit.qda = qda(score ~ host_is_superhost +host_identity_verified+accommodates+      
                bathrooms+ bedrooms+price+minimum_nights+availability_365+instant_bookable
              +reviews_per_month
              , data=bnb.more.train)
 host_has_profile_pic
cutoff = 0.5
pred = predict(fit.qda, newdata=bnb.more.test)$posterior
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(bnb.more.test$score, yhat, dnn=c("Actual", "Predicted")); 
ctable #classification table


### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


#### LDA is better when Out ####
### number of review < 3
### review score > 98
london.less <- london[london$number_of_reviews<=3,] # # of review less than 3
london.less.out<-london.less[london.less$binary==0,]
london.less.out$score<-rep(0,nrow(london.less.out))
colnames(london.less.out)
bnb.less <- london.less.out[,-c(1,5,9,10,14,15,17,19,20)]
colnames(bnb.less)
##############sibal
bnb.less<-bnb.less[,-c(1,2,3)]


fit.lda2 = lda(score ~., data=bnb.more) #review 개수가 3보다 많은 것 전부를 train으로 씀.
plot(fit.lda2)

cutoff = 0.5
pred = predict(fit.lda2, newdata=bnb.less)$posterior
head(pred)
yhat = ifelse(pred[,2] > cutoff, 1, 0)
table(yhat)



