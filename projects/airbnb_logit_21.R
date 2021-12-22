##### logit : data preprocessing #####
summary(london$experiences_offered)
barplot(table(london$experiences_offered)[-3],col="#ff545a",main="숙소유형")
table(london$experiences_offered)[3]
summary(london$cancellation_policy)
barplot(table(london$cancellation_policy)[-c(3,5,6)],col="#ff545a",main="Cancellation Policy")
barplot(table(london$cancellation_policy)[c(3,5,6)],col="#ff545a",main="Cancellation Policy")
barplot(table(london$room_type),col="#ff545a",main="Room type")

setwd("C:/Users/USER/Desktop")
london_raw<-read.csv("data.csv")

#id,x,x.1 빼기
london<-london_raw[,-c(1,2,3)]
str(london)

#data processing
london$host_is_superhost <-as.factor(london$host_is_superhost)
london$host_has_profile_pic <- as.factor(london$host_has_profile_pic)
london$host_identity_verified <- as.factor(london$host_identity_verified)
london$instant_bookable <- as.factor(london$instant_bookable)
london$year <- as.factor(london$year)


str(london)
#sum(is.na(london))
summary(london$review_scores_rating)
london_pred<-london[london$number_of_reviews<=3,]
london<-london[london$number_of_reviews>3,]

#####RATING score
#review개수 낮은거 제외
summary(london$review_scores_rating)
london$score<-ifelse(london$review_scores_rating>=98,1,0)
london$score<-as.factor(london$score)
colnames(london)
colnames(london_pred)
london_pred$score<-rep(0,nrow(london_pred))
out<-london_pred[london_pred$binary=="0",]
center<-london_pred[london_pred$binary=="1",]
#remove binary
out<-out[,-20]
center<-center[,-20]
#data 
barplot(table(london$score), col = "#ff545a", xlab = "region", ylab = "Frequency",main="SCORE")
str(london)

#y=score
#fit
colnames(london)
bnb_num<-london[,c(6,7,8,9,11,12,13,14,15,18)]#accomodates&beds correlation > 0.8
cor(bnb_num)


bnb <- london[,-c(9,14,15,19)] #-year, number of reviews, beds,review rating (acc가 더 price랑 유의)
center<-center[,-c(9,14,15,19)]
out<-out[,-c(9,14,15,19)]
colnames(bnb)
colnames(center)
colnames(out)

#bnb를 center,out으로 나누기
bnb_center<-bnb[bnb$binary=="1",]
bnb_out <- bnb[bnb$binary=="0",]
colnames(bnb_center)
#remove binary
bnb_center<-bnb_center[,-16]
bnb_out<-bnb_out[,-16]


##### logit : bnb_center real start #####
##### DATA partition #####
colnames(bnb_center)
bnb_center2<-bnb_center
bnb_center<-bnb_cetner[,-14] #remove "cancellation_policy"

set.seed(123)
V = 2
n =  NROW(bnb_center)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
bnb.train = bnb_center[ii,]
bnb.test  = bnb_center[-ii,]


##### Model fit #####
fit_bnb = glm(score ~., data = bnb.train, family = binomial(link = "logit"))
summary(fit_bnb)


### Model selection
#extra<- bnb_center[bnb_center$host_is_superhost=="0" & bnb_center$score == "1",]
#summary(bnb_center$host_is_superhost)

fit2_bnb = step(fit_bnb, direction = "both")
fit2_bnb$anova
summary(fit2_bnb)


###Evaluation by validation set approach
fit2_bnb.pred = predict(fit2_bnb, newdata = bnb.test, type = "response") 
cutoff = 0.3
fit2_bnb.yhat = ifelse(fit2_bnb.pred <= cutoff, 0, 1)

ctable = table(bnb.test$score, fit2_bnb.yhat,  dnn = c("Actual", "Predicted")) 
ctable

### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  

diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


##### ROC and AUC #####
library(ROCR)

fit_bnb.pred = predict(fit_bnb, newdata = bnb.test, type = "response") 
pred = prediction(fit_bnb.pred, bnb.test$score)

perf = performance(pred, "tpr","fpr")
plot(perf, col = "#ff545a", lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = "gray", lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Regression","Random"), col = c("#ff545a","gray"), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC

##### test error #####
# Computing the CV error

cutoff = 0.3

V = 5 #V-fold CV
miss.err.train = 0
miss.err.test = 0

set.seed(1234)
id = sample(1:V, nrow(bnb_center), replace = T)

for(i in 1:V) {
  
  print(i)
  
  bnb.train = bnb_center[id != i,]
  bnb.test = bnb_center[id == i,] 
  
  
  fitt = glm(score ~ ., data = bnb.train, family = binomial(link = "logit"))
  fitt2 = step(fitt, direction="both", trace=FALSE) #Stepwise variable selection
  
  pred.train = predict(fitt2, newdata=bnb.train, type="response")
  yhat.train = ifelse(pred.train <= cutoff, 0, 1)
  miss.err.train = miss.err.train + mean(bnb.train$score != yhat.train) 
  
  pred.test = predict(fitt2, newdata=bnb.test, type="response")
  yhat.test = ifelse(pred.test<= cutoff, 0, 1)
  miss.err.test = miss.err.test + mean(bnb.test$score != yhat.test)
  
}

cv.err.train = miss.err.train/ V # CV training error
cv.err.train

cv.err.test = miss.err.test/ V # CV test error
cv.err.test


###prediction
#위에 한 stepwise
#fit2_bnb = step(fit_bnb, direction = "both")
#fit2_bnb$anova
#summary(fit2_bnb)

#
#pred1<- glm(score ~ experiences_offered + host_is_superhost + 
#  host_has_profile_pic + host_identity_verified + room_type + 
#  accommodates + bedrooms + bed_type + price + availability_365 + 
#  instant_bookable + reviews_per_month, family = binomial(link = "logit"), data =center)
colnames(center)
center<-center[,-14]

glm.prob <- predict(fitt2, newdata=center,type="response")
head(glm.prob)
glm.pred<-rep("0",nrow(center))
glm.pred[glm.prob>0.5]="1"
table(glm.pred)


