##### reg : data preprocessing #####
setwd("C:/Users/USER/Desktop")
london<-read.csv("data.csv")

str(london)
#id,x,x.1 빼기
london<-london[,-c(1,2,3)]
str(london)

#data processing
london$host_is_superhost <-as.factor(london$host_is_superhost)
london$host_has_profile_pic <- as.factor(london$host_has_profile_pic)
london$host_identity_verified <- as.factor(london$host_identity_verified)
london$instant_bookable <- as.factor(london$instant_bookable)
london$year <- as.factor(london$year)
london$binary <- as.factor(london$binary)

str(london)
sum(is.na(london))

summary(london$review_scores_rating)
score<-london[london$review_scores_rating<=90,]

#data 
barplot(table(london$binary), col = "#ff545a", xlab = "region", ylab = "Frequency")
str(london)
#y=binary
#fit
colnames(london)
bnb_num<-london[,c(6,7,8,9,11,12,13,14,15,18)]#accomodates&beds correlation > 0.8
cor(bnb_num)

bnb_num<-london[,c(7,8,9,11,12,13,14,15,18,20)]#accomodates&beds correlation > 0.8
bnb <- london[,-c(6,19)] #year뺌 별로 안유의한데 자리만 차지하길래

str(london)
colnames(london)


##### reg : model select w/o validation (미사용) #####
#-year
fit <- glm(binary~., data=bnb, family = binomial(link = "logit"))
summary(fit)
#stepwise
fit2 = step(fit, direction = "both")
fit2$anova
summary(fit2)

fit2.pred = predict(fit2, newdata = bnb, type = "response") 
cutoff = 0.5 #0.5, 0.3
fit2.yhat = ifelse(fit2.pred <= cutoff, 0, 1)

ctable <- table(bnb$binary, fit2.yhat,  dnn = c("Actual", "Predicted"))  
ctable

miss.err = 1-sum(diag(ctable))/sum(ctable) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  

diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity

library(ROCR)

fit.pred = predict(fit, newdata = bnb, type = "response") 
pred = prediction(fit.pred, bnb$binary)

perf = performance(pred, "tpr","fpr")
plot(perf, col = "#ff545a", lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
lines(x = c(0,1), y = c(0,1), col = "gray", lty = 2, lwd = 2)
legend(0.6, 0.3, legend = c("Regression","Random"), col = c("#ff545a","gray"), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


##### reg : model select validation 7:3 #####
set.seed(123)
V = 2
n =  NROW(bnb)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
bnb.train = bnb[ii,]
bnb.test  = bnb[-ii,]


### Model fit
fit_bnb = glm(binary ~., data = bnb.train, family = binomial(link = "logit"))
summary(fit_bnb)


### Model selection
fit2_bnb = step(fit_bnb, direction = "both")
fit2_bnb$anova
summary(fit2_bnb)


### Prediction
fit2_bnb.pred = predict(fit2_bnb, newdata = bnb.test, type = "response") 
cutoff = 0.5
fit2_bnb.yhat = ifelse(fit2_bnb.pred <= cutoff, 0, 1)

ctable = table(bnb.test$binary, fit2_bnb.yhat,  dnn = c("Actual", "Predicted")) 
ctable


### Errors
miss.err = 1-sum(diag(ctable))/sum(ctable) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  

diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity

### ROC and AUC
library(ROCR)
par(mfrow = c(2,2))

fit_bnb.pred = predict(fit_bnb, newdata = bnb.train, type = "response") 
pred = prediction(fit_bnb.pred, bnb.train$binary)

perf = performance(pred, "tpr","fpr")
plot(perf, col = "#ff545a", lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = "gray", lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Regression","Random"), col = c("#ff545a","gray"), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


fit_bnb.pred = predict(fit_bnb, newdata = bnb.test, type = "response") 
pred = prediction(fit_bnb.pred, bnb.test$binary)

perf = performance(pred, "tpr","fpr")
plot(perf, col = "#ff545a", lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = "gray", lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Regression","Random"), col = c("#ff545a","gray"), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC
##### reg : CV #####

cutoff = 0.5

V = 5 #V-fold CV 5,10
miss.err.train = 0
miss.err.test = 0

set.seed(1234)
id = sample(1:V, nrow(bnb), replace = T)

for(i in 1:V) {
  
  print(i)
  
  bnb.train = bnb[id != i,] 
  bnb.test = bnb[id == i,] 
  
  fit = glm(binary ~ ., data = bnb.train, family = binomial(link = "logit"))
  fit2 = step(fit, direction="both", trace=FALSE) #Stepwise variable selection
  
  pred.train = predict(fit2, newdata=bnb.train, type="response")
  yhat.train = ifelse(pred.train <= cutoff, 0, 1)
  miss.err.train = miss.err.train + mean(bnb.train$binary != yhat.train) 
  
  pred.test = predict(fit2, newdata=bnb.test, type="response")
  yhat.test = ifelse(pred.test<= cutoff, 0, 1)
  miss.err.test = miss.err.test + mean(bnb.test$binary != yhat.test)
  
}

cv.err.train = miss.err.train/ V # CV training error
cv.err.train

cv.err.test = miss.err.test/ V # CV test error
cv.err.test



ctable = table(bnb.test$binary,yhat.test,  dnn = c("Actual", "Predicted")) 
ctable

head(bnb.test$binary)
length(pred.test)
head(yhat.test)

diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity
