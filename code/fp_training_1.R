# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
set.seed(46685326,kind="Mersenne-Twister")


########################
library(DMwR)
library(dplyr)

####
casualties <- read.csv("uk_accidents_processed.csv", header=T)
# casualties <- read.csv("10kuk_accidents_processed.csv", header=T)
# casualties <- read.csv("smote_uk_accidents_processed.csv", header=T)

head(casualties,2)
colnames(casualties)

table(casualties$Accident.Severity)
prop.table(table(casualties$Accident.Severity))

#######################
### swap columns
col_swaps = c('AREFNO','ageband','Accident.Date','Time',
              'No..of.Vehicles.in.Acc.','No..of.Casualties',
              'Casualty.Age','Casualty.Sex','Casualty.Class',
              'Light.Conditions.Banded','Road.Surface','Weather','Junction.Detail',
              'Speed.Limit','Road.Type','Highway',
              'hour','day','month','Mode.of.Travel',
              'Accident.Severity') 
casualties<-subset(casualties, select=col_swaps)
head(casualties,2)

##################################################
##### data preprocessing #############
# original class
sapply(casualties, class)

#https://datascience.stackexchange.com/questions/17759/encoding-features-like-month-and-hour-as-categorial-or-numeric#:~:text=One%20technique%20that%20may%20be,%2C%20Apr%2DJun%2C%20etc.
cols = c('Casualty.Sex','Casualty.Class',
         'Light.Conditions.Banded','Road.Surface','Weather','Junction.Detail',
         'Road.Type','Highway',
         'day','month','Mode.of.Travel',
         'Accident.Severity') 

# apply factor class to all columns
casualties[cols] <- lapply(casualties[cols], factor)  ## as.factor() could also be used

# after apply .factor
sapply(casualties, class)

########################################
### make month and day numerical
head(casualties,2)
casualties$month=match(casualties$month,month.name)
# casualties$day <- recode(casualties$day, 
#                         "Sunday"="1",
#                         "Monday"="2",
#                         "Tuesday"="3",
#                         "Wednesday"="4",
#                         "Thursday"="5",
#                         "Friday"="6",
#                         "Saturday"="7")
table(casualties$month)
table(casualties$day)
#######################################
############## split data #####################
df = casualties
colnames(df)
# shuffle data
rows <- sample(nrow(df))
df <- df[rows, ]

## choose only feature columns
col_features = c("Casualty.Age", "Casualty.Sex",           
                 "Casualty.Class","Light.Conditions.Banded","Road.Surface","Weather",                
                 "Junction.Detail","Speed.Limit","Road.Type", "Highway",                
                 "hour","day","month" ,"Mode.of.Travel","Accident.Severity")
df = df[,col_features]
head(df,2)
###########################
### balanced data using SMOTE ####
df <- SMOTE(Accident.Severity ~ .,df)#, perc.over = 600,perc.under=100)
# shuffle data
rows <- sample(nrow(df))
df <- df[rows, ]
table(df$Accident.Severity)
prop.table(table(df$Accident.Severity))
########################
library(caret)
# https://stackoverflow.com/questions/40709722/how-does-createdatapartition-function-from-caret-package-split-data
splitIndex <- createDataPartition(df$Accident.Severity, p = .9,
                                  list = FALSE,
                                  times = 1)
set1 <- df[ splitIndex,]
set2 <- df[-splitIndex,]
# head(set1)
# head(set2)

table(set1$Accident.Severity)
prop.table(table(set1$Accident.Severity))
table(set2$Accident.Severity)
prop.table(table(set2$Accident.Severity))
#################
library('MLmetrics')
library(caTools)
library(xgboost)
library(ROCR)
library(PRROC)
########
set1_ = set1
set2_ = set2
set1_$Accident.Severity <- factor(set1_$Accident.Severity,
                                  levels= c("Serious", "Slight"),
                                  labels= c(1, 0))
set2_$Accident.Severity <- factor(set2_$Accident.Severity,
                                  levels= c("Serious", "Slight"),
                                  labels= c(1, 0))
class(set1_$Accident.Severity)
###############
#Applying logistic Regressor of Classification
classifier = glm(formula = Accident.Severity ~., 
                 family = binomial,data =set1)
y_pred_train = predict(classifier,type = 'response',newdata = set1)
# y_pred_train = predict(classifier,newdata = set1)
y_pred_test = predict(classifier,type = 'response',newdata = set2)

y_pred_train_ = ifelse(y_pred_train < 0.5,'Serious','Slight')
y_pred_test_ = ifelse(y_pred_test < 0.5,'Serious','Slight')
mean(y_pred_train_ == set1$Accident.Severity)
mean(y_pred_test_ == set2$Accident.Severity)

PRAUC(y_pred = y_pred_train, y_true = set1$Accident.Severity)
PRAUC(y_pred = y_pred_test, y_true = set2$Accident.Severity)

confusionMatrix(factor(y_pred_train_), set1$Accident.Severity,mode='everything')
confusionMatrix(factor(y_pred_test_), set2$Accident.Severity,mode='everything')

table( set1$Accident.Severity)
table( y_pred_train_)
table( set2$Accident.Severity)
table( y_pred_test_)

##
#Create 10 equally size folds
folds <- cut(seq(1,nrow(set1)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  print(i)
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  
  testData <- set1[testIndexes, ]
  trainData <- set1[-testIndexes, ]
  #Use test and train data partitions however you desire...
  classifier = glm(formula = Accident.Severity ~., 
                   family = binomial,data =trainData)
  y_pred_train = predict(classifier,type = 'response',newdata = trainData)
  y_pred_test = predict(classifier,type = 'response',newdata = testData)
  
  y_pred_train_ = ifelse(y_pred_train < 0.5,'Serious','Slight')
  y_pred_test_ = ifelse(y_pred_test < 0.5,'Serious','Slight')
  acc_train=mean(y_pred_train_ == trainData$Accident.Severity)
  acc_test=mean(y_pred_test_ == testData$Accident.Severity)
  print(acc_train)
  print(acc_test)
}

####
# Run the ROCR functions for AUC calculation
set2_y = set2$Accident.Severity
ROC_perf <- performance(prediction(y_pred_test,set2_y),"tpr","fpr")
# ROC_sens <- performance(prediction(y_pred_test,set2_y),"sens","spec")
# ROC_err <- performance(prediction(y_pred_test, labels=set2_y),"err")
ROC_auc <- performance(prediction(y_pred_test,set2_y),"auc")
# AUC value
auc <- ROC_auc@y.values[[1]]
cat("AUC:",auc)

#AUC of Precision and recall
prc_dataFrame <- data.frame(y_pred_test, set2$Accident.Severity)
head(prc_dataFrame,4)
prc <- pr.curve(prc_dataFrame[prc_dataFrame$set2.Accident.Severity == 'Slight',]$y_pred, 
                prc_dataFrame[prc_dataFrame$set2.Accident.Severity == 'Serious',]$y_pred,
                curve = TRUE)

prauc = prc$auc.integral
cat("PRAUC:", prauc)

pred = ifelse(y_pred_test > 0.5,1,0)
cm = table(set2[,'Accident.Severity'],pred)
cat("confusion matrix", cm)

par(mfrow=c(1,2))
plot(ROC_perf)
plot(prc)

###############################
#############Xgboost
set1_ = set1
set2_ = set2
set1_$Accident.Severity <- factor(set1_$Accident.Severity,
                                  levels= c("Serious", "Slight"),
                                  labels= c(1, 0))
set2_$Accident.Severity <- factor(set2_$Accident.Severity,
                                  levels= c("Serious", "Slight"),
                                  labels= c(1, 0))

sapply(set1_, class)
cols = c('Casualty.Sex','Casualty.Class',
         'Light.Conditions.Banded','Road.Surface','Weather','Junction.Detail',
         'Road.Type','Highway',
         'day','month','Mode.of.Travel') 
set1_[cols] <- lapply(set1_[cols], as.numeric) 
set2_[cols] <- lapply(set2_[cols], as.numeric) 
set1_mt <- xgb.DMatrix(data = as.matrix(set1_[,-15]), label= as.matrix(set1_[,15]))
set2_mt <- xgb.DMatrix(data = as.matrix(set2_[,-15]), label= as.matrix(set2_[,15]))
###
classifier_xg = xgboost(data = set1_mt,nrounds = 10,
                        objective = "binary:logistic")
# pred_xg = predict(classifier_xg,type = 'response', newdata = set2_mt)
y_pred_train = predict(classifier_xg,type = 'response',newdata = set1_mt)
y_pred_test = predict(classifier_xg,type = 'response',newdata = set2_mt)

y_pred_train_ = ifelse(y_pred_train > 0.5,'Serious','Slight')
y_pred_test_ = ifelse(y_pred_test > 0.5,'Serious','Slight')
mean(y_pred_train_ == set1$Accident.Severity)
mean(y_pred_test_ == set2$Accident.Severity)

PRAUC(y_pred = y_pred_train, y_true = set1_$Accident.Severity)
PRAUC(y_pred = y_pred_test, y_true = set2_$Accident.Severity)

confusionMatrix(factor(y_pred_train_), set1$Accident.Severity,mode='everything')
confusionMatrix(factor(y_pred_test_), set2$Accident.Severity,mode='everything')

# PRAUC(y_pred = pred_xg, y_true = set2_$Accident.Severity)

# Run the ROCR functions for AUC calculation
set2_y = set2_$Accident.Severity
ROC_perf <- performance(prediction(y_pred_test,set2_y),"tpr","fpr")
# ROC_sens <- performance(prediction(pred_xg,set2_y),"sens","spec")
# ROC_err <- performance(prediction(pred_xg, labels=set2_y),"err")
ROC_auc <- performance(prediction(y_pred_test,set2_y),"auc")
# AUC value
auc <- ROC_auc@y.values[[1]]
cat("AUC:",auc)

#AUC of Precision and recall
prc_dataFrame <- data.frame(y_pred_test, set2_$Accident.Severity)
prc <- pr.curve(prc_dataFrame[prc_dataFrame$set2_.Accident.Severity == 1,]$y_pred_test, 
                prc_dataFrame[prc_dataFrame$set2_.Accident.Severity == 0,]$y_pred_test,
                curve = TRUE)
prauc = prc$auc.integral
cat("PRAUC:", prauc)

pred = ifelse(y_pred_test > 0.5,1,0)
cm = table(set2[,'Accident.Severity'],pred)
cat("confusion matrix", cm)

par(mfrow=c(1,2))
plot(ROC_perf)
plot(prc)

#################################
########### RF

library(randomForest)
wh.rf <- randomForest(data=set1, Accident.Severity~., 
                      importance=TRUE, keep.forest=TRUE)
wh.rf           

round(importance(wh.rf),3) # Print out importance measures
x11(h=7,w=15)
varImpPlot(wh.rf) # Plot of importance measures; more interesting with more variables
# 

# Predict results of classification. 
pred.rf.train <- predict(wh.rf, newdata=set1, type="response")
pred.rf.test <- predict(wh.rf, newdata=set2, type="response")
##"vote" gives proportions of trees voting for each class
pred.rf.vtrain <- predict(wh.rf, newdata=set1, type="vote")
pred.rf.vtest <- predict(wh.rf, newdata=set2, type="vote")
# head(cbind(pred.rf.test,pred.rf.vtest))

(misclass.train.rf <- mean(ifelse(pred.rf.train == set1$Accident.Severity, yes=0, no=1)))
(misclass.test.rf <- mean(ifelse(pred.rf.test == set2$Accident.Severity, yes=0, no=1)))


prauc_train = pmax(as.data.frame(pred.rf.vtrain)$Slight, as.data.frame(pred.rf.vtrain)$Serious)
prauc_test = pmax(as.data.frame(pred.rf.vtest)$Slight, as.data.frame(pred.rf.vtest)$Serious)

# Precision-Recall curve
PRAUC(y_pred = prauc_train, y_true = set1$Accident.Severity)
PRAUC(y_pred = prauc_test , y_true = set2$Accident.Severity)


confusionMatrix(pred.rf.train, set1$Accident.Severity)
confusionMatrix(pred.rf.test, set2$Accident.Severity)

##############################
reps=5
varz = c(2,4)#,6,10)#,18)
nodez = c(1,3)#,5,7)#,10)

NS = length(nodez)
M = length(varz)
rf.oob = matrix(NA, nrow=M*NS, ncol=reps)
rf.oob
for(r in 1:reps){
  counter=1
  for(m in varz){
    for(ns in nodez){
      wh.rfm <- randomForest(data=set1, Accident.Severity~., 
                             mtry=m, nodesize=ns, ntree=1000)
      rf.oob[counter,r] = mean(predict(wh.rfm, type="response") != set1$Accident.Severity)
      counter=counter+1
    }
  }
}

parms = expand.grid(nodez,varz)
row.names(rf.oob) = paste(parms[,2], parms[,1], sep="|")

mean.oob = apply(rf.oob, 1, mean)
mean.oob[order(mean.oob)]

min.oob = apply(rf.oob, 2, min)

# x11(h=7,w=10,pointsize=8)
boxplot(rf.oob, use.cols=FALSE, las=2)

# x11(h=7,w=10,pointsize=8)
boxplot(t(rf.oob)/min.oob, use.cols=TRUE, las=2, 
        main="RF Tuning Variables and Node Sizes")

