# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
set.seed(46685326,kind="Mersenne-Twister")

########################
library(DMwR)
library(dplyr)
library(caret)
library('MLmetrics')
library(caTools)
library(xgboost)
library(ROCR)
library(PRROC)
library(randomForest)
###################
# casualties <- read.csv("uk_accidents_processed.csv", header=T)
# casualties <- read.csv("10kuk_accidents_processed.csv", header=T)
# casualties <- read.csv("smote_uk_accidents_processed.csv", header=T)
casualties <- read.csv("train.csv", header=T)

head(casualties,2)
# colnames(casualties)
table(casualties$Accident.Severity)
prop.table(table(casualties$Accident.Severity))

# 
# rows <- sample(nrow(casualties))
# casualties <- casualties[rows, ]
# splitIndex <- createDataPartition(casualties$Accident.Severity, p = .9,list = FALSE,times = 1)
# set1 <- casualties[ splitIndex,]
# set2 <- casualties[-splitIndex,]
# table(set1$Accident.Severity)
# prop.table(table(set1$Accident.Severity))
# table(set2$Accident.Severity)
# prop.table(table(set2$Accident.Severity))
# write.table(set1, sep=",",dec = " ",file='train.csv', row.names = FALSE)
# write.table(set2, sep=",",dec = " ",file='test.csv', row.names = FALSE)

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
cols = c('Casualty.Sex','Casualty.Class',
         'Light.Conditions.Banded','Road.Surface','Weather','Junction.Detail',
         'Road.Type','Highway',
         'day','month','Mode.of.Travel',
         'Accident.Severity') 
# apply factor class to all columns
casualties[cols] <- lapply(casualties[cols], factor)  ## as.factor() could also be used
########################################
### make month and day numerical
casualties$month=match(casualties$month,month.name)
# casualties$day <- recode(casualties$day,
#                          "Sunday" = "1",
#                          "Monday" = "2",
#                          "Tuesday" = "3",
#                          "Wednesday" = "4",
#                          "Thursday" = "5",
#                          "Friday" = "6",
#                          "Saturday" = "7"   )
# table(casualties$month)
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
###########################
### balanced data using SMOTE ####
df <- SMOTE(Accident.Severity ~ .,df)#, perc.over = 600,perc.under=100)
# shuffle data
rows <- sample(nrow(df))
df <- df[rows, ]
table(df$Accident.Severity)
prop.table(table(df$Accident.Severity))
########################

set1 = df
# splitIndex <- createDataPartition(df$Accident.Severity, p = .8,list = FALSE,times = 1)
# set1 <- df[ splitIndex,]
# set2 <- df[-splitIndex,]
# 
# table(set1$Accident.Severity)
# prop.table(table(set1$Accident.Severity))
# table(set2$Accident.Severity)
# prop.table(table(set2$Accident.Severity))
#################
########
set1_ = set1
# set2_ = set2
set1_$Accident.Severity <- factor(set1_$Accident.Severity,
                                  levels= c("Serious", "Slight"),
                                  labels= c(1, 0))
# set2_$Accident.Severity <- factor(set2_$Accident.Severity,
                                  # levels= c("Serious", "Slight"),
                                  # labels= c(1, 0))
## matrix for xgboost
cols = c('Casualty.Sex','Casualty.Class',
         'Light.Conditions.Banded','Road.Surface','Weather','Junction.Detail',
         'Road.Type','Highway',
         'day','month','Mode.of.Travel') 
set1_[cols] <- lapply(set1_[cols], as.numeric) 
# set2_[cols] <- lapply(set2_[cols], as.numeric)
#######################################
##########
ACC.cv = matrix(NA, nrow=10, ncol=3)
colnames(ACC.cv) = c('LogisticReg','XGboost','RF')

PRUC.cv = matrix(NA, nrow=10, ncol=3)
colnames(PRUC.cv) = c('LogisticReg','XGboost','RF')

AUC.cv = matrix(NA, nrow=10, ncol=3)
colnames(AUC.cv) = c('LogisticReg','XGboost','RF')
####
#Create 10 equally size folds
folds <- cut(seq(1,nrow(set1)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  print(i)
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  
  testData <- set1[testIndexes, ]
  trainData <- set1[-testIndexes, ]
  # DMtrix
  set2_mt <- xgb.DMatrix(data = as.matrix(set1_[testIndexes,-15]), label= as.matrix(set1_[testIndexes,15]))
  set1_mt <- xgb.DMatrix(data = as.matrix(set1_[-testIndexes,-15]), label= as.matrix(set1_[-testIndexes,15]))
  #
  testData_ <- set1_[testIndexes, ]
  trainData_ <- set1_[-testIndexes, ]
  ##################################
  classifier = glm(formula = Accident.Severity ~., 
                   family = binomial,data =trainData)
  y_pred_test = predict(classifier,type = 'response',newdata = testData)
  y_pred_test_ = ifelse(y_pred_test < 0.5,'Serious','Slight')
  acc_test=mean(y_pred_test_ == testData$Accident.Severity)
  #
  testData_y = testData$Accident.Severity
  ROC_auc <- performance(prediction(y_pred_test,testData_y),"auc")
  
  ACC.cv[i,1] = acc_test
  PRUC.cv[i,1]=PRAUC(y_pred = y_pred_test, y_true = testData$Accident.Severity)
  AUC.cv[i,1] <- ROC_auc@y.values[[1]]
  
  ####################
  classifier_xg = xgboost(data = set1_mt,nrounds = 70,
                          objective = "binary:logistic")
  y_pred_test = predict(classifier_xg,type = 'response',newdata = set2_mt)
  y_pred_test_ = ifelse(y_pred_test > 0.5,'Serious','Slight')
  #
  testData_y =  testData_$Accident.Severity
  ROC_auc <- performance(prediction(y_pred_test,testData_y),"auc")

  ACC.cv[i,2]=mean(y_pred_test_ == testData$Accident.Severity)
  PRUC.cv[i,2]=PRAUC(y_pred = y_pred_test, y_true = testData_$Accident.Severity)
  AUC.cv[i,2] <- ROC_auc@y.values[[1]]
  
  ###########################
  wh.rf <- randomForest(data=trainData, Accident.Severity~., importance=TRUE, keep.forest=TRUE)

  # Predict results of classification. 
  pred.rf.test <- predict(wh.rf, newdata=testData, type="response")
  pred.rf.vtest <- predict(wh.rf, newdata=testData, type="vote")
  (misclass.test.rf <- mean(ifelse(pred.rf.test == testData$Accident.Severity, yes=0, no=1)))
  prauc_test = pmax(as.data.frame(pred.rf.vtest)$Slight, as.data.frame(pred.rf.vtest)$Serious)
  #
  testData_y = testData$Accident.Severity
  # ROC_perf <- performance(prediction(as.numeric(pred.rf.test),as.numeric(testData_y ) ),"tpr","fpr")
  ROC_auc <- performance(prediction(as.numeric(pred.rf.test),as.numeric(testData_y)),"auc")
 
  ACC.cv[i,3]=1-misclass.test.rf
  PRUC.cv[i,3]=PRAUC(y_pred = prauc_test , y_true = testData$Accident.Severity)
  AUC.cv[i,3] <- ROC_auc@y.values[[1]]
  
}
ACC.cv
PRUC.cv
AUC.cv
#
ACC.cv.mean= apply(X=ACC.cv,MARGIN=2,FUN=mean)
ACC.cv.mean
par(mar=c(7,5,5,5)+.1)
boxplot(ACC.cv, las=2, ylim=c(0.6,0.9),
        main="Accuracy \n Cross-Validation")
low.c = apply(ACC.cv, 1, min)
boxplot(ACC.cv/low.c, las=2,ylim=c(0.8,1.4),
        main="Relative Accuracy \n Cross-Validation")
#
PRUC.cv.mean= apply(X=PRUC.cv,MARGIN=2,FUN=mean)
PRUC.cv.mean
par(mar=c(7,5,5,5)+.1)
boxplot(PRUC.cv, las=2, ylim=c(0.5,1),
        main="Precision-Recall AUC \n Cross-Validation")
low.c = apply(PRUC.cv, 1, min)
boxplot(ACC.cv/low.c, las=2,ylim=c(1,1.6),
        main="Relative Precision-Recall AUC \n Cross-Validation")
#
AUC.cv.mean= apply(X=AUC.cv,MARGIN=2,FUN=mean)
AUC.cv.mean
par(mar=c(7,5,5,5)+.1)
boxplot(AUC.cv, las=2, ylim=c(0.6,1),
        main="AUC \n Cross-Validation")
low.c = apply(AUC.cv, 1, min)
boxplot(AUC.cv/low.c, las=2,ylim=c(0.9,1.4),
        main="Relative AUC \n Cross-Validation")

###################################################
######### select that best model to finetune ########
splitIndex <- createDataPartition(df$Accident.Severity, p = .8,list = FALSE,times = 1)
set1 <- df[ splitIndex,]
set2 <- df[-splitIndex,]
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

## XGBoost
# Fit the model on the training set
# set.seed(123)
set.seed(46685326,kind="Mersenne-Twister")

model <- train(
  Accident.Severity ~., data = set1, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)

# Best tuning parameter
model$bestTune
# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 108     150         3 0.4     0              0.8                1         1
model
# Make predictions on the test data
predicted.classes <- model %>% predict(set2)
# head(predicted.classes)
# Compute model prediction accuracy rate
mean(predicted.classes == set2$Accident.Severity)

varImp(model)

#############################################
######## prediction on test set ##################

params <- list(booster = "gbtree",
               max_depth = 3,
               eta = 0.4,
               subsample = 1,
               colsample_bytree = 0.8,
               min_child_weight = 1)
params
xgb_tuned <- xgb.train(params = params,
                       data = set1_mt,
                       nrounds =1000,
                       print_every_n = 10,
                       eval_metric = "merror",
                       num_class = 15,
                       early_stopping_rounds = 30,
                       watchlist = list(train= set1_mt, val= set2_mt))

y_pred <- predict(xgb_tuned, set2_mt)
mean(y_pred == set2_$Accident.Severity)

table(set2_[,'Accident.Severity'],y_pred, dnn=c("Observed","Predicted"))


#####################################################
set1_ = set1
set2_ = set2
set1_$Accident.Severity <- factor(set1_$Accident.Severity,
                                  levels= c("Serious", "Slight"),
                                  labels= c(1, 0))
set2_$Accident.Severity <- factor(set2_$Accident.Severity,
                                  levels= c("Serious", "Slight"),
                                  labels= c(1, 0))
#######################################
#Applying logistic Regressor of Classification
classifier = glm(formula = Accident.Severity ~., 
                 family = binomial,data =set1)
y_pred_train = predict(classifier,type = 'response',newdata = set1)
y_pred_test = predict(classifier,type = 'response',newdata = set2)

y_pred_train_ = ifelse(y_pred_train < 0.5,'Serious','Slight')
y_pred_test_ = ifelse(y_pred_test < 0.5,'Serious','Slight')
mean(y_pred_train_ == set1$Accident.Severity)
mean(y_pred_test_ == set2$Accident.Severity)

PRAUC(y_pred = y_pred_train, y_true = set1$Accident.Severity)
PRAUC(y_pred = y_pred_test, y_true = set2$Accident.Severity)

confusionMatrix(factor(y_pred_train_), set1$Accident.Severity)
confusionMatrix(factor(y_pred_test_), set2$Accident.Severity)

# table( set1$Accident.Severity)
# table( y_pred_train_)
# table( set2$Accident.Severity)
# table( y_pred_test_)

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

confusionMatrix(factor(y_pred_train_), set1$Accident.Severity)
confusionMatrix(factor(y_pred_test_), set2$Accident.Severity)

# PRAUC(y_pred = pred_xg, y_true = set2_$Accident.Severity)

# Run the ROCR functions for AUC calculation
set2_y = set2_$Accident.Severity
# ROC_perf <- performance(prediction(pred_xg,set2_y),"tpr","fpr")
ROC_perf <- performance(prediction(y_pred_test,set2_y),"tpr","fpr")

# ROC_sens <- performance(prediction(pred_xg,set2_y),"sens","spec")
# ROC_err <- performance(prediction(pred_xg, labels=set2_y),"err")
# ROC_auc <- performance(prediction(pred_xg,set2_y),"auc")
ROC_auc <- performance(prediction(y_pred_test,set2_y),"auc")

# AUC value
auc <- ROC_auc@y.values[[1]]
cat("AUC:",auc)

#AUC of Precision and recall
# prc_dataFrame <- data.frame(pred_xg, set2_$Accident.Severity)
prc_dataFrame <- data.frame(y_pred_test, set2_$Accident.Severity)

# prc <- pr.curve(prc_dataFrame[prc_dataFrame$set2_.Accident.Severity == 1,]$pred_xg, 
#                 prc_dataFrame[prc_dataFrame$set2_.Accident.Severity == 0,]$pred_xg,
#                 curve = TRUE)
prc <- pr.curve(prc_dataFrame[prc_dataFrame$set2_.Accident.Severity == 1,]$y_pred_test, 
                prc_dataFrame[prc_dataFrame$set2_.Accident.Severity == 0,]$y_pred_test,
                curve = TRUE)
prauc = prc$auc.integral
cat("PRAUC:", prauc)

# pred = ifelse(pred_xg > 0.5,1,0)
pred = ifelse(y_pred_test > 0.5,1,0)

cm = table(set2[,'Accident.Severity'],pred)
cat("confusion matrix", cm)

par(mfrow=c(1,2))
plot(ROC_perf)
plot(prc)

#################################
########### RF
wh.rf <- randomForest(data=set1, Accident.Severity~., 
                      importance=TRUE, keep.forest=TRUE)
wh.rf           

# Predict results of classification. 
pred.rf.train <- predict(wh.rf, newdata=set1, type="response")
pred.rf.test <- predict(wh.rf, newdata=set2, type="response")
pred.rf.vtrain <- predict(wh.rf, newdata=set1, type="vote")
pred.rf.vtest <- predict(wh.rf, newdata=set2, type="vote")
(misclass.train.rf <- mean(ifelse(pred.rf.train == set1$Accident.Severity, yes=0, no=1)))
(misclass.test.rf <- mean(ifelse(pred.rf.test == set2$Accident.Severity, yes=0, no=1)))

prauc_train = pmax(as.data.frame(pred.rf.vtrain)$Slight, as.data.frame(pred.rf.vtrain)$Serious)
prauc_test = pmax(as.data.frame(pred.rf.vtest)$Slight, as.data.frame(pred.rf.vtest)$Serious)

# Precision-Recall curve
PRAUC(y_pred = prauc_train, y_true = set1$Accident.Severity)
PRAUC(y_pred = prauc_test , y_true = set2$Accident.Severity)

confusionMatrix(pred.rf.train, set1$Accident.Severity)
confusionMatrix(pred.rf.test, set2$Accident.Severity)

set2_y = set2$Accident.Severity
ROC_perf <- performance(prediction(as.numeric(pred.rf.test),as.numeric(set2_y ) ),"tpr","fpr")
ROC_auc <- performance(prediction(as.numeric(pred.rf.test),as.numeric(set2_y)),"auc")

# AUC value
auc <- ROC_auc@y.values[[1]]
auc
##############################

############# NN #########
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}
cols = c('Casualty.Sex','Casualty.Class',
         'Light.Conditions.Banded','Road.Surface','Weather','Junction.Detail',
         'Road.Type','Highway',
         'day','month','Mode.of.Travel') 
set1_[cols] <- lapply(set1_[cols], as.numeric) 
set2_[cols] <- lapply(set2_[cols], as.numeric) 
sapply(set1_[,-15], class)

set1.rescale <- data.frame(cbind(rescale(set1_[,-15], set1_[,-15]), Y=set1_$Accident.Severity))
set2.rescale <- data.frame(cbind(rescale(set2_[,-15], set1_[,-15]), Y=set2_$Accident.Severity))

summary(set1.rescale)
summary(set2.rescale)
# str(set1.rescale)
library(nnet)
###### 1.b
mod.fit <- multinom(data=set1.rescale, formula=Y ~ ., 
                    trace=TRUE, maxit = 1500)
summary(mod.fit)

# Adding a function to perform LR Tests.  Legitimate here since I am fitting one model
library(car)
Anova(mod.fit)

# Misclassification Errors
pred.class.1 <- predict(mod.fit, newdata=set1.rescale) 
# type="class")
pred.class.2 <- predict(mod.fit, newdata=set2.rescale) 
# type="class")
(mul.misclass.train <- mean(ifelse(pred.class.1 == set1_$Accident.Severity, 
                                   yes=0, no=1)))
(mul.misclass.test <- mean(ifelse(pred.class.2 == set2_$Accident.Severity, 
                                  yes=0, no=1)))

# Estimated probabilities for test set
pred.probs.2 <- predict(mod.fit, newdata=set2.rescale, 
                        type="probs")
round(head(pred.probs.2),3)

# Test set confusion matrix
# table(set2$class, pred.class.2, dnn=c("Obs","Pred"))
table(pred.class.2,set2_$Accident.Severity, dnn=c("Predicted","Observed"))


####
# rf.fit <- train(Accident.Severity ~ ., 
#                 data = set1, 
#                 method = "rf",     # Use the "random forest" algorithm
#                 importance = TRUE, # importance=TRUE allows to inspect variable importance
#                 trControl = trainControl(method = "repeatedcv", # Use cross-validation
#                                          number = 2, # Use 10 folds for cross-validation
#                                          repeats = 1)
# )
