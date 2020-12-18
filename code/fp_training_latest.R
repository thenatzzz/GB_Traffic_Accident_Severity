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
casualties <- read.csv("uk_accidents_processed.csv", header=T)
# casualties <- read.csv("10kuk_accidents_processed.csv", header=T)
# casualties <- read.csv("smote_uk_accidents_processed.csv", header=T)
casualties <- read.csv("train.csv", header=T)

head(casualties,2)
# colnames(casualties)
table(casualties$Accident.Severity)
prop.table(table(casualties$Accident.Severity))
# 
# (carindex <- which(casualties$Accident.Severity == 'Slight'))
# (deleteindex <- sample(carindex, length(carindex) - 6000)) 
# casualties=casualties[-deleteindex, ]
# table(casualties$Accident.Severity)

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
# df <- SMOTE(Accident.Severity ~ .,df)#, perc.over = 600,perc.under=100)
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
  ########################## Logistic Regressor #################################
  classifier = glm(formula = Accident.Severity ~., 
                   family = binomial,data =trainData_)
  y_pred_test = predict(classifier,type = 'response',newdata = testData_)
  head(y_pred_test,5)
  head(testData,5)
  head(testData_,5)
  y_pred_test_ = ifelse(y_pred_test > 0.5,'Serious','Slight')
  acc_test=mean(y_pred_test_ == testData$Accident.Severity)
  #
  testData_y = testData$Accident.Severity
  ROC_auc <- performance(prediction(y_pred_test,testData_y),"auc")
  
  ACC.cv[i,1] = acc_test
  # PRUC.cv[i,1]=PRAUC(y_pred = y_pred_test, y_true = testData$Accident.Severity)
  AUC.cv[i,1] <- ROC_auc@y.values[[1]]
  
  y_pred_test_2 = ifelse(y_pred_test < 0.5,0,1)
  prc_dataFrame <- data.frame(y_pred_test_2, testData_[,'Accident.Severity'])
  colnames(prc_dataFrame) = c('y_pred','test_pred')
  prc <- pr.curve(scores.class1=prc_dataFrame[prc_dataFrame$test_pred == 0,]$y_pred, 
                  scores.class0=prc_dataFrame[prc_dataFrame$test_pred ==1,]$y_pred,
                  curve = TRUE)
  prauc = prc$auc.integral
  PRUC.cv[i,1]=prauc
  
  #################### XG Boost ####################################################
  classifier_xg = xgboost(data = set1_mt,nrounds = 100,
                          objective = "binary:logistic")
  y_pred_test = predict(classifier_xg,type = 'response',newdata = set2_mt)
  y_pred_test_ = ifelse(y_pred_test > 0.5,'Serious','Slight')
  #
  testData_y =  testData_$Accident.Severity
  ROC_auc <- performance(prediction(y_pred_test,testData_y),"auc")
  
  ACC.cv[i,2]=mean(y_pred_test_ == testData$Accident.Severity)
  # PRUC.cv[i,2]=PRAUC(y_pred = y_pred_test, y_true = testData_$Accident.Severity)
  AUC.cv[i,2] <- ROC_auc@y.values[[1]]
  
  y_pred_test_2 = ifelse(y_pred_test < 0.5,0,1)
  prc_dataFrame <- data.frame(y_pred_test_2, testData_[,'Accident.Severity'])
  colnames(prc_dataFrame) = c('y_pred','set1_test_pred')
  prc <- pr.curve(scores.class1=prc_dataFrame[prc_dataFrame$set1_test_pred == 0,]$y_pred, 
                  scores.class0=prc_dataFrame[prc_dataFrame$set1_test_pred == 1,]$y_pred,
                  curve = TRUE)
  prauc = prc$auc.integral
  PRUC.cv[i,2]= prauc

  ########################### Random Forest #############################
  wh.rf <- randomForest(data=trainData_, Accident.Severity~., importance=TRUE, keep.forest=TRUE)
  
  # Predict results of classification. 
  pred.rf.test <- predict(wh.rf, newdata=testData_, type="response")
  pred.rf.vtest <- predict(wh.rf, newdata=testData_, type="vote")
  (misclass.test.rf <- mean(ifelse(pred.rf.test == testData_$Accident.Severity, yes=0, no=1)))
  # prauc_test = pmax(as.data.frame(pred.rf.vtest)$Slight, as.data.frame(pred.rf.vtest)$Serious)
  #
  testData_y = testData$Accident.Severity
  # ROC_perf <- performance(prediction(as.numeric(pred.rf.test),as.numeric(testData_y ) ),"tpr","fpr")
  ROC_auc <- performance(prediction(as.numeric(pred.rf.test),as.numeric(testData_y)),"auc")
  
  ACC.cv[i,3]=1-misclass.test.rf
  # PRUC.cv[i,3]=PRAUC(y_pred = prauc_test , y_true = testData$Accident.Severity)
  AUC.cv[i,3] <- ROC_auc@y.values[[1]]
  
  prc_dataFrame <- data.frame(pred.rf.test, testData_[,'Accident.Severity'])
  colnames(prc_dataFrame) = c('y_pred','set1_test_pred')
  prc <- pr.curve(scores.class1=prc_dataFrame[prc_dataFrame$set1_test_pred == 0,]$y_pred, 
                  scores.class0=prc_dataFrame[prc_dataFrame$set1_test_pred == 1,]$y_pred,
                  curve = TRUE)
  prauc = prc$auc.integral
  PRUC.cv[i,3]= prauc
  
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
boxplot(PRUC.cv, las=2, ylim=c(0.2,1),
        main="Precision-Recall AUC \n Cross-Validation")
low.c = apply(PRUC.cv, 1, min)
boxplot(ACC.cv/low.c, las=2,ylim=c(1,1.6),
        main="Relative Precision-Recall AUC \n Cross-Validation")
#
AUC.cv.mean= apply(X=AUC.cv,MARGIN=2,FUN=mean)
AUC.cv.mean
par(mar=c(7,5,5,5)+.1)
boxplot(AUC.cv, las=2, ylim=c(0.5,1),
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
model
#SMOTE
# nrounds max_depth eta  gamma colsample_bytree min_child_weight subsample
# 150         3     0.4   0              0.8                1         1

#normal 
# nrounds max_depth eta   gamma colsample_bytree min_child_weight subsample
# 50         2      0.4     0              0.8                1       0.5

# Make predictions on the test data
predicted.classes <- model %>% predict(set2)
# head(predicted.classes)
# Compute model prediction accuracy rate
mean(predicted.classes == set2$Accident.Severity)

varImp(model)

#############################################
######## prediction on test set ##################
casualties_test <- read.csv("test.csv", header=T)
head(casualties_test,2)
table(casualties_test$Accident.Severity)
prop.table(table(casualties_test$Accident.Severity))
### swap columns
col_swaps = c('AREFNO','ageband','Accident.Date','Time',
              'No..of.Vehicles.in.Acc.','No..of.Casualties',
              'Casualty.Age','Casualty.Sex','Casualty.Class',
              'Light.Conditions.Banded','Road.Surface','Weather','Junction.Detail',
              'Speed.Limit','Road.Type','Highway',
              'hour','day','month','Mode.of.Travel',
              'Accident.Severity') 
casualties_test<-subset(casualties_test, select=col_swaps)
##### data preprocessing #############
cols = c('Casualty.Sex','Casualty.Class',
         'Light.Conditions.Banded','Road.Surface','Weather','Junction.Detail',
         'Road.Type','Highway',
         'day','month','Mode.of.Travel',
         'Accident.Severity') 
casualties_test[cols] <- lapply(casualties_test[cols], factor)  ## as.factor() could also be used
### make month and day numerical
casualties_test$month=match(casualties_test$month,month.name)
table(casualties_test$day)
df_test = casualties_test
col_features = c("Casualty.Age", "Casualty.Sex",           
                 "Casualty.Class","Light.Conditions.Banded","Road.Surface","Weather",                
                 "Junction.Detail","Speed.Limit","Road.Type", "Highway",                
                 "hour","day","month" ,"Mode.of.Travel","Accident.Severity")
df_test = df_test[,col_features]
set1_test = df_test
set1_test = set1_test
set1_test$Accident.Severity <- factor(set1_test$Accident.Severity,
                                  levels= c("Serious", "Slight"),
                                  labels= c(1, 0))
cols = c('Casualty.Sex','Casualty.Class',
         'Light.Conditions.Banded','Road.Surface','Weather','Junction.Detail',
         'Road.Type','Highway',
         'day','month','Mode.of.Travel') 
set1_test[cols] <- lapply(set1_test[cols], as.numeric) 
set1_mt_test <- xgb.DMatrix(data = as.matrix(set1_test[,-15]), label= as.matrix(set1_test[,15]))
#######################################
# prediction
# SMOTE
params <- list(booster = "gbtree",
               max_depth = 3,
               eta = 0.4,
               subsample = 1,
               colsample_bytree = 0.8,
               min_child_weight = 1)
# normal
params <- list(booster = "gbtree",
               max_depth = 2,
               eta = 0.4,
               subsample = 0.5,
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

y_pred <- predict(xgb_tuned, set1_mt_test,type = 'response')
mean(y_pred == set1_test$Accident.Severity)

table(set1_test[,'Accident.Severity'],y_pred, dnn=c("Observed","Predicted"))
confusionMatrix(factor(y_pred), factor(set1_test[,'Accident.Severity']),mode='everything')
PRAUC(y_pred = as.numeric(y_pred ), y_true = as.numeric(set1_test[,'Accident.Severity']))


#AUC of Precision and recall
set1_test_y = set1_test$Accident.Severity
ROC_perf <- performance(prediction(y_pred,set1_test_y),"tpr","fpr")
prc_dataFrame <- data.frame(y_pred, set1_test[,'Accident.Severity'])
colnames(prc_dataFrame) = c('y_pred','set1_test_pred')
prc <- pr.curve(scores.class1=prc_dataFrame[prc_dataFrame$set1_test_pred == 0,]$y_pred, 
                scores.class0=prc_dataFrame[prc_dataFrame$set1_test_pred == 1,]$y_pred,
                curve = TRUE)
dim(prc_dataFrame[prc_dataFrame$set1_test_pred == 0,])
dim(prc_dataFrame[prc_dataFrame$set1_test_pred == 1,])
prauc = prc$auc.integral
cat("PRAUC:", prauc)

pred = ifelse(y_pred> 0.5,1,0)
cm = table(set1_test[,'Accident.Severity'],pred)
cat("confusion matrix", cm)

par(mfrow=c(1,2))
plot(ROC_perf)
plot(prc)



#####################################################
rf.tun <- randomForest(data=df, Accident.Severity~., mtry=2, nodesize=5,ntree=1000,
                       importance=TRUE, keep.forest=TRUE)

# # Predict results of classification. 
pred.rf.test.tun <- predict(rf.tun, newdata=df_test, type="response")

# (misclass.train.rf.tun <- mean(ifelse(pred.rf.train.tun == set1$Y, yes=0, no=1)))
(misclass.test.rf.tun <- mean(ifelse(pred.rf.test.tun != df_test$Accident.Severity, yes=0, no=1)))
# 
table(df_test[,'Accident.Severity'],pred.rf.test.tun, dnn=c("Observed","Predicted"))
confusionMatrix(factor(pred.rf.test.tun), factor(df_test[,'Accident.Severity']),mode='everything')
PRAUC(y_pred = as.numeric(pred.rf.test.tun), y_true = as.numeric(df_test$Accident.Severity))

prc_dataFrame <- data.frame(as.numeric(pred.rf.test.tun), as.numeric(df_test[,'Accident.Severity']))
colnames(prc_dataFrame) = c('y_pred','set1_test_pred')
prc <- pr.curve(scores.class1=prc_dataFrame[prc_dataFrame$set1_test_pred == 2,]$y_pred, 
                scores.class0=prc_dataFrame[prc_dataFrame$set1_test_pred == 1,]$y_pred,
                curve = TRUE)
dim(prc_dataFrame[prc_dataFrame$set1_test_pred == 0,])
dim(prc_dataFrame[prc_dataFrame$set1_test_pred == 1,])
prauc = prc$auc.integral
prauc
# # Recall-Precision curve   
# pred <- prediction(as.numeric(pred.rf.test.tun), as.numeric(df_test$Accident.Severity));
# 
# RP.perf <- performance(pred, "prec", "rec");
# plot (RP.perf);
# 
# # ROC curve
# ROC.perf <- performance(pred, "tpr", "fpr");
# plot (ROC.perf);
