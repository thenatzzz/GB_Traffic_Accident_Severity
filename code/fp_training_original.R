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

head(casualties,2)
colnames(casualties)

casualties = sample_n(casualties, 10000)
# head(casualties, 2)
table(casualties$Accident.Severity)
prop.table(table(casualties$Accident.Severity))



# print(prop.table(table(casualties$Accident.Severity)))

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

############# check NaN ######################
library(naniar)
# vis_miss(casualties, warn_large_data=FALSE)
########################################
############## split data #####################
df = casualties
colnames(df)
# shuffle data
rows <- sample(nrow(df))
df <- df[rows, ]


## choose only feature columns
# col_features = c("No..of.Vehicles.in.Acc.", "No..of.Casualties","Casualty.Age", "Casualty.Sex",           
                    # "Casualty.Class","Light.Conditions.Banded","Road.Surface","Weather",                
                # "Junction.Detail","Speed.Limit","Road.Type", "Highway",                
                  # "hour","day","month" ,"Mode.of.Travel","Accident.Severity")

col_features = c("Casualty.Age", "Casualty.Sex",           
                 "Casualty.Class","Light.Conditions.Banded","Road.Surface","Weather",                
                 "Junction.Detail","Speed.Limit","Road.Type", "Highway",                
                 "hour","day","month" ,"Mode.of.Travel","Accident.Severity")
df = df[,col_features]


### balanced data using SMOTE ####

df <- SMOTE(Accident.Severity ~ .,df)#, perc.over = 600,perc.under=100)
# shuffle data
rows <- sample(nrow(df))
df <- df[rows, ]
table(df$Accident.Severity)
prop.table(table(df$Accident.Severity))

# write.table(df, sep=",",dec = " ",file='smote10k_uk_accidents_processed.csv', row.names = FALSE)

####
library(caret)
# https://stackoverflow.com/questions/40709722/how-does-createdatapartition-function-from-caret-package-split-data
splitIndex <- createDataPartition(df$Accident.Severity, p = .9,
                                  list = FALSE,
                                  times = 1)
set1 <- df[ splitIndex,]
set2 <- df[-splitIndex,]
# colnames(set1)
# colnames(set2)

table(set1$Accident.Severity)
prop.table(table(set1$Accident.Severity))
table(set2$Accident.Severity)
prop.table(table(set2$Accident.Severity))


# set1 <- SMOTE(Accident.Severity ~ .,set1, perc.over = 600,perc.under=100)
# set2 <- SMOTE(Accident.Severity ~ .,set2, perc.over = 600,perc.under=100)
# table(set1$Accident.Severity)
# table(set2$Accident.Severity)



#################
# install.packages('MLmetrics')
library('MLmetrics')
logreg <- glm(formula = Accident.Severity ~ .,
              family = binomial, data = set1)
              # family = binomial(link = "logit"), data = set1)
PRAUC(y_pred = logreg$fitted.values, y_true = set1$Accident.Severity)
head(logreg$fitted.values)
tail(logreg$fitted.values)
head(set1$Accident.Severity)

######################
library(caTools)
library(xgboost)
library(ROCR)
library(PRROC)
# install.packages('PRROC')

#Applying logistic Regressor of Classification
classifier = glm(formula = Accident.Severity ~., 
                 family = binomial,data =set1)
y_pred = predict(classifier,type = 'response',newdata = set2)
PRAUC(y_pred = y_pred, y_true = set2$Accident.Severity)


# Run the ROCR functions for AUC calculation
set2_y = set2$Accident.Severity
ROC_perf <- performance(prediction(y_pred,set2_y),"tpr","fpr")
ROC_sens <- performance(prediction(y_pred,set2_y),"sens","spec")
ROC_err <- performance(prediction(y_pred, labels=set2_y),"err")
ROC_auc <- performance(prediction(y_pred,set2_y),"auc")
# AUC value
auc <- ROC_auc@y.values[[1]]
cat("AUC:",auc)

#AUC of Precision and recall
prc_dataFrame <- data.frame(y_pred, set2$Accident.Severity)
head(prc_dataFrame,4)
prc <- pr.curve(prc_dataFrame[prc_dataFrame$set2.Accident.Severity == 'Slight',]$y_pred, 
                prc_dataFrame[prc_dataFrame$set2.Accident.Severity == 'Serious',]$y_pred,
                curve = TRUE)

prauc = prc$auc.integral
cat("PRAUC:", prauc)

pred = ifelse(y_pred > 0.5,1,0)
cm = table(set2[,'Accident.Severity'],pred)
cat("confusion matrix", cm)

par(mfrow=c(1,2))
plot(ROC_perf)
plot(prc)

#####
#Xgboost

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

classifier_xg = xgboost(data = set1_mt,nrounds = 10,
                        objective = "binary:logistic")
pred_xg = predict(classifier_xg,type = 'response', newdata = set2_mt)
# y_pred
PRAUC(y_pred = pred_xg, y_true = set2_$Accident.Severity)



# Run the ROCR functions for AUC calculation
set2_y = set2_$Accident.Severity
ROC_perf <- performance(prediction(pred_xg,set2_y),"tpr","fpr")
ROC_sens <- performance(prediction(pred_xg,set2_y),"sens","spec")
ROC_err <- performance(prediction(pred_xg, labels=set2_y),"err")
ROC_auc <- performance(prediction(pred_xg,set2_y),"auc")
# AUC value
auc <- ROC_auc@y.values[[1]]
cat("AUC:",auc)

#AUC of Precision and recall
prc_dataFrame <- data.frame(pred_xg, set2_$Accident.Severity)
head(prc_dataFrame,4)
prc <- pr.curve(prc_dataFrame[prc_dataFrame$set2_.Accident.Severity == 1,]$pred_xg, 
                prc_dataFrame[prc_dataFrame$set2_.Accident.Severity == 0,]$pred_xg,
                curve = TRUE)

prauc = prc$auc.integral


cat("PRAUC:", prauc)

pred = ifelse(pred_xg > 0.5,1,0)
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

