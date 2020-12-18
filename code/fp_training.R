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
casualties <- read.csv("train.csv", header=T)
# casualties <- read.csv("test.csv", header=T)
# casualties <- read.csv("full_data.csv", header=T)

head(casualties,2)
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
table(casualties$day)

##########
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
set1 = df
set1_ = set1
set1_$Accident.Severity <- factor(set1_$Accident.Severity,
                                  levels= c("Serious", "Slight"),
                                  labels= c(1, 0))

## matrix for xgboost
cols = c('Casualty.Sex','Casualty.Class',
         'Light.Conditions.Banded','Road.Surface','Weather','Junction.Detail',
         'Road.Type','Highway',
         'day','month','Mode.of.Travel') 
set1_[cols] <- lapply(set1_[cols], as.numeric) 
#######################################
##########
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}


##
ACC.cv = matrix(NA, nrow=10, ncol=4)
colnames(ACC.cv) = c('LogisticReg','XGboost','RF','NNs')

####
#Create 10 equally size folds
folds <- cut(seq(1,nrow(set1)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  print(i)
  # i=1
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
 
  pc <-  prcomp(x=set1_[-testIndexes,-15], scale.=TRUE)
  trainData_ <- data.frame(pc$x,
                     Accident.Severity = as.factor(set1_[-testIndexes,'Accident.Severity']))
  testData_ <- data.frame(predict(pc, newdata=set1_[testIndexes, ]), 
                     Accident.Severity = as.factor(set1_[testIndexes, 'Accident.Severity']))
  
  
  #
  set1.rescale <- data.frame(cbind(rescale(set1_[-testIndexes,-15], set1_[-testIndexes,-15]), 
                                   Accident.Severity=set1_[-testIndexes,'Accident.Severity']))
  
  set2.rescale <- data.frame(cbind(rescale(set1_[testIndexes,-15], set1_[-testIndexes,-15]),
                                   Y=set1_[testIndexes,'Accident.Severity'] )      )
  ########################## Logistic Regressor #################################
  classifier = glm(formula = Accident.Severity ~., 
                   family = binomial,data =trainData_)
  y_pred_test = predict(classifier,type = 'response',newdata = testData_)
  y_pred_test_ = ifelse(y_pred_test > 0.5,'Serious','Slight')
  acc_test=mean(y_pred_test_ == testData$Accident.Severity)
  ACC.cv[i,1] = acc_test
  
  # classifier = glm(formula = Accident.Severity ~., 
  #                  family = binomial,data =set1.rescale)
  # y_pred_test = predict(classifier,type = 'response',newdata = set2.rescale)
  # y_pred_test_ = ifelse(y_pred_test > 0.5,'Serious','Slight')
  # acc_test=mean(y_pred_test_ == testData$Accident.Severity)
  # ACC.cv[i,1] = acc_test
  #################### XG Boost ####################################################
  classifier_xg = xgboost(data = set1_mt,nrounds = 100,
                          objective = "binary:logistic")
  y_pred_test = predict(classifier_xg,type = 'response',newdata = set2_mt)
  y_pred_test_ = ifelse(y_pred_test > 0.5,'Serious','Slight')
  ACC.cv[i,2]=mean(y_pred_test_ == testData$Accident.Severity)
  
  ########################### Random Forest #############################
  wh.rf <- randomForest(data=trainData_, Accident.Severity~., importance=TRUE, keep.forest=TRUE)

  # Predict results of classification.
  pred.rf.test <- predict(wh.rf, newdata=testData_, type="response")
  pred.rf.vtest <- predict(wh.rf, newdata=testData_, type="vote")
  (misclass.test.rf <- mean(ifelse(pred.rf.test == testData_$Accident.Severity, yes=0, no=1)))
  ACC.cv[i,3]=1-misclass.test.rf

  ############## NN ########################
  mod.fit <- multinom(data=set1.rescale, formula= Accident.Severity~ ., 
                      trace=TRUE, maxit = 1500)
  pred.class.2 <- predict(mod.fit, newdata=set2.rescale) 
  (mul.misclass.test <- mean(ifelse(pred.class.2 == set1_[testIndexes,'Accident.Severity'], 
                                    yes=0, no=1)))
  ACC.cv[i,4]=1-mul.misclass.test
  
}

ACC.cv
ACC.cv.mean= apply(X=ACC.cv,MARGIN=2,FUN=mean)
ACC.cv.mean
par(mar=c(7,5,5,5)+.1)
boxplot(ACC.cv, las=2, ylim=c(0.3,0.7),
        main="Accuracy with PCA \n 10-fold Cross-Validation")
low.c = apply(ACC.cv, 1, min)
boxplot(ACC.cv/low.c, las=2,ylim=c(0.8,2),
        main="Relative Accuracy \n Cross-Validation")

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


# pc <-  prcomp(x=set1_[,-15], scale.=TRUE)
# set1 <- data.frame(pc$x,Accident.Severity = as.factor(set1_$Accident.Severity))
# set2 <- data.frame(predict(pc, newdata=set2_), Accident.Severity = as.factor(set2_$Accident.Severity))


##################################
## XGBoost
set.seed(46685326,kind="Mersenne-Twister")

model <- train(
  Accident.Severity ~., data = set1, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)
model$bestTune
model
model

# Make predictions on the test data
predicted.classes <- model %>% predict(set2)
mean(predicted.classes == set2$Accident.Severity)
# mean(predicted.classes == set2$class)


varImp(model)
#####################################################
#################### RF ########
# rf.fit <- train(Accident.Severity ~ ., 
#                 data = set1, 
#                 method = "rf",     # Use the "random forest" algorithm
#                 importance = TRUE, # importance=TRUE allows to inspect variable importance
#                 trControl = trainControl(method = "repeatedcv", # Use cross-validation
#                                          number = 10, # Use 10 folds for cross-validation
#                                          repeats = 5)
# )
# 
# 
# rf.fit$finalModel
# plot(rf.fit)
# 
# plot(rf.fit$finalModel)
# 
# plot(varImp(rf.fit), top = 20)
####################################################
############## cv RF ############
reps=5
varz = c(2,4,6,10,18)
nodez = c(1,3,5,7,10)

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
      rf.oob[counter,r] = 1-mean(predict(wh.rfm, type="response") != set1$Accident.Severity)
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
boxplot(rf.oob, use.cols=FALSE, las=2,ylim=c(0.64,0.67)
        ,main="RF Accuracy: Tuning Variables and Node Sizes")

# x11(h=7,w=10,pointsize=8)
boxplot(t(rf.oob)/min.oob, use.cols=TRUE, las=2, 
        main="RF Tuning Variables and Node Sizes")

# Suggested parameters are mtry=4, nodesize=1
set.seed(46685326,kind="Mersenne-Twister")
wh.rf.tun <- randomForest(data=set1, Accident.Severity~., mtry=4, nodesize=1,ntree=1000,
                          importance=TRUE, keep.forest=TRUE)
# Predict results of classification. 
pred.rf.train.tun <- predict(wh.rf.tun, newdata=set1, type="response")
pred.rf.test.tun <- predict(wh.rf.tun, newdata=set2, type="response")
#"vote" gives proportions of trees voting for each class
pred.rf.vtrain.tun <- predict(wh.rf.tun, newdata=set1, type="vote")
pred.rf.vtest.tun <- predict(wh.rf.tun, newdata=set2, type="vote")
head(cbind(pred.rf.test.tun,pred.rf.vtest.tun))

(misclass.train.rf.tun <- mean(ifelse(pred.rf.train.tun == set1$Accident.Severity, yes=0, no=1)))
(misclass.test.rf.tun <- mean(ifelse(pred.rf.test.tun == set2$Accident.Severity, yes=0, no=1)))



########################################
source("Helper Functions.R")

n = nrow(set1)
M = 5 # Number of times to re-fit each model

### Define parameter values and use expand.grid() to get all combinations
all.mtry = c(2,4,6,10,18) #mtry
all.nodesize = c(1,3,5,7,10)  # nodesize


all.pars = expand.grid(mtries = all.mtry,
                       nodesizes = all.nodesize)
n.pars = nrow(all.pars) # Number of parameter combinations
K = 5 # Number of folds
### Create folds
folds = get.folds(n, K)
### Create container for test Merror
CV.Merror = array(0, dim = c(K, n.pars))
for(i in 1:K){
  ### Print progress update
  print(paste0(i, " of ", K))

  ### Split data and rescale predictors
  data.train = set1[folds != i,]
  XY.train = data.train

  data.valid = set1[folds == i,]
  XY.valid =data.valid

  ### Fit RF models for each parameter combination. A second
  ### for loop will make our life easier here
  for(j in 1:n.pars){
    ### Get current parameter values
    this.mtry = all.pars[j,1]
    this.nodesize = all.pars[j,2]

    ### We need to run RF multiple times to avoid bad local minima. Create
    ### containers to store the models and their errors.
    all.rfs = list(1:M)
    all.Merror = rep(0, times = M)

    ### We need to fit each model multiple times. This calls for another
    ### for loop.
    for(l in 1:M){
      ### Fit model
      wh.rfm <- randomForest(data=XY.train, Accident.Severity~.,
                             mtry=this.mtry, nodesize=this.nodesize, ntree=1000)

      # get misclassification rate
      Merror.rf = mean(predict(wh.rfm, type="response") != XY.train$Accident.Severity)

      all.rfs[[l]] = wh.rfm
      all.Merror[l] = Merror.rf
    }

    ### Get best fit using current parameter values
    ind.best = which.min(all.Merror)
    fit.rf.best = all.rfs[[ind.best]]

    ### Get predictions and Test Merror, then store Test Merror
    Merror.val.rf = mean(predict(fit.rf.best, type="response", newdata=XY.valid) != XY.valid$Accident.Severity)
    CV.Merror[i, j] = 1-Merror.val.rf # Be careful with indices for CV.Merror

  }
}
CV.Merror


dim(CV.Merror)
(mean.MerrorCV = apply(X=CV.Merror, MARGIN=2, FUN=mean))

V=K

(mean.MerrorCV.sd = apply(X=CV.Merror, MARGIN=2, FUN=sd))
Merror.cv.CIl = mean.MerrorCV - qt(p=.975, df=V-1)*mean.MerrorCV.sd/V
Merror.cv.CIu = mean.MerrorCV + qt(p=.975, df=V-1)*mean.MerrorCV.sd/V

round(cbind(Merror.cv.CIl, Merror.cv.CIu),3)
table = round(cbind(Merror.cv.CIl, Merror.cv.CIu,mean.MerrorCV),3)
colnames(table)[3] = "mean"
names.pars = paste0(all.pars$mtries,",",
                    all.pars$nodesizes)
names.pars
rownames(table)= names.pars
table

colnames(CV.Merror) = names.pars

### Make boxplot
boxplot(CV.Merror, las = 2, main = "RF Tuning Variables and Node Sizes")
# boxplot(CV.Merror, las = 2, main = "Focused Test Accuracy Boxplot",ylim=c(0.78,0.85))

min.err = apply(CV.Merror, 1, min)

boxplot((CV.Merror)/min.err, use.cols=TRUE, las=2, 
        main="RF Tuning Variables and Node Sizes (Relative)")
############################
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

#####################################################
# rf.tun <- randomForest(data=set1, Accident.Severity~., mtry=2, nodesize=5,ntree=1000,
                       # importance=TRUE, keep.forest=TRUE)
rf.tun <- randomForest(data=df, Accident.Severity~., mtry=2, nodesize=2,ntree=1000,
                       importance=TRUE, keep.forest=TRUE)

# # Predict results of classification. 
pred.rf.test.tun <- predict(rf.tun, newdata=df_test, type="response")
# pred.rf.test.tun <- predict(rf.tun, newdata=set2, type="response")

# (misclass.train.rf.tun <- mean(ifelse(pred.rf.train.tun == set1$Y, yes=0, no=1)))
(misclass.test.rf.tun <- mean(ifelse(pred.rf.test.tun != df_test$Accident.Severity, yes=0, no=1)))
# (misclass.test.rf.tun <- mean(ifelse(pred.rf.test.tun != set2$Accident.Severity, yes=0, no=1)))

# 
table(df_test[,'Accident.Severity'],pred.rf.test.tun, dnn=c("Observed","Predicted"))
confusionMatrix(factor(pred.rf.test.tun), factor(df_test[,'Accident.Severity']),mode='everything')
# confusionMatrix(factor(pred.rf.test.tun), factor(set2[,'Accident.Severity']),mode='everything')

importance(rf.tun)


