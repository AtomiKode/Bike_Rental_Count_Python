rm(list=ls())

#Set current working directory
setwd('C:/Users/SAGAR/Downloads/Data Science/Bike _Rental_Count')

#Loading important libraries
x = c('ggplot2', 'corrgram', 'DataCombine','lubridate','dplyr','geosphere','gridExtra',
      'DataExplorer','tidyr')

lapply(x, require, character.only = TRUE)



df=read.csv("day.csv",header=T)

summary(df)
dim(df)
str(df)

#Cheking the missing values
colSums(is.na(df)) #There are no missing values in the dataset

# Removing instant variable which is just index number of the records 
df=select(df,-c('instant'))

#Extracting Day variable from dteday
df$day=(day(df$dteday))

df=select(df,-c(dteday))

df=df[,c(15,1:14)]




###################################### Data Cleaning and Exploratory data analysis #############################

#Making pie chart to compare Registered and Casual user count
pie = data.frame( 
                group = c("Casual", "Registered"),
                value = c(mean(df$casual),mean(df$registered))
                )
head(pie)

pie_plot =  ggplot(pie, aes(x="", y=value, fill=group)) + geom_bar(stat="identity", width=1)+ coord_polar("y", start=0) + 
            geom_text(aes(label = paste0((round((value*100)/sum(value))), "%")), position = position_stack(vjust = 0.5))+
            labs(x = NULL, y = NULL, fill = NULL, title = "Comparision - Casual vs Registered")+
            theme_classic() + 
            theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())

pie_plot


#Also, we remove casual and registered variables as we are only concerned with the total rental count
df=select(df,-c(casual,registered))
df=select(df,-c(atemp))


#Now, we will do some Exploratory data analysis

library("scales")
library("psych")
library("gplots")



#Season vs.Count
ggplot(df, aes_string(x = df$season,y=df$cnt)) +
  geom_point(aes(color = df$cnt)) + theme_bw() +
  xlab("Seasons") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Change in Count with Seasons") +  theme(text=element_text(size=15))

#Weather vs. Count
ggplot(df, aes_string(x = as.factor(df$weathersit),y=df$cnt)) +
  geom_point(aes(color = df$cnt)) + theme_bw() +
  xlab("Weather") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Change in Count with Weather") +  theme(text=element_text(size=15))

#Holiday vs. Count
ggplot(df, aes_string(x = as.factor(df$holiday),y=df$cnt)) +
  geom_point(aes(color = df$cnt)) + theme_bw() +
  xlab("Holiday") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Change in Count with Holiday") +  theme(text=element_text(size=15))

#Workingday vs. Count
ggplot(df, aes_string(x = as.factor(df$workingday),y=df$cnt)) +
  geom_point(aes(color = df$cnt)) + theme_bw() +
  xlab("Workingday") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Change in Count with Workingday") +  theme(text=element_text(size=15))


#Year vs. Count
ggplot(df, aes_string(x = as.factor(df$yr),y=df$cnt)) +
  geom_point(aes(color = df$cnt)) + theme_bw() +
  xlab("Year") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Change in Count with Year") +  theme(text=element_text(size=15))

#Month vs. Count
ggplot(df, aes_string(x =as.factor(df$mnth),y=df$cnt)) +
  geom_point(aes(color = df$cnt)) + theme_bw() +
  xlab("Month") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Change in Count with Month") +  theme(text=element_text(size=15))


#Date vs. Count
ggplot(df2, aes_string(x = df2$day  ,y=df2$cnt)) +
  geom_point(aes(color = df2$cnt)) + theme_bw() +
  xlab("Date") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=30))+
  ggtitle("Change in Count with Date") +  theme(text=element_text(size=15))


#Weekday vs. Count
ggplot(df, aes_string(x = df$weekday,y=df$cnt)) +
  geom_point(aes(color = df$cnt)) + theme_bw() +
  xlab("Weekday") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=7))+
  ggtitle("Change in Count with Weekday") +  theme(text=element_text(size=15))



#Scatter Plots for continuous variables
#Temp vs. Count
ggplot(df, aes_string(x = df$temp, y = df$cnt)) + 
  geom_point(aes_string(colour = df$cnt),size = 4) +
  theme_bw()+ ylab("Count") + xlab("Temperature") + ggtitle("Temp vs. Count") + 
  theme(text=element_text(size=25))+scale_y_continuous(breaks=pretty_breaks(n=7)) + 
  scale_x_continuous(breaks=pretty_breaks(n=9))

#Windspeed vs. Count
ggplot(df, aes_string(x = df$windspeed, y = df$cnt)) + 
  geom_point(aes_string(colour = df$cnt),size = 4) +
  theme_bw()+ ylab("Count") + xlab("Windspeed") + ggtitle("Windspeed vs. Count") + 
  theme(text=element_text(size=25))+scale_y_continuous(breaks=pretty_breaks(n=7)) + 
  scale_x_continuous(breaks=pretty_breaks(n=9))

#Humidity vs. Count
ggplot(df, aes_string(x = df$hum, y = df$cnt)) + 
  geom_point(aes_string(colour = df$cnt),size = 4) +
  theme_bw()+ ylab("Count") + xlab("Humidity") + ggtitle("Humidity vs. Count") + 
  theme(text=element_text(size=25))+scale_y_continuous(breaks=pretty_breaks(n=7)) + 
  scale_x_continuous(breaks=pretty_breaks(n=9))


#Building correlation plot
library(corrplot)
corr=cor(df)
corrplot(corr, method="color",type="lower",addCoef.col = "black")


seasons.f = factor(df$season)
dummies = model.matrix(~seasons.f)

#Creating dummy variables from our categorical variable
library(dummies)
df=dummy.data.frame(df, c('season','weathersit'))


cols1=c('cnt','temp','hum','windspeed','yr','mnth','day','weekday','holiday','workingday','season1', 'season2',
       'season3','season4','weathersit1','weathersit2','weathersit3')

df=df[,cols1]



############################################# Outlier Analysis #################################################

df
par(mfrow=c(1,3))

#Box plot for temp outliers
boxplot(df$temp, main="Temperature",sub=paste(boxplot.stats(df$temp)$out))

#Box plot for humidity outliers
boxplot(df$hum,main="Humidity",sub=paste(boxplot.stats(df$hum)$out))

#Box plot for windspeed outliers
boxplot(df$windspeed,main="Windspeed",sub=paste(boxplot.stats(df$windspeed)$out))

par(mfrow=c(1,1))
#Box plot for count outliers
boxplot(df$cnt,main="Count",sub=paste(boxplot.stats(df$cn)$out))



#Only windspeed and humidity contain outliers

install.packages('missForest')
library(missForest)
library(DMwR)
#create subset for windspeed and humidity variable
wind_hum = subset(df,select=c('windspeed','hum'))

#column names of wind_hum
cnames = colnames(wind_hum)
for(i in cnames){
  val=wind_hum[,i][wind_hum[,i] %in% boxplot.stats(wind_hum[,i])$out]   # select outlier values
  wind_hum[,i][wind_hum[,i] %in% val]= NA                               # Replace outliers with NA 
}

df2 = subset(df2,select=-c(windspeed,hum))
#Combined new_df and wind_hum data frames
df2 = cbind(df2,wind_hum)

wind_hum1 = missForest(df2, verbose = TRUE)

df2=wind_hum1$ximp

cols1=c('cnt','temp','hum','windspeed','yr','mnth','day','weekday','holiday','workingday','season1', 'season2',
        'season3','season4','weathersit1','weathersit2','weathersit3')

df2=df2[,cols1]


####################################### Creating Function for Calculating Error ###############################

library(Metrics)

errors = function(y, yhat){
  sse=sse(y,yhat)
  n=length(y)
  k=8
  AIC = 2*k + n*log(sse/n)
  
  print("RMSLE:"); print(rmsle(y,yhat));
  print("RMSE:") ; print(rmse(y, yhat)); 
  print("MAE:" ) ; print(mae(y, yhat));
  print("MAPE:") ; print(mape(y, yhat));
  print("AIC:") ; print(AIC);
  
}

############################################ Model Development #############################################

set.seed(1234)
train_index = sample(1:nrow(df2), 0.8 * nrow(df2))
train = df2[train_index,]
y1=train$cnt
traincheck=subset(train,select=-cnt)

valid = df2[-train_index,]
y2=valid$cnt
valid=subset(valid,select=-cnt)

str(df2)


######################## Random Forest Regression ################################
set.seed(123)
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 200)

RF_Predictions = predict(RF_model, traincheck)
errors(y1,RF_Predictions)

RF_Predictions = predict(RF_model, valid)
errors(y2,RF_Predictions)


########################## Adaboost Regression ###################################
library(gbm)
set.seed(102)  
adaboost <- gbm(cnt~.,data=train,
                     distribution = "gaussian",
                     interaction.depth=3,
                     bag.fraction=0.7,
                     n.trees = 1000)

adaboost_pred = predict(adaboost, traincheck,n.trees = 1000)
errors(y1,adaboost_pred)

adaboost_pred = predict(adaboost, valid,n.trees = 1000)
errors(y2,adaboost_pred)

######################## KNN Regression ################################
library(caret)
install.packages('pROC')
library(pROC)

modelCtrl=trainControl(method='repeatedcv',number = 10,repeats = 3)

set.seed(123)
model=train(cnt~.,data=train, tuneGrid=expand.grid(k=1:70),
            method ='knn' , trControl=modelCtrl,preProc=c('center','scale'))
 
model

KNN_Predictions = predict(model, traincheck)
errors(y1,KNN_Predictions)

KNN_Predictions = predict(model, valid)
errors(y2,KNN_Predictions)


############################# LightGBM Regression ################################
library(lightgbm)

lgb.grid = list(objective = "regression",
                metric="rmse",
                learning_rate=0.05,
                num_leaves=100,
                feature_fraction = 0.7,
                bagging_fraction = 0.9,
                bagging_freq = 10)

lmodel=lightgbm(data = as.matrix(train),label =NULL,params= lgb.grid)

m_lgb = lgb.train(params = lgb.grid, data = lgb.train)

pred_lgb <- predict(m_lgb, as.matrix(traincheck))
errors(y1,pred_lgb)

pred_lgb <- predict(m_lgb, as.matrix(valid))
errors(y2,pred_lgb)


######################## XGBoost Regression ################################
library(xgboost)
dtrain <- xgb.DMatrix(data = as.matrix(traincheck), label =y1)
dvalid <- xgb.DMatrix(data = as.matrix(valid), label = y2)

p <- list(objective = "reg:linear",
          eval_metric = "rmse",
          max_depth = 8 ,
          eta = .05, 
          subsample=1,
          colsample_bytree=0.8,
          num_boost_round=100,
          nrounds = 600)


set.seed(122)
m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dvalid), print_every_n = 10, early_stopping_rounds = 100)

#XGBoost was overfitting the train data, but on using n rounds as 60, we get a better result
set.seed(122)

m_xgb <- xgb.train(p, dtrain,nrounds = 60)
pred_xgb <- predict(m_xgb, as.matrix(traincheck))
errors(y1,pred_xgb)

pred_xgb <- predict(m_xgb, as.matrix(valid))
errors(y2,pred_xgb)

#### Out of all the models we have implemented, Adaboost and XGBoost performed the best with RMSLE of 0.2326 and
#### 0.2304 respectively for validation data with variance of 0.044 and 0.141. Since XGBoost is overfitting to the 
#### data despite changing the values (tuning) manually, we consider Adaboost and perform hyperparameter tuning to   
#### see weather the results improve.

##################################### Summary of Models ######################################
summary_tab = data.frame(Model = c("Random Forest", "Adaboost", "KNN Regression", "LGBoost", "XGBoost"), 
                train_RMSLE = c(0.2197,0.1362,0.2961,0.482,0.089), 
                valid_RMSLE = c(0.2639,0.2326,0.344,0.5115,0.2304))

#Season vs.Count

ggplot(df, aes_string(x = df$Model,y=df$valid_RMSLE)) +
  geom_bar(stat="identity",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Models") + ylab('RMSLE') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Model Summary") +  theme(text=element_text(size=15))

#### Out of all the models we have implemented, Adaboost and XGBoost performed the best with RMSLE of 0.2326 and
#### 0.2304 respectively for validation data with variance of 0.044 and 0.141. Since XGBoost is overfitting to the 
#### data despite changing the values (tuning) manually, we consider Adaboost and perform hyperparameter tuning to   
#### see weather the results improve.



##################################### HyperParameter Tuning ###################################

ntree=250
set.seed(102)  
adaboost <- gbm(cnt~.,data=train,
                distribution = "gaussian",
                interaction.depth=3,
                bag.fraction=0.9,
                n.trees = ntree)

adaboost_pred = predict(adaboost, traincheck,n.trees = ntree)
errors(y1,adaboost_pred)

adaboost_pred = predict(adaboost, valid,n.trees = ntree)
errors(y2,adaboost_pred)


#At ntree = 250 and bagging fraction = 0.9, our model gives best RMSLE of 0.2139 with least variance between 
#training and validation data of 0.009.

########################## Creating Test set and Predicting Count Value ###############################

df = data.frame('temp'= c(0.4576,0.5476,0.8976,0.3545,0.2354),
                'hum'=c(0.3234,0.3424,0.7566,0.7567,0.8979),
                'windspeed'=c(0.2132,0.1323,0.3222,0.3131,0.1233),
                'yr'=c(1,0,0,1,0),
                'mnth'=c(7,11,10,11,40),
                'day'=c(4,27,21,15,9),
                'weekday'=c(1,5,6,6,4),
                'holiday'=c(1,0,0,1,0),
                'workingday'=c(0,0,1,0,1),
                'season1'=c(1,0,0,0,0), 
                'season2'=c(0,1,0,0,0),
                'season3'=c(0,0,1,0,0),
                'season4'=c(0,0,0,0,1),
                'weathersit1'=c(0,1,0,1,0),
                'weathersit2'=c(1,0,0,0,1),
                'weathersit3'=c(0,0,1,0,0))

adaboost_pred = predict(adaboost, df,n.trees = ntree)

predicted=cbind(df,adaboost_pred)
cols1=c('adaboost_pred','temp','hum','windspeed','yr','mnth','day','weekday','holiday','workingday','season1', 'season2',
        'season3','season4','weathersit1','weathersit2','weathersit3')

predicted=predicted[,cols1]

write.csv(predicted,"Final Predictions.csv",row.names=F)
