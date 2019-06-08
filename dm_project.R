#Clearing all old material
rm(list = ls())
dev.off()
#Install Packages
#install.packages("corrplot")
#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("ggthemes")
#install.packages("randomForest")
#Libraries
library(corrplot)
library(ggplot2)
library(plotly)
library(ggthemes)
library(randomForest)
#Directory Settings
getwd()
setwd("C:\\Users\\shree\\Documents\\Quarter 3\\Data Mining\\Project\\bike-sharing-demand")
list.files()
View(train)
#Fetching data into R
train <- read.csv("train.csv")
test <- read.csv("test.csv")
head(train)
dim(train)
head(test)
dim(test)
#######################################Data Cleaning#######################################
#Checking the count of available NA values in the data
na_count <- sum(is.na(train))
na_count

str(train)
train$season = as.factor(train$season)
train$holiday = as.factor(train$holiday)
train$workingday = as.factor(train$workingday)
train$weather = as.factor(train$weather)

test$season = as.factor(test$season)
test$holiday = as.factor(test$holiday)
test$workingday = as.factor(test$workingday)
test$weather = as.factor(test$weather)

summary(train)
####################################Exploratory Data Analysis###############################
names(train)
#Checking relation of each variable with count of all users
ggplot(train,aes(season,count,color = season)) + geom_boxplot() +
  labs(title = "Season VS Count of Users", x="Season", y="Count of Users") + theme_economist() +  
  scale_x_discrete(labels=c("1" = "Spring", "2" = "Summer","3" = "Fall", "4"="Winter")) + 
  theme(legend.position = "none")
  
ggplot(train,aes(weather,count,color = weather)) + geom_boxplot() +
  labs(title = "Weather VS Count of Users", x="Weather", y="Count of Users") + theme_economist() +  
  scale_x_discrete(labels=c("1" = "Clear", "2" = "Mist","3" = "Light Rain", "4"="Thunderstorm")) + 
  theme(legend.position = "none")

ggplot(train,aes(temp,count)) + 
  geom_point(aes(color=temp),alpha=0.2) + theme_economist() + theme(legend.position = "none") +
  labs(title = "Scatter Plot: Temperature VS Count of Users", x="Temperatue", y="Count of Users") +
  geom_smooth(method='lm') 

ggplot(train,aes(atemp,count)) + 
  geom_point(aes(color=atemp),alpha=0.2) + theme_economist() + theme(legend.position = "none") +
  labs(title = "Scatter Plot: Feels Like Temperature VS Count of Users", x="Feels Like Temperatue", y="Count of Users") +
  geom_smooth(method='lm') 

###############################Hypothesis Testing#########################################
# Hypothesis 1: There might be more number of the registerd users at officing timings
head(train$datetime)
train$time <- format(as.POSIXct(strptime(train$datetime,"%m/%d/%Y %H:%M",tz="")) ,format = "%H:%M")
train$time
str(train$time)
train$time=as.factor(train$time)

#boxplot(train$count~train$time,xlab="hour", ylab="count of users")

ggplot(train,aes(time,count,color = time)) + geom_boxplot() +
  labs(title = "Boxplot:Hours VS Count of Users", x="Hours", y="Count of Users") + theme_economist() +  
  theme(legend.position = "none")

ggplot(train,aes(time,registered,color = time)) + geom_boxplot() +
  labs(title = "Boxplot:Hours VS Count of Registered Users", x="Hours", y="Count of Registered Users") + theme_economist() +  
  theme(legend.position = "none")

ggplot(train,aes(time,casual,color = time)) + geom_boxplot() +
  labs(title = "Boxplot:Hours VS Count of Casual Users", x="Hours", y="Count of Casual Users") + theme_economist() +  
  theme(legend.position = "none")

# Hypotrhesis 2: Number of Casual users increases on weekends and Number of registered users decreses on weekends
train$days <- weekdays(as.POSIXct(strptime(train$datetime,"%m/%d/%Y %H:%M",tz="")), abbreviate = F)
train$days

ggplot(train,aes(days,count,color = days)) + geom_boxplot() +
  labs(title = "Boxplot:Days VS Count of Users", x="Days", y="Count of Users") + theme_economist() +  
  theme(legend.position = "none")

ggplot(train,aes(days,registered,color = days)) + geom_point() +
  labs(title = "Boxplot:Days VS Count of Registered Users", x="Days", y="Count of registered Users") + theme_economist() +  
  theme(legend.position = "none")

ggplot(train,aes(days,casual,color = days)) + geom_point() +
  labs(title = "Boxplot:Days VS Count of Casual Users", x="Days", y="Count of Casual Users") + theme_economist() +  
  theme(legend.position = "none")
########################### Statistical Analysis####################################
# Checking Correlation between two all variables
str(train)
newdf <- data.frame(train$temp,train$atemp,train$humidity,train$windspeed,train$casual,train$registered,train$count)
head(newdf)
newdf_matrix <- as.matrix(as.data.frame(newdf)) 
matrix <- cor(newdf)
corrplot(matrix,order = "alphabet",method='color',addCoef.col = "red")
###########################  Models ##################################################
#Multiple linear regression
names(train)
lg = lm(formula = count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed, data = train)
lg
#test$predict_lm <- predict1


lg1 = lm(formula = count ~ season + holiday + workingday + weather + temp + windspeed, data = train)
lg1

summary(lg)
summary(lg1)

predict_train <- predict(lg1, train)
train$pred_train <- predict_train
res <- train$count -train$pred_train
rmse <- sqrt(mean(res ^ 2))
print(rmse)

# Checking with ANOVA
anova(lg1,lg)
#plot(predict1,predict2)

# Stepwise AIC checking
library(MASS)
lmBikeRentAIC<-stepAIC(lg1, direction="both")
summary(lmBikeRentAIC)


#Random Forest Modeling
set.seed(415)
fit1 <- randomForest(count ~ workingday+holiday +temp+humidity+atemp+windspeed+season+weather, data=train,importance=TRUE, ntree=250)
fit1
predict_train1=predict(fit1,train)
train$pred_train1 <- predict_train1
res <- train$count -train$pred_train1
rmse1 <- sqrt(mean(res ^ 2))
print(rmse1)


#Prediction on test data using random forest
predict_test=predict(fit1,test)
test$predicted_count <- predict_test
View(test)

#Registered Users
set.seed(415)
fit2 <- randomForest(registered ~ workingday+holiday +temp+humidity+atemp+windspeed+season+weather, data=train,importance=TRUE, ntree=250)
train$predict_registered=predict(fit2,train)
#res <- train$registered -train$predict_registered
#rmse3 <- sqrt(mean(res ^ 2))
#print(rmse3)


#Prediction on test data using random forest
predict_test_registered=predict(fit2,test)
test$predicted_registered <- predict_test_registered

#Casual Users
set.seed(415)
fit3 <- randomForest(casual ~ workingday+holiday +temp+humidity+atemp+windspeed+season+weather, data=train,importance=TRUE, ntree=250)
train$predict_casual=predict(fit3,train)
#res <- train$registered -train$predict_registered
#rmse3 <- sqrt(mean(res ^ 2))
#print(rmse3)


#Prediction on test data using random forest
predict_test_casual=predict(fit3,test)
test$predicted_casual <- predict_test_casual
