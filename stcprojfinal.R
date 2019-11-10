#installing packages and loading relevant libraries

install.packages("xlsx");
install.packages("heuristica")
install.packages("caret")
install.packages("tidyr")
library("caret")
library("heuristica")
library("tidyr")
library("xlsx")
library("rpart")



#loading the dataset
getwd()
setwd("C:/Users/HP/Documents/data science")
data<- read.xlsx("startup_funding.xlsx",sheetIndex = 1);
model<-head(data,1000)
print(head(model));


#data wrangling ,cleaning and processing
model[is.na(model)]<-0;O<- -15;
data[is.na(data)]<-0;
model$City.Location[is.na(model$City.Location)]<-"BANGALORE";
model$Remarks<-NULL;
model$City.Location<-toupper(model$City.Location);
model$City.Location<-trimws(model$City.Location,which = "right");
model$City.Location<-as.factor(model$City.Location);
head(model)



#Dividing the dataset into train and test dataset

set.seed(1234)
ind <- sample(2,nrow(model),replace = TRUE,prob = c(0.8,0.2))
train_data <- model[ind ==1,]
test_data <- model[ind==2,]


#training the dataset to get a model using linear regression

mod<- lm(formula = Amount.In.USD~City.Location+Industry.Vertical+SubVertical+Date+Investment.Type,data = model)
summary(mod)



#using the test data set ,predicting the funding amount for the test data set

pred<- predict(object = mod,newdata = test_data,type ="response")
print(head(pred))
pred[is.na(pred)]<-0



#calculating the accuracy

acc <- pred/test_data$Amount.In.USD
acc[is.infinite(acc)]<-0
acc
acc<-ifelse(acc<0,O,acc)
accuracy<- sum(acc)/length(acc)
accuracy


#plotting graphs for further analysis
help("barplot")
barplot(tapply(data$Amount.In.USD, format(data$Date,"%Y"), FUN=sum),main = "Year wise Funding invested on Startups",xlab = "Year",ylab = "Amount")
barplot(rev(sort(tapply(data$Amount.In.USD, data$City.Location, FUN=sum)))[1:5],main = "Top 5 Cities in terms of Startup Funding",,xlab = "City",ylab = "Amount")
barplot(rev(sort(tapply(data$Amount.In.USD, data$Industry.Vertical, FUN=sum)))[1:5],main = "Top 5 Industry Vertical in terms of Startup Funding",xlab = "Industry Vertical",ylab = "Amount")

 