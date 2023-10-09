#Reading the csv
loans<-read.csv('loan_data.csv')
str(loans)
summary(loans)
loans$credit.policy<-factor(loans$credit.policy)
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)

library(ggplot2)
#Plotting the histogram of fico
pl<-ggplot(loans,aes(x=fico))
pl<-pl+geom_histogram(aes(fill=not.fully.paid),color='black',bins=40,alpha=0.5)
pl+scale_fill_manual(values=c('green','red'))+theme_bw()

#Plotting the bar plot of purpose
pl<-ggplot(loans,aes(x=factor(purpose)))
pl<-pl+geom_bar(aes(fill=not.fully.paid),position = 'dodge')
pl+theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1))

#Plotting the scatterplot of int.rate vs fico
ggplot(loans,aes(x=int.rate,y=fico))+geom_point()+theme_bw()
ggplot(loans,aes(x=int.rate,y=fico))+geom_point(aes(color=not.fully.paid),alpha=0.3) + theme_bw()


#Building and Training the model
library(caTools)
set.seed(101)
spl=sample.split(loans$not.fully.paid,SplitRatio = 0.7)
train=subset(loans,spl==T)
test=subset(loans,spl==F)
library(e1071)
model<-svm(not.fully.paid ~ .,data=train)
summary(model)

#Predicting the result
predicted.values<-predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)

#Tune the model to get the best cost and gamma value
#Here we will get different combination of model with different cost and gamma to find the best model
tune.results<-tune(svm,train.x=not.fully.paid ~ .,data=train,kernel='radial',
     ranges=list(cost=c(1,10),gamma=c(0.1,1)))
summary(tune.results)


#Tuned model or updated model with best cost and gamma value
model<-svm(not.fully.paid ~ .,data=train,cost=10,gamma=0.1)
predicted.values<-predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)
