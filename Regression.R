View(Concrete_Data_Tab)
mydata<-Concrete_Data_Tab
summary(mydata)

maxs <- apply(mydata, 2, max)
mins <- apply(mydata, 2, min)
mydatascaled<-scale(mydata, center = mins, scale = maxs - mins)
mydatascaled<-as.data.frame(mydatascaled)

summary(mydatascaled)
plot(mydatascaled)

attach(mydatascaled)
lr.model<-lm(compressivestrength~Cement)
summary(lr.model)
anova(lr.model)

abline(lr.model)

plot(lr.model$fitted.values,lr.model$residuals)

newVal = data.frame(Cement = c(31,32,99))
predict.lm(lr.model, newdata = newVal, scale = TRUE)

#logistic regression

myData<-germancredit
summary(myData)

levels(myData$history)=c("good","good","poor","poor","terrible")

#cross Validation

train = sample(1000, 900)
head(train)

tr = myData[train,]
dim(tr)
te<-myData[-train,]


model <- glm(Default~.,data=tr, family="binomial")
summary(model)

pred <-predict(object = model, newdata = tr)

sum(pred>=0.5)

op<-rep(0,100)
op[pred>=0.5]<-1
table(actual=tr$Default,predicted=op)

na.omit(tr$Default)
options()
x<-c(0:10)
unique(x)[which.max(tabulate(match(x,unique(x))))]
