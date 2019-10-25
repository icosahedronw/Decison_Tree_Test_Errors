library(MASS)
set.seed(3221)
X<-matrix(0,12000,10)
for (i in 1:10) {
  X[,i]<-rnorm(12000)  
}
dim(X)
expr<-function(X){
  if(sum((X^2))>=9.34)
    return(1)
  else return(0)
}
Y<-rep(0,12000)
for (j in 1:12000) {
  Y[j]<-expr(X[j,])
}

Y<-as.factor(Y)
data<-cbind(Y,data.frame(X))
class(data)
class(data$Y)
head(data)

set.seed(3221)
n <- nrow(data)
rnd <- sample(n,n*1/6)
train <- data[rnd,]
test <- data[-rnd,]
attach(data)
dim(train)
dim(test)

#tree
set.seed(3221) 
library(tree)
fit2.tree<-tree(Y~., data=train)
summary(fit2.tree)
plot(fit2.tree)
text(fit2.tree,cex=0.6)
set.seed(3321)
cvtree<-cv.tree(fit2.tree,FUN=prune.tree)
plot(cvtree$size,cvtree$dev,type='b')

#pruning
prune.tree<-prune.tree(fit2.tree,best=20)
plot(prune.tree)
text(prune.tree,pretty=0,cex=0.6)
pred.tree<-predict(fit2.tree,newdata=test,type = 'class')
table(pred.tree,test$Y)
error.tree=1-mean(pred.tree==test$Y)

#STUMP
set.seed(3321)
fit.stump<-prune.tree(fit2.tree,best=2)
summary(fit.stump)
plot(fit.stump)
text(fit.stump,cex=0.6)
pred.stump<-predict(fit.stump,newdata=test,type = 'class')
table(pred.stump,test$Y)
error.stump=1-mean(pred.stump==test$Y)

#RandomForest
library(randomForest)
grid<-seq(1,500,25)
err.rf<-as.numeric()
for (i in 1:20) {
  fit.rf <- randomForest(Y~., data=train, ntree=grid[i],importance=TRUE) 
  pred.rf<-predict(fit.rf,test,type='class')
  table4<-table(pred.rf,test$Y)
  err.rf[i]<-1-(table4[1,1]+table4[2,2])/sum(table4)
}
sort(importance(fit.rf,type = 1),decreasing = T)[1:20]

#Bagging
grid<-seq(1,500,25)
err.bagging<-as.numeric()
for (i in 1:20) {
  fit.bagging <- randomForest(Y~., data=train, ntree=grid[i],importance=TRUE,mtry=10) 
  pred.bagging<-predict(fit.bagging,test,type='class')
  table3<-table(pred.bagging,test$Y)
  err.bagging[i]<-1-(table3[1,1]+table3[2,2])/sum(table3)
}

#Adaboost
library(gbm)
library(adabag)
error.adaboost<-as.numeric()
for (i in 1:20) {
  fit.adaboost<-boosting(Y~., data=train, mfinal = grid[i]) 
  pred.adaboost<-predict.boosting(fit.adaboost,newdata = test)
  error.adaboost[i]<-pred.adaboost$error
}

#plot1
err.rf <- as.data.frame(err.rf)
error.stump <- as.data.frame(error.stump)
error.tree <- as.data.frame(error.tree)
err.bagging<- as.data.frame(err.bagging)
error.adaboost<- as.data.frame(error.adaboost)
A<-data.frame(1:20)
library(ggplot2)
library(scales)
p1 <- ggplot(err.rf,aes(1:20,y=err.rf))
p1+ geom_line(aes(x=1:20,y=err.rf),colour="red", linetype="dashed",size = 1)+
  geom_point(aes(x=1:20,y=err.rf),colour="red",size=3, shape=18)+
  geom_line(aes(x=1:20,y=error.tree),colour="blue", linetype="dashed",size = 1)+
  geom_point(aes(x=1:20,y=error.tree),colour="blue",size=3, shape=18)+
  geom_line(aes(x=1:20,y=error.stump),colour="green", linetype="dashed",size = 1)+
  geom_point(aes(x=1:20,y=error.stump),colour="green",size=3, shape=18)+
  geom_line(aes(x=1:20,y=err.bagging),colour="black", linetype="dashed",size = 1)+
  geom_point(aes(x=1:20,y=err.bagging),colour="black",size=3, shape=18)+
  geom_line(aes(x=1:20,y=error.adaboost),colour="purple", linetype="dashed",size = 1)+
  geom_point(aes(x=1:20,y=error.adaboost),colour="purple",size=3, shape=18)+
  xlab("number of trees")+
  ylab("Test Error")+
  scale_x_continuous(breaks=A, labels = A*25)

#plot2
A<-data.frame(1:20)
Number<-seq(1,500,25)
Number<-rep(Number,times=5)
type<-rep(c('adaboost','stump','tree','bagging','randomforest'),each=20)
value<-c(error.adaboost,rep(error.stump,20),rep(error.tree,20),err.bagging,err.rf)
df<-data.frame(Number=Number,type=type,value=value)
df



