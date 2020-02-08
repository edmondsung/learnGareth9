# 9.6 SVM lab
# 96.1 suport vector classifier
set.seed(1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,] =x[y==1,] +1
plot(x, col=(3-y))
# next  fit sv classifier
dat = data.frame (x=x , y=as.factor(y))
library(e1071)
svmfit = svm(y~., data=dat, kernel="linear", cost=10, scale=FALSE)
plot(svmfit, dat)
svmfit$index
#support vectors (7) are cross , their ID show above
summary(svmfit)
#what if we use a smaller cost?
svmfit2 = svm(y~., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit2, dat)
svmfit2$index
# e1071 has built in CV to tune, tune() 10fold CV
set.seed(1)
tune.out = tune(svm,y~., data=dat, kernel="linear", ranges=list(cost= c(0.001 , 0.01 , 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)
# generate a test data set
xtest = matrix (rnorm(20*2) , ncol=2)
ytest = sample(c(-1,1) , 20 , rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] +1
#
testdat=data.frame(x=xtest, y=as.factor(ytest))
ypred=predict(bestmod, testdat)
table(predict=ypred , truth = testdat$y)
#what if cost = 0.01
svmfit3 = svm(y~., data=dat, kernel="linear", cost=0.01, scale=FALSE)
ypred=predict(svmfit3, testdat)
table(predict=ypred , truth = testdat$y)
# now consider situation 2 classes are linearly separable
x[y==1,]=x[y==1,]+0.5
plot (x, col=(y+5)/2 , pch=19)
#
dat = data.frame (x=x , y=as.factor(y))
svmfit4 = svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit4)
plot(svmfit4, dat)
#try a smaller value of cost
svmfit5 = svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit5)
plot(svmfit5, dat)