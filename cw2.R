#2
library('ISLR')
len<-nrow(Carseats)
set.seed(1)
rand<-sample(len,len/2)
train<-Carseats[rand,]
test<-Carseats[-rand,]


library(tree)
model<-tree(Sales~.,data = train)
summary(model)
plot(model)
text(model,pretty=0)
pred<-predict(model,newdata = test)
#MSE
mean((pred- test$Sales)**2)
#The most important factor in determining the number of sales is the shelve location being bad or medium, or the shelve
#location being good. Once the shelve location is good, the major factors that affect sales are the price and the 
#competitors price. Lower prices tend to result in highest sales while higher competitor prices tend to lead to higher
#sales. Many additional factors(price and competitors price also play major roles) affect sales where there
#is a bad or medium shelve location. These are the average age of the local population, advertising budget and community
#income level.


set.seed(10)
cv.mod<-cv.tree(model)
plot(cv.mod$size,cv.mod$dev,type='b')
#From the graph, the lowest deviance is at size 6 nodes
prune1<-prune.tree(model,best = 6)
pred1<-predict(prune1,newdata = test)
#MSE 6 nodes
mean((pred1-test$Sales)**2)
#pruning the tree to 6 nodes resulted in worse test MSE


library('randomForest')
set.seed(10)
rf<-randomForest(Sales~.,data = train,mtry=ncol(train)-1, importance = TRUE)
predb<-predict(rf,newdata = test)
#MSE bagging
mean((predb-test$Sales)**2)
importance(rf)
barplot(sort(importance(rf)[,1]),xlab='Relative Importance')
#from the %increase in MSE, we see that the price factor has the highest importance



#number of predictors
a<-ncol(train)-1
#new mtry is
a/3
#we use mtry=3and mtry=4 in our random forest models
set.seed(10)
modrf<-randomForest(Sales~.,data = train,mtry=3,importance = TRUE)
predrf<-predict(modrf,newdata = test)
#MSE randomforest mtry=3
mean((predrf-test$Sales)**2)
importance(modrf)
barplot(sort(importance(modrf)[,1]),xlab='Relative Importance')
set.seed(10)
modrf2<-randomForest(Sales~.-Sales,data = train,mtry=4,importance = TRUE)
predrf2<-predict(modrf2,newdata = test)
#MSE randomforest mtry=3
mean((predrf2-test$Sales)**2)
importance(modrf2)
barplot(sort(importance(modrf2)[,1]),xlab='Relative Importance')
#we can see the effect of changing m on the MSE in the graph below.
testm<-rep(0,10)
for(i in 1:10){set.seed(10)
  modrf3<-randomForest(Sales~.-Sales,data = train,mtry=i,importance = TRUE)
  predrf3<-predict(modrf3,newdata = test)
  testm[i]<-mean((predrf3-test$Sales)**2)}
testm
plot(testm,type='b',xlab='m',ylab='MSE')
#from the graph, we  observe generally an exponential decrease in MSE as m increases


#3
library(ISLR)
library(tree)
set.seed(14)
sam<-sample(nrow(OJ),800)
train<-OJ[sam,]
test<-OJ[-sam,]

mod3<-tree(Purchase~.,data = train)
summary(mod3)
#The training error rate is 0.1875
#There are 7 terminal nodes in our tree
#LoyalCH and PriceDiff are the factors that affect Purchase the most.

mod3
#4) LoyalCH < 0.276142 161  120.80 MM ( 0.12422 0.87578 ) *
#This is a terminal node because it ends with *.This node gives the majority vote when LoyaltyCH is less than
#0.276142. There are 161 observations that satisfy this condition. The deviance is 120.80 and the ovverall
#prediction is the MM category. 87.578% of observations with LoyalCH<0.276142 purchase MM while 12.422% purchase CH


plot(mod3)
text(mod3,pretty = 0)
#The most important factor in determining purchase is the LoyalCH
#Most people will buy CH unless they have a very low brand loyalty to CH (<0.276142) or a quite low brand loyalty 
#to CH (<0.50395 set.seed(8)
cvmod3<-cv.tree(mod3,FUN = prune.misclass)

plot(cvmod3$size,cvmod3$dev,type = "b")but >=0.276142) and the prices of CH and MM are similar(priceDiff<0.05).


pred3<-predict(mod3,newdata = test,type = 'class')
table(pred3,test$Purchase)
mean(pred3!=test$Purchase)




#A tree size of 4

prunemod3<-prune.misclass(mod3,best = 4)

summary(prunemod3)
#The training error rate remains the same at 0.1875

pred3pruned<-predict(prunemod3,newdata = test, type = 'class')
mean(pred3pruned!=test$Purchase)
#The test error rate also remains the same at 0.1518519


#4
library(e1071)
library(ISLR)
gasm<-rep(3,nrow(Auto))
gasm<-ifelse(Auto$mpg>median(Auto$mpg),1,0)
gasm<-as.factor(gasm)

#removing mpg from being a predictor
dat4<-data.frame(gasm, Auto[,-1])

set.seed(1)
tuned<-tune(svm,gasm~.,data = dat4,kernel='linear',ranges=list(cost=c(0.001,0.01,0.1,1,10,100,1000)))
summary(tuned)
#cost=1 gives the lowest error of 0.09179487. we chose cost values around 1 and tune again to see if we get a
#lower error
set.seed(1)
tuned1<-tune(svm,gasm~.,data = dat4,kernel='linear',ranges=list(cost=c(0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8)))
summary(tuned1)

mod4<-tuned1$best.model
summary(mod4)
#From tuning, the cost 0.4 gives the lowest error of 0.07910256.
set.seed(1)
tune4poly<-tune(svm,gasm~.,data=dat4,kernel='polynomial', ranges = list(cost=c(0.01,0.1,1,10,100,1000),
                                                                       degree=c(1,2,3,4,5)))
summary(tune4poly)
#polynomials of degree 1 has the lowest error. Our lowest error of 0.08667 was obtained with cost 100 and degree 1
#adjusting cost and degree to get a lower error
set.seed(1)
tune4poly<-tune(svm,gasm~.,data=dat4,kernel='polynomial', ranges = list(cost=c(100,1000,10000,100000,1000000,10000000),
                                                                        degree=c(1,3)))
summary(tune4poly)
#we have the lowest error at cost 1000000, degree 3
#adjusting further
set.seed(1)
tune4poly<-tune(svm,gasm~.,data=dat4,kernel='polynomial', ranges = list(cost=c(1000000,2000000,3000000,4000000,
                                                                               5000000),
                                                                        degree=c(1,3)))
summary(tune4poly)
#Our lowest error 0.07634615 is obtained at cost=2000000,degree=3
mod4poly<-tune4poly$best.model
summary(mod4poly)


set.seed(1)
tune4rad<-tune(svm,gasm~.,data=dat4,kernel='radial', ranges = list(cost=c(0.01,0.1,1,10,100,1000),
                                                                        gamma=c(1,2,3,4,5)))
summary(tune4rad)
#adjusting cost and gamma
set.seed(1)
tune4rad<-tune(svm,gasm~.,data=dat4,kernel='radial', ranges = list(cost=c(0.4,0.6,0.8,1,1.2,1.4),
                                                                   gamma=c(1,2,3)))
summary(tune4rad)
#Our lowest error 0.07647436 is obtained at cost=1,gamma=1
mod4rad<-tune4rad$best.model
summary(mod4rad)

names(dat4)
plot(mod4,data = dat4,weight~horsepower)
plot(mod4poly,data = dat4,weight~horsepower)
plot(mod4rad,data = dat4,acceleraion~horsepower)
t<-svm(gasm~.,data=dat4, kernel = 'radial')
t
#5
x1<-c(3,2,4,1,2,4,4)
x2<-c(4,2,4,4,1,3,1)
y<-c(rep('RED',4),rep('BLUE',3))
plot(x1,x2,col=y,cex=1.5)

#from the sketch, the hyperplane will pass right through the middle of the two points at x1=2 and the two at x1=4
#at x1 = 2,x2 = 1.5  and at x1 = 4, x2 = 3.5
#we can calculate for the slope 3.5 - 1.5 / 4 - 2 = 1
#we can now solve for the intercept
#                    1.5 = 1*2 + intercept
#intercept = 1.5 - 2 = -0.5
plot(x1,x2,col=y,cex=1.5)
abline(-0.5,1,lwd=2)
# The equation ofthe hyperplane is
#                      -0.5 + X1 - X2 = 0


#Classify to Red if -0.5 + X1 - X2 < 0, and classify to Blue otherwise.


#the margin lines will have the same slope as the hyperplane but different intercepts
#for red margin:
#            2 = 1*2 + intercept
#            intercept = 2 - 2 = 0
#for blue line:
#            1 = 1*2 + intercept
#            intercept = 1 - 2 = -1
plot(x1,x2,col=y,cex=1.5)
abline(-0.5,1,lwd=2)
abline(-1,1,lty=2)
abline(0,1,lty=2)


#The support vectors are observations 2,3,5 and 6
#These are points (2,2),(4,4),(2,1),(4,3)


#plotting observation 7 in green
plot(x1,x2,col=y,cex=1.5)
abline(-0.5,1,lwd=2)
abline(-1,1,lty=2)
abline(0,1,lty=2)
points(4,1,col='green',cex=1.5,pch=16)
#As this green point is not close to the margin line, a slight change in this observation will not affect the margin
#line which means the maximum margin hyperplane will not be affected.


#A hyperplane passing through (2,1.8),(4,3.2)
#The slope of this line is 3.2 - 1.8 / 4 - 2 = 0.7
#The intercept is 
1.8 - (0.7*2)
#The intercept of the red margin is
2 - (0.7*2)
#The intercept of the blue margin is
3 - (0.7*4)
#plotting this line and the margins
plot(x1,x2,col=y,cex=1.5)
abline(0.4,0.7,lwd=2)
abline(0.6,0.7,lty=2)
abline(0.2,0.7,lty=2)
#the margin of this hyperplane is clearly smaller than that of the maximal margin hyperplane
#The equation is 
#             X2 - 0.7X1 - 0.4 = 0


plot(x1,x2,col=y,cex=1.5)
#adding the point 
points(3.5,1.5,col='red',cex=1.5)


#6
library(ISLR)
set.seed(10)
clust<-hclust(dist(USArrests),method = 'complete')
plot(clust)

cutclust<-cutree(clust,3)
cutclust

set.seed(10)
scaled<-scale(USArrests)
set.seed(10)
sclust<-hclust(dist(scaled),method = 'complete')
plot(sclust)


cutsclust<-cutree(sclust,3)
table(notscaled=cutclust,scaled=cutsclust)
mean(cutclust==cutsclust)
#we see that after scaling, only 56% of our results remain the same in hierachical clustering.
#the variables should be scaled.
#looking at the details of the dataset shows that the unit of the variable UrbanPop(%) is different from the other 
#3 variables(number of variable arrests per 100000 residents). Scaling will ensure the different variables have the same
#importance in the hierachical clustering being performed despite their different scales.


#7
set.seed(1)
a<- matrix(rnorm(20*50,mean = 15),nrow = 20)
b<- matrix(rnorm(20*50,mean = 16),nrow = 20)
c<- matrix(rnorm(20*50,mean = 17),nrow = 20)
data<-rbind(a,b,c)

pr<-prcomp(data)
plot(pr$x[,1:2],col=c(rep(1,20),rep(2,20),rep(3,20)))

set.seed(1)
km<-kmeans(data,3,nstart=20)
plot(data,col=km$cluster,pch=19)
true.label<-c(rep(1,20),rep(2,20),rep(3,20))
table(true=true.label,predicted=km$cluster)
#the clusters from k means match perfectly with the true class labels. There are three distinct labels,each with 
#20 observations and the same is predicted with kmeans using k = 3


set.seed(1)
km2<-kmeans(data,2,nstart=20)
plot(data,col=km2$cluster,pch=19)
table(true=true.label,predicted=km2$cluster)
#in this case, two of the true classes are lumped together to form one class with kmeans using k = 2. this results
#in two classes one with 20 observations and the other with 40 observations predicted.

set.seed(1)
km4<-kmeans(data,4,nstart=20)
plot(data,col=km4$cluster,pch=19)
table(true=true.label,predicted=km4$cluster)
#in this case, one of the true classes is divided into two distinct classes with kmeans using k = 4. This results
#in two classes with 20 observations each, one class with 9 observations and another class with 11 observations 
#predicted.

set.seed(1)
kmpc<-kmeans(pr$x[,1:2],3,nstart=20)
plot(pr$x[,1:2],col=kmpc$cluster,pch=19)
table(true=true.label,predicted=kmpc$cluster)
#the clusters from k means match perfectly with the true class labels. There are three distinct labels,each with 
#20 observations and the same is predicted with kmeans using k = 3 and the first two principal component"s 
#corresponding eigenvectors.

set.seed(1)
sdata<-scale(data)
kms<-kmeans(sdata,3,nstart=20)
plot(sdata,col=kms$cluster,pch=19)
table(true=true.label,predicted=kms$cluster)
#scaling does not change the accuracy of our model as three distinct classes are predicted each with 20 observations.
#This is the same using the raw data.

