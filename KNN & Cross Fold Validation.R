#K NEAREST NEIGHBOR WORK & CROSSFOLD VALIDATION

library(MASS)
data("Boston")
head(Boston)
attach(Boston)

age.std <- scale(age)
rad.std <- scale(rad)
crim.std <- scale(crim)

age.std[,1]
rad.std
crim.std

BostonStd <- matrix(ncol = 3, nrow = 506)
BostonStd <- cbind(age.std[,1], rad.std[,1], crim)
head(crim.std)
x.std <- cbind(BostonStd[,1], rad.std[,1])
y <- BostonStd[,3]

#Fit 25 KNN using 2 variables to predict crim. Compute MSE.
library(FNN) #Initialze library FNN
predictions <- knn.reg(x.std, x.std, y, k=25)
mean((y-predictions$pred)^2)
?dim
#Compute LOOCV measure for CV w/ 25 nearest neighbors model.
n=dim(BostonStd)[1]
LOOCVpredictions = rep(NA,n)
for (fold in 1:n) {
  train.x = x.std[-fold,]
  train.x.std = scale(train.x)
  train.y = y[-fold]
  valid.x = matrix(x.std[fold,],ncol=2)
  valid.x.std = scale(valid.x, 
                      center = attr(train.x.std, "scaled:center"), 
                      scale = attr(train.x.std, "scaled:scale"))
  predictions = knn.reg(train.x.std, valid.x.std, train.y, k = 25)
  LOOCVpredictions[fold] = predictions$pred
}
mean( (y - LOOCVpredictions)^2 )

#Perform m-fold cross-validation. Use 10-fold.
set.seed(100)
m <- 10
groups <- c(rep(1:m, floor(n/m)), 1:(n%%m))
cvgroups <- sample(groups, n)
#Compute 10-fold cross-validated measure CV for 25 KNN.
mfoldCVpredictions = rep(NA,n)
for (fold in 1:m) {
  train.x = x.std[cvgroups != fold,]
  train.x.std = scale(train.x)
  train.y = y[cvgroups != fold]
  valid.x = x.std[cvgroups == fold,]
  valid.x.std = scale(valid.x, 
                      center = attr(train.x.std, "scaled:center"), 
                      scale = attr(train.x.std, "scaled:scale"))
  predictions = knn.reg(train.x.std, valid.x.std, train.y, k = 25)
  mfoldCVpredictions[cvgroups == fold] = predictions$pred
}
mean( (y - mfoldCVpredictions)^2 )

#Build linear regression model to compuete standard errors.
log.crim <- log(crim)
BostonTrans <- data.frame(age, rad, log.crim)
BostonTrans[,"log.crim"] <- log(crim)
BostonTrans$crim <- NULL
names(BostonTrans)
summary(lm(BostonTrans$log.crim ~ ., data = BostonTrans))

#Define beta.fn that takes inputdata and index and returns coeff for MLR.
beta.fn = function(inputdata,index) {
  lmfitboot = lm(formula = log.crim ~., data= inputdata[index,])
  return(lmfitboot$coef)
} 
beta.fn(BostonTrans,1:n)  # outputs coefficients of model fit on full dataset (observations 1 to n)


#Use boot function to compute standard error on bootstrap sample.
library(boot)
set.seed(100)
bootoutput <- boot(BostonTrans, beta.fn, R=5000)
print(bootoutput)


#HOMEWORK TWO...
trees <- read.csv("Trees.csv")
head(trees)
treeModel <- lm(Volume ~ Girth + Height + GirthHeight + Girth2 + Girth2Height, data = trees)
summary(treeModel)

m <- 5
n <- dim(trees)[1]
groups <- c(rep(1:5, 6), 1)
set.seed(2)
cvgroups <- sample(groups, n)
cvgroups
#Compute 5-fold CV
head(trees)
mfoldCVpredictions = rep(NA,n)
treeFrame <- cbind(trees[,3], trees[,4], trees[,5], trees[,6], trees[,7])
treeFrame
treeMSE <- rep(NA,5)
treeMSEadj <- rep(NA,5)
treeCV <- rep(NA,5) #Results from CV

predictedLM <- rep(0, n)
for (fold in 1:m) {
  groupi <- (cvgroups==fold) #T/F vector to call on part of data set. 
  lmfitCV <- lm(Volume ~ Girth + Height + GirthHeight + Girth2 + Girth2Height, data = trees, subset=!groupi)
  predictedLM[groupi] <- predict.lm(lmfitCV, trees[groupi,])
}
predictedLM
n
plot(treeModel$fitted.values, trees$Volume)
points(predictedLM, trees$Volume, pch=10, col="red")
CV5 <- sum((predictedLM-trees$Volume)^2)/n; CV5

#Use bootstrapping to estimate the variability of the coefficients.
tree.fn = function(inputdata, index){
  lmfitboot <- lm(Volume ~ Girth + Height + GirthHeight + Girth2 + Girth2Height, data = inputdata[index,])
  return(lmfitboot$coef)
}
library(boot)
set.seed(2)
bootoutput <- boot(trees, tree.fn, R=1000)
print(bootoutput)



#Going to use LOOCV to predict. 
m1 <- (Volume ~ Girth + Height + GirthHeight + Girth2 + Girth2Height)
m2 <- (Volume ~ Girth + Height)
m3 <- (Volume ~ Girth + Height + GirthHeight)
m4 <- (Volume ~ Girth + Height + Girth2 + Girth2Height)
m5 <- (Volume ~ Girth2 + Girth2Height)
m6 <- (Volume ~ Girth2Height)
allModels <- list(m1, m2, m3, m4, m5, m6)

k <- 31 # LOOCV is just CV with n = k. 
#groups <- c(rep(1:k, floor(n/k)), 1:(n-floor(n/k)*k)) #list of group labels
groups <- c(rep(1:k, 1))
?rep
set.seed(2)
cvgroups <- sample(groups, n)
cvgroups
n <- dim(trees)[1]; n
treeMSE <- rep(NA,6)
treeMSEadj <- rep(NA,6)
treeCV <- rep(NA,6) #Results from CV
for (m in 1:6){
  mPredFit <- lm(formula = allModels[[m]], data = trees)
  treeMSE[m] <- sum((mPredFit$fitted.values - trees$Volume)^2)/n #calculate mse
  treeMSEadj[m] <- sum((mPredFit$fitted.values - trees$Volume)^2)/(n-1-m)
  #predict via cross-validation
  allPredictedCV <- rep(0, n)
  for (i in 1:k){
    groupi <- (cvgroups == i)
    lmfitCV <- lm(formula = allModels[[m]], data = trees, subset=!groupi)
    allPredictedCV[groupi] = predict.lm(lmfitCV, trees[groupi,])
  }
  treeCV[m] <- sum((allPredictedCV - trees$Volume)^2)/n
}
i<-1
allPredictedCV[groupi]
cbind(treeMSE, treeMSEadj, treeCV) #mseAdj we're dividing by a smaller number.
#We'll see increase in allmodelCV because we've removed 
allModels[[m]]
allModels[m]
#Now do for 5 folds --
m<-1
k <- 5 # LOOCV is just CV with n = k. 
groups <- c(rep(1:k, floor(n/k)), 1:(n-floor(n/k)*k)) #list of group labels
set.seed(2)
cvgroups <- sample(groups, n)
cvgroups
n <- dim(trees)[1]; n
treeMSE <- rep(NA,6)
treeMSEadj <- rep(NA,6)
treeCV <- rep(NA,6) #Results from CV
for (m in 1:6){
  mPredFit <- lm(allModels[[m]], data = trees)
  treeMSE[m] <- sum((mPredFit$fitted.values - trees$Volume)^2)/n #calculate mse
  treeMSEadj[m] <- sum((mPredFit$fitted.values - trees$Volume)^2)/(n-1-m)
  #predict via cross-validation
  allPredictedCV <- rep(0, n)
  for (i in 1:k){
    groupi <- (cvgroups == i)
    lmfitCV <- lm(formula = allModels[[m]], data = trees, subset=!groupi)
    allPredictedCV[groupi] = predict.lm(lmfitCV, trees[groupi,])
  }
  treeCV[m] <- sum((allPredictedCV - trees$Volume)^2)/n
}
cbind(treeMSE, treeMSEadj, treeCV) #mseAdj we're dividing by a smaller number.
#We'll see increase in allmodelCV because we've removed 


#Use ISLR package from the Auto data set.
library(ISLR)
data(Auto)
head(Auto)
attach(Auto)
#Use 10-fold cross validation for 1 KNN regression
#REMEMBER TO STANDARDIZE EACH TRAINING SET.
weight.std <- scale(weight)
year.std <- scale(year)


library(FNN)
m <- 10
groups <- c(rep(1:10, 39), 1, 2)
n <- dim(Auto)[1]; n
set.seed(2)
cvgroups <- sample(groups, n); cvgroups
autoPredictions <- rep(NA, n)
x.std <- cbind(weight.std, year.std)
y <- mpg

for (fold in 1:m) {
  train.x = x.std[cvgroups != fold,]
  train.x.std = scale(train.x)
  train.y = y[cvgroups != fold]
  valid.x = x.std[cvgroups == fold,]
  valid.x.std = scale(valid.x, 
                      center = attr(train.x.std, "scaled:center"), 
                      scale = attr(train.x.std, "scaled:scale"))
  predictions = knn.reg(train.x.std, valid.x.std, train.y, k = 1)
  autoPredictions[cvgroups == fold] = predictions$pred
}
mean( (y - autoPredictions)^2 )



#Now run a for loop to calculate k values from 1 to 30. 
#Try loop on the outside...
library(FNN)
m <- 10
groups <- c(rep(1:10, 39), 1, 2)
n <- dim(Auto)[1]; n
set.seed(2)
cvgroups <- sample(groups, n); cvgroups
autoPredictions <- rep(NA, n)
x.std <- cbind(weight.std, year.std)
y <- mpg
K<-1
overallCV <- numeric()
for(K in 1:30){
  for (fold in 1:m) { #Calculates MSE 
    train.x = x.std[cvgroups != fold,]
    train.x.std = scale(train.x)
    train.y = y[cvgroups != fold]
    valid.x = x.std[cvgroups == fold,]
    valid.x.std = scale(valid.x, 
                        center = attr(train.x.std, "scaled:center"), 
                        scale = attr(train.x.std, "scaled:scale"))
    predictions = knn.reg(train.x.std, valid.x.std, train.y, k = K)
    autoPredictions[cvgroups == fold] = predictions$pred
    
  }
  overallCV[K] <- mean( (y - autoPredictions)^2 )
}
Kgraph <- seq(1, 30, by = 1)
plot(Kgraph, overallCV, type = "l", lwd = 2, main = "CV by K groups", ylab = "CV Value", xlab = "K groups")



#Use for loop to test values of K from 1 to 20.
K = seq(1, 20, by = 2)
overall = numeric(length(K))
for (i in 1:length(K)){
  predictions <- knn.reg(train.x, valid.x, mpg[train], k = K[i])
  myTable <- table(predictions$pred, mpg[-train])
  overall[i] <- (myTable[1,2] + myTable[2,1])/136
}
plot(K, overall, type="l", lwd=2)
myTable
predictions



#Puts the loop on the inside
m <- 10
groups <- c(rep(1:10, 39), 1, 2)
n <- dim(Auto)[1]; n
set.seed(2)
cvgroups <- sample(groups, n); cvgroups
autoPredictions <- rep(NA, n)
x.std <- cbind(weight.std, year.std)
y <- mpg
overall <- numeric(length(K))
autoGraph <- matrix(ncol = K, nrow = n)
i<-1
for (fold in 1:m) { #First part pulls training data out...
  K <- seq(1, 30, by = 1) #Want this to reset at each go around.
  train.x = x.std[cvgroups != fold,]
  train.x.std = scale(train.x)
  train.y = y[cvgroups != fold]
  valid.x = x.std[cvgroups == fold,]
  valid.x.std = scale(valid.x, 
                      center = attr(train.x.std, "scaled:center"), 
                      scale = attr(train.x.std, "scaled:scale"))
  
  for (i in 1:length(K)){
    #This will take KNN
    predictions = knn.reg(train.x.std, valid.x.std, train.y, k = K[i])
    autoPredictions[cvgroups == fold] = predictions$pred #stores prediction for current fold
    #This will fill in matrix of autograph.
    #Now to compute the error at each point... 
  }
  autoGraph[,fold] <- autoPredictions #Store the values of overall to new column. 
}
mpg[cvgroups == fold]
autoPredictions
predictions$pred
mpg[-train.x,]
autoPredictions[cvgroups == fold] = predictions$pred
?knn.reg
myTable
overall


