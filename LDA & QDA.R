#80% of companies issued dividens this year.
#Those offering divided distribution X is normal
  #Mean 10, SD of 6.
#For companies that DONT - X is normal, mean 0, SD 6. 

#Calculate posterior probability that a company will issue a divident this year.
#Given percentage profit was 2% last year.
x=2
4*exp((20*x-100)/72)/(1+(4*(exp((20*x-100)/72))))

1/(1+(4*(exp((20*x-100)/72))))

#Same criteria, not use log base e. 
library(MASS)
#mu is the mean, sigma is the SD, and pi is .8 for 80% of companies offer dividend.
mu1 = 10; sigma = 6; pi1 =.8
mu1/sigma^2; -mu1^2/(2*sigma^2)+log(pi1)

#When y=0, using pi = .2 since 20% of companies DO NOT offer dividents. 
#compute goal function.
mu0 = 0; sigma = 6; pi0=.2
mu0/sigma^2; -mu0^2/(2*sigma^2) + log(pi0)

d <- read.csv(file = "C:/Data Science Masters Program/DS 740 - Data Mining/Assignments/Week 4 - LDA QDA/Dividends.csv")
head(d)

library(pROC)
roc(d$y, d$x)
lda.y <- lda(d$y ~ d$x)
fitted.d <- predict(lda.y, data=d)$class
fitted.d
table(d$y, fitted.d)
d.error <- sum(d$y != fitted.d)/length(d$y); d.error

cle <- read.csv(file = "C:/Data Science Masters Program/DS 740 - Data Mining/Assignments/Week 4 - LDA QDA/Heart_Disease_Cleveland.csv")
head(cle)
heart <- cle[,c(1, 4, 5, 8, 10, 14)]
head(heart)

model1 <- lda(heart$DiseaseStatus ~ heart$MaxHeartRate + heart$STdepress)
model1
#classify
fitted.heart <- predict(model1, data=heart)$class
table(heart$DiseaseStatus, fitted.heart)

error.model1 <- sum(heart$DiseaseStatus != fitted.heart)/length(heart$DiseaseStatus); error.model1
#.429 error rate. 
head(heart)
head(cle)
model2 <- lda(heart$DiseaseStatus ~ ., data = heart)
fitted.heart2 <- predict(model2, data=heart)$class
table(heart$DiseaseStatus, fitted.heart2)
error.model2 <- sum(heart$DiseaseStatus != fitted.heart2)/length(heart$DiseaseStatus); error.model2

#Predict someone with HR of 135, STdepress of 2...
heartTest <- data.frame(ChestPain=factor(4), MaxHeartRate = 135, STdepress=2)
predict(fitted.heart,heartTest)$class

#Use Cross-Validation to choose between the models. 
n <- dim(heart)[1]; n
allPredictedCV <- rep("NA", n); allPredictedCV
cvk <- 10
groups <- c(rep(1:cvk, n/cvk, length.out=n)); groups
set.seed(4)
cvgroups <- sample(groups, n)
#calculate # of parameters - K is number of response options... so 5. p is # of predictors used.
K <- 5; p <- 2
K+K*p+p*(p+1)/2

for(i in 1:cvk){
  ldafit1i <- lda(heart$DiseaseStatus ~ MaxHeartRate + STdepress, data=heart, subset=(cvgroups!=i))
  newdatai <- data.frame(heart[cvgroups==i, ])
  allPredictedCV[cvgroups==i] = as.character(predict(ldafit1i, newdatai)$class)
}
table(heart$DiseaseStatus, allPredictedCV)
CVmodel1 <- sum(allPredictedCV!=heart$DiseaseStatus)/n; CVmodel1


allPredictedCV2 <- rep("NA", n); allPredictedCV
cvk <- 10
groups <- c(rep(1:cvk, n/cvk, length.out=n)); groups
set.seed(4)
cvgroups <- sample(groups, n)
#calculate # of parameters - K is number of response options... so 5. p is # of predictors used.
K <- 5; p <- 5
K+K*p+p*(p+1)/2

for(i in 1:cvk){
  ldafit2i <- lda(heart$DiseaseStatus ~ ., data=heart, subset=(cvgroups!=i))
  newdata2i <- data.frame(heart[cvgroups==i, ])
  allPredictedCV2[cvgroups==i] = as.character(predict(ldafit2i, newdata2i)$class)
}
table(heart$DiseaseStatus, allPredictedCV2)
CVmodel2 <- sum(allPredictedCV2!=heart$DiseaseStatus)/n; CVmodel2

#Create model 3. As quadratic discriminant analysis using predictors max HR and STdepress to predict DiseaseStatus.
model3 <- qda(heart$DiseaseStatus ~ heart$MaxHeartRate + heart$STdepress)
fitted.model3 <- predict(model3, data=heart)$class
sd(fitted.model3)
table(heart$DiseaseStatus, fitted.model3)
 
#Using QDA model fit, predict with max HR of 135 and ST depress of 2. 
#Predict someone with HR of 135, STdepress of 2...
heartTestQDA <- data.frame(ChestPain=factor(4), MaxHeartRate = 135, STdepress = 2)
predict(fitted.heart,heartTestQDA)$class

#Create model 4 using all 5 predictors.
model4 <- qda(heart$DiseaseStatus ~ ., data = heart)
fitted.model4 <- predict(model4, data=heart)$class

#Use 10 fold CV to check model 3 and 4...
#calculate # of parameters - K is number of response options... so 5. p is # of predictors used.
K <- 5; p <- 2
K+K*p+K*p*(p+1)/2



allPredictedCV4 <- rep("NA", n);
cvk <- 10
groups <- c(rep(1:cvk, n/cvk, length.out=n)); groups
set.seed(4)
cvgroups <- sample(groups, n)
#calculate # of parameters - K is number of response options... so 5. p is # of predictors used.
K <- 5; p <- 5
K+K*p+K*p*(p+1)/2

allpredictedCV3 = allpredictedCV4 = factor(rep(NA,n),levels=c("0","1","2","3","4"))
for (i in 1:n)  {
  qdafit3 = qda(DiseaseStatus ~ MaxHeartRate + STdepress, data=heart, subset=(cvgroups!=i))
  newdata3 = heart[cvgroups==i,c(4,5)]
  allpredictedCV3[cvgroups==i] = predict(qdafit3,newdata3)$class
  qdafit4 = qda(DiseaseStatus~., data=heart, subset=(cvgroups!=i))
  newdata4 = data.frame(heart[cvgroups==i,-6])
  allpredictedCV4[cvgroups==i] = predict(qdafit4,newdata4)$class
}
CVmodel3 = sum(allpredictedCV3!= heart$DiseaseStatus)/n; CVmodel3
CVmodel4 = sum(allpredictedCV4!= heart$DiseaseStatus)/n; CVmodel4



#----------------Homework Week 4--------------
library(ISLR)

head(Auto)
attach(Auto)
auto <- Auto #so i can keep integrity of original frame.

#Define var domestic with value of 1 when car is domestic
#0 when car is foreign (which is 2 or 3 in Auto).
#report count of domestic vehicles. 
auto$Domestic[which(auto$origin==1)] = 1
auto$Domestic[which(auto$origin!=1)] = 0
auto$Domestic
sum(auto$Domestic)

boxplot(auto$mpg ~ auto$Domestic, main = "Distribution of mpg in International (0) and Domestic (1) Vehicles", ylab = "Miles Per Gallon")
mean(mpg[which(auto$Domestic==1)])
mean(mpg[which(auto$Domestic==0)])

#Predict the classifications from LDA fit.
#Formulate Domestic with LDA classification
#report # of correctly classified domestic vehicles.

ldafit <- lda(auto$Domestic ~ auto$mpg)
pred.ldafit <- predict(ldafit, data=heart)$class
table(auto$Domestic, pred.ldafit)

#Plot the other 6 predictors (cyl, disp, hp, weight, acc, yr)
#Which one is most discriminating? 
head(auto)
xvars <- cbind(auto$cylinders, auto$displacement, auto$horsepower, auto$weight, auto$acceleration, auto$year)
xDomestic <- xvars[auto$Domestic == 1]
xInternational <- xvars[auto$Domestic == 0]

par(mfrow=c(3,2))
boxplot(auto$cylinders ~ auto$Domestic, main = "Cylinders")
boxplot(auto$displacement ~ auto$Domestic, main = "Displacement")
boxplot(auto$horsepower ~ auto$Domestic, main = "Horsepower")
boxplot(auto$weight ~ auto$Domestic, main = "Weight")
boxplot(auto$acceleration ~ auto$Domestic, main = "Acceleration")
boxplot(auto$year ~ auto$Domestic, main = "Year")

par(mfrow=c(1,1))
#Create a ROC curve
autoROC <- roc(auto$Domestic, auto$displacement)
plot(autoROC, main = "ROC Curve of Auto Displacement predicting Car Origin")
library(ggplot2)

#Make a scatterplot of mpg and displacement - marked by origin.
c(1, 0)[unclass(auto$Domestic)]
plot(auto$displacement, auto$mpg, pch=21, bg=c("red", "blue")[unclass(auto$Domestic)], main = "Auto MPG vs. Displacement, marked by Origin")
?theme
ggplot(auto, x = auto$displacement, y = auto$mpg, size = .5) + geom_point(aes(color = factor(auto$Domestic), x = auto$displacement, y = auto$mpg)) + labs(x = "Displacement", y = "MPG", title = "How is displacement and MPG related in domestic and foreign cars?") + theme(legend.position = "none")# +scale_fill_discrete(name = "Car Origin", breaks = c(0,1), labels = c("Foreign", "Domestic"))


#1 is American, 2 is european, #3 is japanese. Get avg MPG. 
mean(auto$mpg[which(auto$origin==1)]) #American
mean(auto$mpg[which(auto$origin==2)]) #Euro
mean(auto$mpg[which(auto$origin==3)]) #Japanese
auto$origin
mean(mpg[which(auto$Domestic==0)])



#Fit LDA using mpg, cylinders, displacement, hp and weight to predict origin.
#Report mpg
head(auto)

fullFitModel <- lda(origin ~ mpg + cylinders + displacement + horsepower + weight, data = auto)
pred.fullFitModel <- predict(fullFitModel, data=auto)$class
table(auto$origin, pred.fullFitModel)

#predict LDA Fit for vehicle with...
#20 mpg, 8 cyl, dis of 320, 280 hp, 3600 lbs.
carFrame <- data.frame(mpg=20, cylinders=8, displacement=100, horsepower=280, weight=3600)
pred.carFrame <- predict(fullFitModel, carFrame)$class
predict(fullFitModel, carFrame) #Came back as japanese?
error.lda.cars <- sum(auto$origin != pred.fullFitModel)/length(auto$origin); error.lda.cars
#error rate of .7984694


#Predict classifications from QDA fit.
qdaCars <- qda(origin ~ mpg + cylinders + displacement + horsepower + weight, data = auto)
pred.qdaCars <- predict(qdaCars, data = auto)$class
table(auto$origin, pred.qdaCars)
error.qda.cars <- sum(auto$origin != pred.qdaCars)/length(auto$origin); error.qda.cars
#Error rate of .2244898
#Predict whether vehicle is American, Euro or Japanese
#same as above... carFrame still works.
predict(qdaCars, carFrame)$class

#--- Use CV for model prediction----
#1. LDA - displacement
#2. LDA - mpg & displacement
#3. LDA - mpg, cylinders, displacement, hp & weight.
#4. QDA - displacement
#5. QDA - mpg & displacement.
#6. QDA - mpg, cylinders, displacement, hp & weight.

#Determine the number of parameters for each of the above.
#LDA = K+K*p+p*(p+1)/2
#calculate # of parameters - K is number of response options... so 5. p is # of predictors used.
#Model 1 - 3 response options for origin.
k1 <- 3; p1 <- 1 #1 predictor
k1+k1*p1+p1*(p1+1)/2
#Model 2 - 3 & 2.
k2 <- 3; p2 <- 2
k2+k2*p2+p2*(p2+1)/2
#Model 3 - 3 and 5
k3 <- 3; p3 <- 5
k3+k3*p3+p3*(p3+1)/2

#QDA = K+K*p+K*p*(p+1)/2
#Model 4 - 3 and 1...
k4 <- 3; p4 <- 1
k4+k4*p4+k4*p4*(p4+1)/2
#Model 5 - 3 and 2
k5 <- 3; p5 <- 2
k5+k5*p5+k5*p5*(p5+1)/2
#model 6 - 3 and 5
k6 <- 3; p6 <- 5
k6+k6*p6+k6*p6*(p6+1)/2

#Set seed to 4, define cv groups using sample().
#Build Model 1.
require(MASS)
n <- dim(auto)[1]; n #392
allpredictedCV1 <- rep("NA", n)
k <- 10
groups <- c(rep(1:10, 39), 1, 2)
set.seed(4)
cvgroups <- sample(groups, n)

for (i in 1:k){
  ldaModel1 <- lda(origin ~ displacement, data = auto, subset = (cvgroups!=i))
  newData1 <- data.frame(auto[cvgroups==i, ])
  allpredictedCV1[cvgroups==i] <- as.character(predict(ldaModel1, newData1)$class) 
}
table(auto$origin, allpredictedCV1)
CVModel1 <- sum(allpredictedCV1!=auto$origin)/n; CVModel1

#Set seed to 4, define cv groups using sample().
#Build Model 2.
n <- dim(auto)[1]; n #392
allpredictedCV2 <- rep("NA", n)
k <- 10
groups <- c(rep(1:10, 39), 1, 2)
set.seed(4)
cvgroups <- sample(groups, n)

for (i in 1:k){
  ldaModel2 <- lda(origin ~ displacement + mpg, data = auto, subset = (cvgroups!=i))
  newData2 <- data.frame(auto[cvgroups==i, ])
  allpredictedCV2[cvgroups==i] <- as.character(predict(ldaModel2, newData2)$class) 
}
table(auto$origin, allpredictedCV2)
CVModel2 <- sum(allpredictedCV2!=auto$origin)/n; CVModel2

#Set seed to 4, define cv groups using sample().
#Build Model 3.
n <- dim(auto)[1]; n #392
allpredictedCV3 <- rep("NA", n)
k <- 10
groups <- c(rep(1:10, 39), 1, 2)
set.seed(4)
cvgroups <- sample(groups, n)

for (i in 1:k){
  ldaModel3 <- lda(origin ~ displacement + mpg + cylinders + horsepower + weight, data = auto, subset = (cvgroups!=i))
  newData3 <- data.frame(auto[cvgroups==i, ])
  allpredictedCV3[cvgroups==i] <- as.character(predict(ldaModel3, newData3)$class) 
}
table(auto$origin, allpredictedCV3)
CVModel3 <- sum(allpredictedCV3!=auto$origin)/n; CVModel3

#Build models 4, 5 and 6 using QDA.
#4. QDA - displacement
#5. QDA - mpg & displacement.
#6. QDA - mpg, cylinders, displacement, hp & weight.
n <- dim(auto)[1]; n #392
allpredictedCV4 = allpredictedCV5 = allpredictedCV6 = factor(rep(NA, n), levels=c("1", "2", "3"))
k <- 10
groups <- c(rep(1:10, 39), 1, 2)
set.seed(4)
cvgroups <- sample(groups, n)
head(auto) #mpg=1, disp = 3, cyl = 2, hp = 4, weight = 5
for (i in 1:k){
  qdaModel4 <- qda(origin ~ displacement, data=auto, subset = (cvgroups!=i))
  newdata4 <- auto[cvgroups==i, auto$origin]
  allpredictedCV4[cvgroups==i] <- predict(qdaModel4, newdata4)$class
  qdaModel5 <- qda(origin ~ displacement + mpg, data=auto, subset = (cvgroups!=i))
  newdata5 <- auto[cvgroups==i, c(3, 1)]
  allpredictedCV5[cvgroups==i] <- predict(qdaModel5, newdata5)$class
  qdaModel6 <- qda(origin ~ displacement + mpg + horsepower + cylinders + weight, data=auto, subset = (cvgroups!=i))
  newdata6 <- auto[cvgroups==i, c(3,1,4,2,5)]
  allpredictedCV6[cvgroups==i] <- predict(qdaModel6, newdata6)$class
}
CVModel4 <- sum(allpredictedCV4!=auto$origin)/n; CVModel4
CVModel5 <- sum(allpredictedCV5!=auto$origin)/n; CVModel5
CVModel6 <- sum(allpredictedCV6!=auto$origin)/n; CVModel6

allPredictedCV4 <- rep("NA", n);
cvk <- 10
groups <- c(rep(1:cvk, n/cvk, length.out=n)); groups
set.seed(4)
cvgroups <- sample(groups, n)

#Check the assumption of multivariate normality for model 6.
library(MVN)
#use hzTest to test for multivariate normality. 
xvar
xvar <- cbind(auto$mpg, auto$displacement, auto$cylinders, auto$horsepower, auto$weight)
xAmerican <- xvar[auto$origin == 1,]
#xAmerican
xEuro <- xvar[auto$origin == 2,]
xJapanese <- xvar[auto$origin == 3,]
hzTest(xAmerican)
hzTest(xEuro)
hzTest(xJapanese)

install.packages("biotools")
library(biotools)
boxM(xvar, auto$origin)
