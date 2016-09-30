
BR <- read.csv(file = "C:/Data Science Masters Program/DS 740 - Data Mining/Assignments/Week 3 - Regression/BadRegression.csv")
head(BR)
library(pROC)
myROC <- roc(response = BR$y, predictor = BR$predictvals, direction = ">")
plot.roc(myROC)

boxplot(BR$predictvals ~ BR$y)

myROC <- roc(response = BR$y, predictor = BR$predictvals, direction = "<")
plot.roc(myROC)

heart <- read.csv(file = "C:/Data Science Masters Program/DS 740 - Data Mining/Assignments/Week 3 - Regression/Heart_disease_Cleveland.csv")
head(HDC)
summary(heart) #Sex - male = 1, female = 0.

heart$Slope <- as.factor(heart$Slope)
heart$Sex <- as.factor(heart$Sex)
heart$ChestPain <- as.factor(heart$ChestPain)
heart$HighBloodSugar <- as.factor(heart$HighBloodSugar)
heart$ECG <- as.factor(heart$ECG)
heart$ExerAngina <- as.factor(heart$ExerAngina)
heart$Thal <- as.factor(heart$Thal)

heart$HD <- rep(0, length(heart$DiseaseStatus))
heart$HD[which(heart$DiseaseStatus>0)] = 1

fit <- glm(HD~., data=heart, family = "binomial")

heart = heart[which(complete.cases(heart)), ] # remove 6 missing values
model1 <- glm(HD~. - DiseaseStatus, data = heart, family = "binomial")
model2 <- glm(HD ~ BloodPressure + Chol + Thal, data = heart, family = "binomial")

myroc = roc(response = heart$HD, predictor = model1$fitted.values)
myroc2 = roc(response = heart$HD, predictor = model2$fitted.values)
plot.roc(myroc, col = "red", lty = 2)
plot.roc(myroc2, add = T)

fit2 <- glm(STdepress~.-HD, data=heart)
par(mfrow = c(2, 2))
plot(fit2) # where fit is what you called the regression model

hist(heart$STdepress)
fit3 <- lm(log(heart$STdepress+1)~.-HD, data = heart)
plot(fit3)
AIC(fit2); AIC(fit3)

#------HOMEWORK 3----------
WI <- read.csv(file = "C:/Data Science Masters Program/DS 740 - Data Mining/Assignments/Week 3 - Regression/Wisconsin_income.csv")
head(WI)
WI$CIT2 <- as.factor(WI$CIT2)
WI$COW <- as.factor(WI$COW)
WI$LANX <- as.factor(WI$LANX)
WI$MAR <- as.factor(WI$MAR)
WI$SEX <-as.factor(WI$SEX)
WI$DIS <- as.factor(WI$DIS)
WI$RAC <- as.factor(WI$RAC)
WI$Hispanic <- as.factor(WI$Hispanic)

#Make histogram of total earnings, usual hours, travel time.
#Which needs to be transformed? 
par(mfrow = c(1, 3))
hist(PERNP); hist(WKHP); hist(JWMNP)
#Earnings and Travel time appear to be skewed right.
ln.PERNP <- log(PERNP)
ln.JWMNP <- log(JWMNP)

#Perform regsubsets for best subset for lm for total earnings as a function of all other variables in data set. 
#Adding two transformed variables to existing DF.
WI$lnPERNP <- ln.PERNP
WI$lnJWMNP <- ln.JWMNP
library(leaps)
regfit.full <- regsubsets(lnPERNP ~. -PERNP -JWMNP, data=WI, method="seqrep", nvmax = 41)
plot(regfit.full)
summary(regfit.full)$bic

#Plot adj R^2 as a function of num variables.
#Find num of variables in best model by Adj R^2.
regfit.summary <- summary(regfit.full); regfit.summary

regfit.summary$adjr2
par(mfrow = c(1, 1))
plot(regfit.summary$adjr2, xlab = "# variables", ylab = "Adjusted R^2", type="l", lwd=2)
max(regfit.summary$adjr2)
min(regfit.summary$bic)
plot(regfit.summary$bic, xlab = "# variables", ylab = "BIC", type="l", lwd=2)

#Set seed to 3. Perform 10-fold CV.
#Choose best model size 1 to 41 based on X-validation MSE.
#Record MSE within each fold for each size of variables. 
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[ , xvars] %*% coefi
}

n <- length(RAC)
k <- 10 #10-fold cv
groups <- c(rep(1:k, floor(n/k)), 1:(n-floor(n/k)*k))

set.seed(3)
cvgroups <- sample(groups, n)
group.error <- matrix(nr=41, nc=k)

for (i in 1:k){
  groupi <- (cvgroups == i)
  cv.fit <- regsubsets(lnPERNP ~. -PERNP -JWMNP, data=WI[!groupi,], nvmax=41)

  for (j in 1:41){
    y.pred <- predict(cv.fit, newdata = WI[groupi, ], id=j)
    group.error[j,i] = mean((WI$lnPERNP[groupi]-y.pred)^2)
  }
}
group.error

MSE <- apply(group.error, 1, mean)
plot(MSE)
which.min(MSE)

se <- apply(group.error, 1, sd)/sqrt(k)
se[39] #.02066
which(MSE <= MSE[39] + se[39])


coef(regfit.full, 27)
#look at the most parsimonious model
coef(regfit.full, 6)
#y-hat = 9.534 -.557*COW6 -.246*MAR5 -.274*SEX2 +.0301*WKHP +.093*Education +.0806*lnJWMNP

plot(WI$lnPERNP)

#-- Auto Data
library(ISLR)
?Auto
attach(Auto)
head(Auto)

#Create binary var equal 1 for cars with mpg above median.
median(mpg)
MPGbinary <- rep(0, length(mpg))
MPGbinary[which(mpg>median(mpg))] = 1
MPGbinary <- as.factor(MPGbinary)
Auto$origin <- as.factor(Auto$origin)

#Create matrix of scatterplots of vars in auto.
#Check for collinearity.
pairs(Auto)

#Perform log regression of mpg.binary on other vars.
#Exclude mpg and name.
LRauto <- glm(MPGbinary ~ . -mpg -name, data=Auto, family="binomial")
library(car)
vif(LRauto)
LRauto2 <- glm(MPGbinary ~ . -mpg -name -displacement, data=Auto, family="binomial")




n <- length(MPGbinary)
k <- 10 #10-fold cv
groups <- c(rep(1:k, floor(n/k)), 1:(n-floor(n/k)*k))
Auto$MPGbinary <- MPGbinary
set.seed(3)
cvgroups <- sample(groups, n)
predictedvals <- vector()
Auto2 <- Auto[ ,-9]
head(Auto2)

for (i in 1:k){
  groupi <- (cvgroups == i)
  fit <- glm(MPGbinary ~ . -mpg -displacement, data=Auto2[!groupi,], family="binomial")
  predictedvals[groupi] <- predict(fit, Auto2[groupi,], type = "response")
}
length(mpg)
predictedvals
library(pROC)
head(Auto2)
myroc <- roc(response = Auto2$MPGbinary, predictor = predictedvals)
plot(myroc)

myroc = roc(response = Auto2$MPGbinary, predictor = model1$fitted.values)
myroc2 = roc(response = heart$HD, predictor = model2$fitted.values)
plot.roc(myroc, col = "red", lty = 2, main = "ROC Curve")
plot.roc(myroc2, add = T)




for (i in 1:k){
  groupi <- (cvgroups == i)
  cv.fit <- regsubsets(lnPERNP ~. -PERNP -JWMNP, data=WI[!groupi,], nvmax=41)
  
  for (j in 1:41){
    y.pred <- predict(cv.fit, newdata = WI[groupi, ], id=j)
    group.error[j,i] = mean((WI$lnPERNP[groupi]-y.pred)^2)
  }
}
