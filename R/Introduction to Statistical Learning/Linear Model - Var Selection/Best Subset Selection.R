library(ISLR)
fix(Hitters)
names(Hitters)

#Remove NA's 
hitters <- na.omit(Hitters)
#Check 
sum(is.na(hitters)) #Should be 0.

library(leaps)
#Leaps used to perform best subset selection
regfit.full = regsubsets(Salary~., data = hitters)
summary(regfit.full)

#Plotting
reg.summary = summary(regfit.full)
par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",
       type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ",
       ylab="Adjusted RSq",type="l")


which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)

plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type="l")
which.min(reg.summary$cp ) [1] 10
points(10,reg.summary$cp [10],col="red",cex=2,pch=20)
which.min(reg.summary$bic )
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",
       type="l")
points(6,reg.summary$bic [6],col="red",cex=2,pch=20)

#Get coefs of best model
coef(regfit.full ,6)

#Choosing among models using the validation set approach + cross val. 
#In order to get accurate estimates, we need to use only the training observations

#Split the data intro training and test set
set.seed(1)
train = sample(c(TRUE, FALSE), nrow(hitters), rep = TRUE)
test = !(train)

#Perform regsubsets() on the training set in order to perform best subset selection
regfit.best = regsubsets(Salary~., data = hitters[train,], nvmax = 19)

#Make a model matrix of the test data
test.mat = model.matrix(Salary~., data = hitters[test,])

#Loop: for each size i, we get the coefficients from regfit.best to get the best model
#of that size i. Multiply them with the appropriate columns of the test model matrix
#to get the predictions and compute mean squared error
val.errors = rep(NA, 19)
for (i in 1:19) {
  coefi = coef(regfit.best, id = i)
  pred = test.mat[,names(coefi)] %*% coefi
  val.errors[i] = mean((hitters$Salary[test]-pred) ^2)
}

#find min error
which.min(val.errors)
coef(regfit.best,10)




#New predict method for regsubsets
predict.regsubsets = function(object, newdata, id,...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[,xvars] %*% coefi
}


#Using cross val
k = 10 #num of folds.
set.seed(1)
folds = sample(1:k, nrow(hitters), replace = TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))

for(j in 1:k){
    best.fit = regsubsets(Salary~., data = hitters[folds!=j,], nvmax = 19)
    for (i in 1:19) {
      pred=predict(best.fit,hitters[folds==j,],id=i)
      cv.errors[j,i]=mean( (hitters$Salary[folds==j]-pred)^2)
    }
}

#Use apply() function to average over the columns of this matrix in order to obtain a vector
#for which the jth element is the cross validation error for the j variable model
mean.cv.errors = apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow = c(1,1))
plot(mean.cv.errors, type = "b")

#Cross validation selects the 11 variable model 
#Perform best subset selection on the full data set in otrder to obtain the 11 variable model
reg.best = regsubsets(Salary~., data = hitters, nvmax = 19)
coef(reg.best,11)
