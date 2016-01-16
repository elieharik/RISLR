#Use the glmnet package in order to perform ridge regression and the lasso 
#Using main function in package: glmnet()
#Need to pass x matrix and y vector (different than lm where we are not passing (y~x))

#Use Ridge / Lasso to predict Salary on the Hitters data
hitters <- na.omit(Hitters)
x = model.matrix(Salary~., hitters)[,-1]
y = hitters$Salary 
#Use the model.matrix() function to create x. Produces a matrix corresponding to the 19 predictors + 
# automatically transforms any qualitative variables into dummy variables (glmnet() can only take quantitative inputs!!)
#If alpha = 0 ridge, if alpha = 1 lasso
library(glmnet)
grid = 10^seq(10, -2, length = 100)
ridge.mod = glmnet(x,y,alpha = 0, lambda = grid)

#Coefs
ridge.mod$lambda[50]
coef(ridge.mod)[,50]

#predict function
predict(ridge.mod, s = 50, type = "coefficients")[1:20,]

#Split the samples into a trainign set and test set 
#Estimate the test error of ridge regressin

#Produce a random vector of TRUE FALSE and select observations corresponding to TRUE for training set
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test= -train
y.test = y[test]

#Fit a regression model on the trainign set and evaluate its MSE on test set, with lambda = 4
ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred = predict(ridge.mod, s = 4, newx = x[test,])
mean((ridge.pred-y.test)^2) #test MSE

#Note: Use cross validation to choose the tuning parameter lambda
#(function: cv.glmnet() performs 10 fold cross validation by default)