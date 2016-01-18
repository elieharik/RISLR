library(ISLR)
attach(Wage)

#Analyzing wage data 

#1) Using Polynomial regression + Step Functions
fit = lm(wage~poly(age,4), data = Wage)
#Poly returns matrix whose columns are a inear combination of variables
#age, age^2, age^3...
coef(summary(fit))

#Alternatively:
fit2 = lm(wage~poly(age,4,raw = TRUE), data = Wage)
coef(summary(fit2))

#Alternatively again
fit2a = lm(wage~age + I(age^2) + I(age^3) + I(age^3) + I(age^4), data = Wage)
coef(fit2a)
#Don't forget to use the wrapper function I()

#Alternatively I(again^2)
fit2b = lm(wage~cbind(age+age^2+age^3+age^4))

#Create new values of age at which we want to make predictions
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2 * preds$se)

#Plot the data
#mar / oma arguments allow you to change margins of the plot
par(mfrow = c(1,2), mar = c(4.5,4.5,1,1), oma = c(0,0,4,0))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree -4 Poly", outer = T) #outer = T -> outer Title
lines(age.grid, preds$fit, lwd = 2, col = "blue")
#Plt the columns of se.bands against the columns of age.grid
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3) 

#When performing poly regression, we need to decide on the deg of poly to use.
#Use hyp tests.
#Fit models ranging from linear to 5 deg polys and try to find simplest model

#Perform ANOVA to test H0 = "Simpler model M1 is sufficient to explain data against
# more complex model M2". Note: Preds in M1 should be subset of preds in M2
#Let's fit the different models
fit.1 = lm(wage~age, data = Wage)
fit.2 = lm(wage~poly(age,2), data = Wage)
fit.3 = lm(wage~poly(age,3), data = Wage)
fit.4 = lm(wage~poly(age,4), data = Wage)
fit.5 = lm(wage~poly(age,5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
#Cubic or Quadratic models give a good fit (Quadratic so so a la limite
 # if have crit val of .05)
#Alternatively
coef(summary(fit.5)) #note that squares of t vals = F val
#anova() more flexible because can compare models with different predictors
#We could also use Cross Val to compare models.

#Want to predict whether an individual earns more than 250k a year
#Need to create the appropriate response var
# + apply glm() function using family = "binom" to fit poly log reg model
fit = glm(I(wage>250)~poly(age,4), data = Wage, family = binomial)
preds = predict(fit, newdata = list(age = age.grid), se = T)

#Need to calculate the CI's
#Have to fit a model of type e^alpha / (1+e^alpha)
pfit = exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands = exp(se.bands.logit) / (1+exp(se.bands.logit))

#Could have directly computed the probabilities using the type = response option
preds = predict(fit, newdata = list(age = age.grid), type = "response", se =T)

#Plot it
plot(age, I(wage>250), xlim = agelims, type = "n", ylim = c(0,.2))
#Show age of high earners and low earners. 
points(jitter(age), I((wage>250)/5), cex = .5, pch = "|", col = "darkgrey")
#Solid blue curve indicates fitted proba of being high earner. 
lines(age.grid, pfit, lwd = 2, col = "blue") #plot probabs.
#Estimated 95% confidence interval: 
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3) #Plot SE bands
#Reminder for plot: plotting binary event wage > 250 using log regression,
#with deg 4 poly. Shows posterior probability of wage exceeding 250k in blue
# + 95% CI