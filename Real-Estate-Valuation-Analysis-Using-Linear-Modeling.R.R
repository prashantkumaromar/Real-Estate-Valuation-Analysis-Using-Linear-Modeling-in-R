# Project 2 Code

#2.

#first step: save data to csv file mydata.csv

#then:
library(readxl)
install.packages("xlsx")
dataset2 <- read_excel("Real estate valuation data set.xlsx")
colnames(dataset2)[colnames(dataset2) == "X1 transaction date"] <- "X1"
colnames(dataset2)[colnames(dataset2) == "X2 house age"] <- "X2"
colnames(dataset2)[colnames(dataset2) == "X3 distance to the nearest MRT station"] <- "X3"
colnames(dataset2)[colnames(dataset2) == "X4 number of convenience stores"] <- "X4"
colnames(dataset2)[colnames(dataset2) == "X5 latitude"] <- "X5"
colnames(dataset2)[colnames(dataset2) == "X6 longitude"] <- "X6"
colnames(dataset2)[colnames(dataset2) == "Y house price of unit area"] <- "Y"
dim(dataset2)
dataset_n1 <- dataset2[c('X1', 'X2', 'X3', 'X4', 'X5','X6','Y')]
plot(dataset_n1, col = 'blue')
x5x6 <- lm(X5 ~ X6, data = dataset_n1)
summary(x5x6)
library(corrplot)
datasetcorr <- dataset_n1[c('X1', 'X2', 'X3', 'X4', 'X5','X6','Y')]
datasetcorr2 <- cor(datasetcorr)
corrplot(datasetcorr2, method = 'number')

dataset_n1 <- dataset2[c('X1', 'X2', 'X3', 'X4', 'X5','X6','Y')]
plot(dataset_n1, col = 'blue')

# X6,X5,X4 appear to have a somewhat linear relationship. The other variables do not seem to have a linear relationship with Y.


# split data into training and test sets:

# now we check to see to see that Y is normally distributed before moving further:

hist(dataset_n1$Y)
# Y appears normally distributed

# now we can make a linear model with Y as the response and check the significance of the coefficients:

linearmodel <- lm(Y ~ X3, data = dataset_n1)
summary(linearmodel)
confint(linearmodel)
predict (linearmodel , data.frame(X3 = (c(4, 8, 12))),
         interval = "confidence")

predict (linearmodel , data.frame(X3 = (c(4, 8, 12))),
         interval = "prediction")
plot(X3,Y)

par(mfrow=c(2,2))
plot(linearmodel)
plot ( predict (linearmodel), residuals (linearmodel))
plot ( predict (linearmodel), rstudent (linearmodel))
plot ( hatvalues (linearmodel))

# The residuals plot shows  the model has linear relationship and the residuals. Also, the QQ plot shows that the theoretical residuals 
# correlate well with the residuals from our model. These graphs indicate that our model is probably acceptable.

# Cross Validate:

# CV method 1: LOOCV

glm1 <- glm(Y ~ X3, data = dataset_n1)
coef(glm1)

library(boot)

cv.error <- rep(0, 10)
for (i in 1:10) {
  glm1 <- glm(Y ~ poly(X3, i), data = dataset_n1)
  cv.error[i] <- cv.glm(dataset_n1, glm1)$delta[1]
}
cv.error

# The LOOCV produced an CV error of  81.15789.

# CV Method 2 - K- fold, 5 folds:

glm2 <- glm(Y ~ X3, data = dataset_n1)
coef(glm2)

library(boot)

cv.error <- rep(0, 5)
for (i in 1:5) {
  glm2 <- glm(Y ~ poly(X3, i), data = dataset_n1)
  cv.error[i] <- cv.glm(dataset_n1, glm2, K = 5)$delta[1]
}
cv.error

# The k fold = 5 linear model has a CV error of 81.44828, slightly more than LOOCV but less than validation set method.

# CV method 3: K-fold, 10 folds:

glm2 <- glm(Y ~ X5, data = dataset_n1)
coef(glm2)

library(boot)

cv.error <- rep(0, 10)
for (i in 1:10) {
  glm2 <- glm(Y ~ poly(X3, i), data = dataset_n1)
  cv.error[i] <- cv.glm(dataset_n1, glm2, K = 10)$delta[1]
}
cv.error

# The CV error is lowest for the K= 10 linear model and is equal to 81.56777, which is almost the exact same as that produced by LOOCV.

# Since the CV error is lowest for the linear model, we will proceed with just X3 as a predictor for the basic linear model.

# Now, build a model:

# Method 1: Plain Linear Regression
set.seed (1)
data_train <- sample (c(TRUE , FALSE), nrow (dataset_n1),
                   replace = TRUE)
data_test <- (!train)

#  Ridge Regression:


x=model.matrix(Y~ ., data= dataset_n1)[,-1]
y=dataset_n1$Y
library(glmnet)
grid <- 10^ seq (10, -2, length = 100)
ridge_model <- glmnet (x, y, alpha = 0, lambda = grid)
dim ( coef (ridge_model))
# coefficient plot

plot(ridge_model,xvar="lambda",label=T)
ridge_model$lambda[50]
coef(ridge_model)[,50]
sqrt(sum(coef(ridge_model)[-1,50]^2))
predict(ridge_model,s=50, type = "coefficients")[1:7,]
set.seed(1)
train=sample (1: nrow(x), nrow(x)/2)
test=(-train )
y_test=y[test]
ridge_model = glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge_pred = predict(ridge_model ,s=4, newx=x[test, ])
mean((ridge_pred -y_test)^2)
mean((mean(y[train])-y_test)^2)
set.seed(1)
cv= cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv)
bestlam =cv$lambda.min
bestlam
ridge_pred=predict (ridge_model ,s=bestlam ,newx=x[test, ])
mean((ridge_pred - y_test)^2)

# MSE for ridge = 62.25089


# Method 4: Lasso
# Cross validate to find lambda:
lasso_model = glmnet(x[train, ], y[train], alpha = 1, lambda= grid)
plot(lasso_model)

# Best lambda = 0.3862175

set.seed(1)
cv = cv.glmnet(x[train, ],y[train], alpha =1)
plot(cv)

bestlam = cv$lambda.min
lasso_pred = predict(lasso_model, s=bestlam, newx=x[test, ])
mean((lasso_pred -y_test)^2)
# MSE = 62.33709

# Method 5: PCR

library(pls)

pcr.fit=pcr(Y~., data=dataset_n1, scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type = "MSEP")
summary(pcr.fit)
# the lowest cv error is found at M = 3
# so:

pcr.pred=predict(pcr.fit, x[test, ], ncomp=3)
mean((pcr.pred - y_test)^2)

# MSE is 63.98791

# Method 6: PLS

set.seed(1)
pls.fit=plsr(Y~., data=dataset_n1 ,subset =train ,scale=TRUE ,
             validation ="CV")
summary(pls.fit )

# lowest cv error is found at M = 4.
# so:

pls.pred=predict(pls.fit, x[test, ], ncomp =4)
mean((pls.pred -y_test)^2)

# The MSE is 62.8356.

