#EML Assignment 3: Unsupervised Learning and Regularization

#set seed for reproducible results
set.seed(1)

# libraries
library(boot)
library(glmnet)

#function for adding labels to coefficient plots
#taken from: https://stackoverflow.com/questions/30560689/adding-labels-on-curves-in-glmnet-plot-in-r
lbs_fun <- function(fit, offset_x=1, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])+ offset_x
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, ...)
}

#@@@ Q1 @@@
# Load the data
prostate = load("prostate.Rdata")
summary(prostate)

#construct test and train sets
train_X = model.matrix(lpsa~., prostate.train)[ ,1:8]
train_Y = as.matrix(prostate.train[,9])
test_X = model.matrix(lpsa~., prostate.test)[ ,1:8]
test_Y = as.matrix(prostate.test[,9])

#@@@ Q2 @@@
# Fit ridge regression models and generate a plot
srr_fit = glmnet(train_X, train_Y, alpha=0)

#plot coefficients vs. lambda
par(mfrow=c(1,1))
plot(srr_fit, xvar="lambda", label=T)
lbs_fun(srr_fit)

#@@@ Q3 @@@
#fit a ridge regression to the training data using 10-fold CV
cvrr_fit = cv.glmnet(x=train_X, y=train_Y, alpha=0, nfolds=10)
#get the lambda value that produces the best model
best_lambda_cvrr = cvrr_fit$lambda.min
best_lambda_cvrr

#calculate the train and test mse of the ridge regression model with the best lambda value
#train mse
srr_train_pred = predict(srr_fit, s=best_lambda_cvrr, newx=train_X)
srr_mse = mean( (train_Y - srr_train_pred)^2 )
srr_mse
#test mse
srr_test_pred = predict(srr_fit, s=best_lambda_cvrr, newx=test_X)
srr_mspe = mean( (test_Y - srr_test_pred)^2 )
srr_mspe

#which variables are used?
ridge_coef = predict(srr_fit, type="coefficients", s=best_lambda_cvrr)
ridge_coef

#@@@ Q4 @@@
# Fit lasso regression models and generate a plot
slr_fit = glmnet(train_X, train_Y, alpha=1)

#plot coefficients vs. lambda
par(mfrow=c(1,1))
plot(slr_fit, xvar="lambda", label=T)
lbs_fun(slr_fit)

#@@@ Q5 @@@
#fit a lasso to the training data using 10-fold CV

cvlr_fit = cv.glmnet(x=train_X, y=train_Y, alpha=1, nfolds=10)
#get the lambda value that produces the best model
best_lambda_cvlr = cvlr_fit$lambda.min
best_lambda_cvlr

#calculate the train and test mse of the lasso regression model with the best lambda value
#train mse
slr_train_pred = predict(slr_fit, s=best_lambda_cvlr, newx=train_X)
slr_mse = mean( (train_Y - slr_train_pred)^2 )
slr_mse
#test mse
slr_test_pred = predict(slr_fit, s=best_lambda_cvlr, newx=test_X)
slr_mspe = mean( (test_Y - slr_test_pred)^2 )
slr_mspe

#which variables are used?
lasso_coef = predict(slr_fit, type="coefficients", s=best_lambda_cvlr)
lasso_coef
