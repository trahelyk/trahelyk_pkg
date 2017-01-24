require(gmodels)

# split a data frame into a training set and a test set
train_test <- function(x, frac, seed) {
  set.seed(seed)
  x$guid <- seq(1:nrow(x))
  train <- sample_frac(x, size=frac, replace=FALSE)
  test <- x[!(x$guid %in% train$guid),]
  train$guid <- test$guid <- NULL
  return(list(train=train,test=test))
}

# Calcualte classificaiton test error rate
test_error <- function(yhat,observed) {
  CrossTable(yhat, observed, 
             prop.chisq = FALSE, prop.t = FALSE,
             dnn = c("Prediction", "observed"))
  crosstab <- table(yhat, observed)
  return((crosstab[1,1] + crosstab[2,2])/sum(crosstab))
}

# Calcualte MSE
mse <- function(yhat, ybar) {
  return(mean((yhat-ybar)^2))  
}
