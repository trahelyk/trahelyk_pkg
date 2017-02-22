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
  return(mean((yhat-ybar)^2), na.rm=TRUE)  
}

# K-fold cross-validation (tested for lme objects so far)
kfold_cv <- function(mdl, idvar, k=5, seed=421159) {
  df <- mdl$data
  
  # Divide the dataset into k groups:
  set.seed(seed)
  foldnumbers <- c()
  df$k <- NA
  for(i in 1:(k-1)) {
    fold <- sample(unique(df[!(df$k %in% foldnumbers),idvar]),floor(length(unique(df[[idvar]]))/k))
    df$k[df[[idvar]] %in% fold] <- i
    foldnumbers <- c(foldnumbers, i)
  }
  df$k[is.na(df$k)] <- k
  
  # Refit the model and compute test error for each fold
  error <- c()
  for(i in 1:k) {
    err <- try(newmdl <- update(mdl, data=df[df$k!=i,]), silent=TRUE)
    if(class(err)=="try-error") warning(paste0("Fold ", i, " did not converge.")) else {
      mean(df[[as.character(mdl$terms[[2]])]], na.rm = TRUE)
      mean(predict(newmdl)[df$k==i], na.rm = TRUE)
      
      length(df[df$k==i,as.character(mdl$terms[[2]])])
      length(predict(newmdl)[df$k==i])
      
      error[i] <- mean((df[df$k==i,as.character(mdl$terms[[2]])] - predict(newmdl)[df$k==i])^2,
                       na.rm = TRUE)
    }
  }
  return(mean(error, na.rm=TRUE))
}

# K-fold cross-validation using parallel processing (tested for lme objects so far)
kfold_cv_par <- function(mdl, idvar, k=5, seed=421159) {
  df <- mdl$data
  
  # Divide the dataset into k groups:
  set.seed(seed)
  foldnumbers <- c()
  df$k <- NA
  for(i in 1:(k-1)) {
    fold <- sample(unique(df[!(df$k %in% foldnumbers),idvar]),floor(length(unique(df[[idvar]]))/k))
    df$k[df[[idvar]] %in% fold] <- i
    foldnumbers <- c(foldnumbers, i)
  }
  df$k[is.na(df$k)] <- k
  
  # Define nested function to refit the model and compute test error
  calc_err <- function(mdl, df, k) {
    err <- try(newmdl <- update(mdl, data=df[df$k!=k,]), silent=TRUE)
    if(class(err)=="try-error") warning(paste0("Fold ", k, " did not converge: ", err)) else {
      return(mean((df[df$k==k,as.character(mdl$terms[[2]])] - predict(newmdl)[df$k==k])^2,
                  na.rm = TRUE))
    }
  }
  
  # Call the function using parallel processing
  cores <- detectCores()
  cluster <- makeCluster(cores)
  clusterSetRNGStream(cluster, 421159)
  invisible(clusterEvalQ(cluster, {library(nlme); library(splines)}))
  tmp <- parLapply(cluster, 1:k, fun = calc_err,
                   mdl=mdl,
                   df=df)
  stopCluster(cluster)
  return(mean(unlist(tmp), na.rm=TRUE))
}