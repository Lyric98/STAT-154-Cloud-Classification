#### loss function
err_rate <- function(y, y_pred){
  err_rate <- mean(y != y_pred)
  return(err_rate)
}

#### logistic regression
lrClassifier <- function(train.x, train.y, valid.x){
    train.y[train.y == -1] <- 0
    train <- cbind(train.x, label=train.y)
    currFormula <- as.formula(paste(colnames(train)[ncol(train)], '~', paste(colnames(train.x), collapse = '+'), sep = ''))
    fit.glm <- glm(currFormula, data = train, family=binomial)
    pred.glm <- round(predict(fit.glm,  valid.x, type="response"))
    pred.glm[pred.glm == 0] = -1
    return(list(pred=pred.glm, fit=fit.glm))
}



#### LDA
ldaClassifier <- function(train.x, train.y, valid.x){
  # currFormula <- as.formula(paste(colnames(train.y), '~', paste(colnames(train.x), collapse = '+'), sep = ''))
  fit.lda <- lda(train.x, train.y)
  pred.lda <- predict(fit.lda, valid.x)
  return(list(pred=pred.lda$class, fit=fit.lda))
}



#### QDA
qdaClassifier <- function(train.x, train.y, valid.x){
  # currFormula <- as.formula(paste(colnames(train.y), '~', paste(colnames(train.x), collapse = '+'), sep = ''))
  fit.qda <- qda(train.x, train.y)
  pred.qda <- predict(fit.qda, valid.x)
  return(list(pred=pred.qda$class, fit=fit.qda))
}

#### SVM
svmClassifier <- function(train.x, train.y, valid.x){
    train <- cbind(train.x, train.y)
    fit.svm <- svm(train.x, train.y, type = "C-classification", probability = TRUE)
    pred.svm <- predict(fit.svm,  valid.x, type="response")
    return(list(pred=pred.svm, fit=fit.svm))
}


#### Random Forest
rFClassifier <- function(train.x, train.y, valid.x){
  train <- cbind(train.x,label=train.y)
  currFormula <- as.formula(paste(colnames(train)[ncol(train)],'~',paste(colnames(train.x), collapse = "+"), sep= ''))
  fit.rf <- randomForest(currFormula, data=train, ntree= 300, importance = T)
  pred.rf <- predict(fit.rf, valid.x)
  return(list(pred=pred.rf, fit=fit.rf))
}







#### Cross validation (default 10 folds)
CVgeneric <- function(train.x, train.y, test.x = NULL, classifier=lrClassifier, k=10, loss=err_rate){
  require(caret)
  folds <- createFolds(y=train.y, k=k)
  lossList <- rep(0, k)
  for (i in 1: k){
    trainX <- train.x[-folds[[i]],]
    trainY <- train.y[-folds[[i]]]
    testX <- train.x[folds[[i]],]
    testY <- train.y[folds[[i]]]
    
    model <- classifier(trainX, trainY, testX)
    lossList[i] <- loss(testY, model$pred)
  }
  fit.model <- classifier(train.x, train.y, testX)$fit
  
  return(list(lossList = lossList, fit = fit.model))
}

