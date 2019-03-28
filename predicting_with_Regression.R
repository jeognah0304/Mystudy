  library(caret); data("faithful"); set.seed(333)
  
  inTrain = createDataPartition(y=faithful$waiting, p=0.5, list=FALSE)
  trainFaith = faithful[inTrain,]; testFaith <- faithful[-inTrain,]
  
  head(trainFaith)
  
  # Eruption duration versus waiting time
  plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")
  
  # Fit a linear model
  lm1 <- lm(eruptions ~ waiting, data=trainFaith)
  summary(lm1)  
  
  plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")
  lines(trainFaith$waiting, lm1$fitted.values,lwd=3)  
  
  
  # predict a new value.
  # ED_hat = beta0 + beta1*WT
  
  coef(lm1)[1] + coef(lm1)[2]*80 # the ED_hat value , when WT=80
  
  
  newdata = data.frame(waiting=80)
  predict(lm1, newdata)  

    
  
  
  # plot predictions = training and test
  par(mfrow=c(1,2))
  plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")  
  lines(trainFaith$waiting, predict(lm1), lwd=3)  
  

  plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")  
  lines(testFaith$waiting, predict(lm1, newdata=testFaith, lwd=3))  
  
  
  
  
  
  
  # Get training set/test set errors.
  sqrt(sum((lm1$fitted.values-trainFaith$eruptions)^2))
  
  
  
  # Calculate RMSE on training
  sqrt(sum((lm1$fitted.values-trainFaith$eruptions)^2))
  
  # Calculate RMSE on test
  sqrt(sum((predict(lm1, newdata=testFaith)- testFaith$eruptions)^2)) # This is the more realistic.
  
  
  
  
  # Prediction intervals.
  pred1 = predict(lm1, newdata=testFaith, interval="prediction")
  ord = order(testFaith$waiting)  
  plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue")
  matlines(testFaith$waiting[ord], pred1[ord,], type="l",col=c(1,2,2), lty=c(1,1,1),lwd=3)  
  
  
  
  
  # Same process with caret
  modFit = train(eruptions ~ waiting, data= trainFaith, method="lm")
  summary(modFit$finalModel)  
  