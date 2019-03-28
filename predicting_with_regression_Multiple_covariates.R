  library(ISLR); library(ggplot2); library(caret)

  data(Wage); Wage <- subset(Wage, select=-c(logwage))  
  summary(Wage)  
  
  inTrain = createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
  training = Wage[inTrain,] ; testing = Wage[-inTrain,]  
  dim(training) ; dim(testing)  
  
  ## feature plot
  featurePlot(x=training[,c("age","education","jobclass")],
              y = training$wage,
              plot="pairs")

  # feature plot:   It's too complicated to interpreted because everything is so squished together but if we make it on our own, by using this function, We'll see that it's a little bit easier to see.
  
  
  
  ## Plot age versus Wage
  qplot(age, wage, data=training)
  qplot(age, wage, colour=jobclass,data=training)
  
  # Plot age versus wage colour by education
  qplot(age, wage, colour=education, data=training)
  
  
  
  ## Fit a linear model
  # ED(i) = b0 + b1*age + b2*I(jobclass(i)="Information") + sum r(k)*I(education(i)=levelk)
  
  modFit = train(wage ~ age + jobclass + education, 
                 method = "lm", data=training)
  finMod = modFit$finalModel  
  print(modFit)  
  
  
  
  
  ## Diagnostics
  plot(finMod, 1, pch=19 ,cex = 0.5, col="#00000010")
  
  
  # Color by variables not used in the model
  qplot(finMod$fitted.values, finMod$residuals, colour=race, data=training)
  
  # Plot by index
  plot(finMod$residuals, pch=19)
  
  
  # Predicted versus truth in Test set
  pred <- predict(modFit, testing)
  qplot(wage, pred, colour=year, data=testing) # 테스트셋을 통해서 항상 모델을 업데이트 하는 것이 매우매우 중요하다는 사실을 잊지말자!
  
  
  
  
  # If we want to use all covariates
  modFitAll = train(wage ~. , data=training, method="lm")
    pred = predict(modFitAll, testing)
    qplot(wage, pred, data=testing)
    