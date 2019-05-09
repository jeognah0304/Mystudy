
    # Data
    setwd("C:\\Users\\parkjeongah\\Desktop\\working\\studying major\\È¸±ÍºÐ¼®\\graduate-admissions")
    Admission = read.csv(file="Admission_Predict.csv", header=T)    
    head(Admission)
    str(Admission)
    Admission = Admission[,-1]
    head(Admission)
    
    
    # Regression
    lm0 = lm(Chance.of.Admit~.,data=Admission)
    lm0    
    summary(lm0)  
    fit0 = step(lm0)
    summary(fit0)    
    
    
    
    
    
    #1. Training(70%) & TEST(30%)
    library(caret)
    
    set.seed(123)
    
    inTrain = createDataPartition(Admission$Chance.of.Admit, time=1, p=0.7,list=FALSE) 
    
    Train = Admission[inTrain,] 
    Test = Admission[-inTrain,]
  
    dim(Train)    
    dim(Test)    
    dim(Admission)    
    
    lm1 = lm(Admission$Chance.of.Admit~., data=Admission)
    summary(lm1)
    fit1 = step(lm1)    
    summary(fit1)
    
    #RMSE
    pred.y = predict(fit1, newdata=Test)
    real.y = Test$University.Rating
    
    RMSE = sqrt(mean(sum((real.y-pred.y)^2)))
    
    
    
    #2. 10-fold cross validation
    Admission$sample = sample(1:10, replace=TRUE, size=nrow(Admission))
    
    
    for(i in 1:10){
      
      
      index = Admission$sample==i
      Ad_test.i = Admission[index,] 
      Ad_train.i = Admission[-index,] 
      
      lm_i <- lm(Chance.of.Admit~GRE.Score + TOEFL.Score + University.Rating+ SOP  +LOR+CGPA+Research  , data=Ad_train.i)
      fit_i <- step(lm_i)
      
      pred.y.i = predict(fit_i, newdata=Ad_train.i)
      real.y.i = Ad_train.i$University.Rating
      
      RMSE.i =sqrt(mean(sum((pred.y.i-real.y.i)^2)))
      
      print(RMSE.i)
      
      
    }
    
    
    
    # OverfittingÀÇ ¹®Á¦°¡ »ý±è.
    