
#_________________________________________________________
  ## 0. road and exploring panel data
  setwd("C:\\Users\\parkjeongah\\Desktop\\working\\경영공부\\Business analytics")
  wagepan = read.csv(file="wagepan.csv", header=T)
  head(wagepan)
  coplot(lwage ~ year|nr, type="l", data=wagepan)
  coplot(lwage ~ year|nr, type="b", data=wagepan)
  
  
  wagepan$occ = ifelse(wagepan$occ1==1,"1",ifelse(wagepan$occ2==1,"2",ifelse(wagepan$occ3==1,"3",ifelse(wagepan$occ4==1,"4",ifelse(wagepan$occ5==1,"5",ifelse(wagepan$occ6==1,"6",ifelse(wagepan$occ7==1,"7",ifelse(wagepan$occ8==1,"8","9"))))))))
  
  
  library(car)
  scatterplot(lwage~year|occ, boxplot=FALSE, smoothe=TRUE, reg.line=FALSE, data=wagepan) 
  
  # Check Fixed effect : Heterogeneity across educ, exper
  library(gplots)
  plotmeans(lwage ~ educ , main="Heterogeineity across years", data=wagepan)
  plotmeans(lwage ~ occ , main="Heterogeineity across years", data
            =wagepan)
  plotmeans(lwage ~ exper , main="Heterogeineity across years", data=wagepan)
  plotmeans(lwage ~ expersq , main="Heterogeineity across years", data=wagepan)
  
  
  #_________________________________________________________
  ## a. Estimate this equation by pooled OLS, and report the results in standard form. 
  
  # educ variable - heterogeneity 문제있음
  ols = lm(lwage ~ educ+black + hisp + exper + expersq + married + union, data=wagepan)
  summary(ols)  
  yhat = ols$fitted.values
  
  
  plot(wagepan$lwage, wagepan$educ, pch=10, xlab="educ", ylab="lwage")
  abline(lm(lwage ~ educ, data=wagepan), lwd=3, col="red")
  
  
  # summary:: regular OLS regression considers heterogenety across educ and exper
  
  
  
  #_________________________________________________________
  ## Fixed effects: n entity-specific intercepts (using plm)
  ## b. Estimate the equation between-equations (BE) and first-difference (FD) estimations, and compare the results 
  
  library(plm)
  head(wagepan)
  
  table(wagepan$nr, wagepan$year)
  any(table(wagepan$nr, wagepan$year)!=1)
  
  str(wagepan)
  
  
  ## b answer
  
  library(plm)
  head(wagepan)
  
  table(wagepan$nr, wagepan$year)
  any(table(wagepan$nr, wagepan$year)!=1)
  
  str(wagepan)
  
  
  BE = plm(lwage ~ educ + black + hisp + exper + expersq + married + union , data=wagepan, index=c("nr", "year"), model="between")
  summary(BE)  
  
  FD = plm(lwage ~ educ + black + hisp + exper + expersq + married + union , data=wagepan, index=c("nr", "year"), model="fd")
  summary(FD)  
  
  
  
  pFtest(BE, FD)  # BE is better choice

  
  
  #_________________________________________________________
  # Random effects (using plm)
  ## c. Estimate the equation by random effects. Compare your estimates with the pooled OLS estimates. Please describe what problems the outcomes of the pooled OLS estimates may have
  
  random = plm(lwage ~ educ + black + hisp + exper + expersq + married + union , data=wagepan, index = c("nr","year"), model="random")
  summary(random)
  summary(ols)
  
  
  
  # Setting as panel data (an alternative way to run the above model)
  panel.set = pdata.frame(wagepan, index=c("nr","year"))
  
  # Random effects using panel setting (same output as above)
  random.set = plm(lwage ~ educ + black + hisp + exper + expersq + married + union , data=panel.set, model="random")
  summary(random.set)
  
  pFtest(random.set, ols)
  pwtest(ols)
  
  
  # educ variable - heterogeneity 문제있음
  ols = lm(lwage ~ educ+black + hisp + exper + expersq + married + union, data=wagepan)
  summary(ols)  
  
  pwtest(lwage ~ educ+black + hisp + exper + expersq + married + union, data=wagepan)
  pbsytest(lwage ~ educ+black + hisp + exper + expersq + married + 
             union, data=wagepan, test="J")
  pbsytest(lwage ~ educ+black + hisp + exper + expersq + married + union, data=wagepan)
  
  
  #_________________________________________________________
  # Fixed or Random: Hausman test
  ## d. Now estimate the equation by fixed effects. Why is experit redundant in the model even though it changes over time? What happens to the marriage and union premiums as compared with the random effects estimates?
  
  
  
  
  fixed = plm(lwage ~ educ + black + hisp + exper + expersq + married + union , data=wagepan, index=c("nr", "year"), model="within")
  summary(fixed)  
  
  fixef(fixed)
  

  phtest(fixed, random) # fixed effect를 써야함.
  
  
  
  
  #_________________________________________________________
  # Testing for time-fixed effects
  ## e. Estimate the equation by LSDV and compare the result with the fixed effects.  
  ## e는 나중에 더 풀기.
  
  
  #e answer
  
  
  
  str(wagepan)
  wagepan$occ = factor(wagepan$occ)
    
  fixed.dum = lm(lwage ~educ+black + hisp + exper + expersq + married + union +occ , data=wagepan)
  summary(fixed.dum)  
  
  yhat = fixed.dum$fitted.values
  library(car)  
  scatterplot(yhat ~ wagepan$exper|wagepan$nr, boxplot=FALSE, xlab="exper", ylab="yhat", smooth=FALSE)  
  abline(lm(wagepan$lwage~wagepan$exper), lwd=3, col="red")  
  
  
  
  
  
  
  library(apsrtable)
  
  apsrtable(fixed, fixed.dum, model.names=c("fixed","fixed_dum"))  
  cat(apsrtable(ols, fixed.dum, model.names = c("fixed", "fixed_dum"), Sweave=F), file="ols_fixed1.txt")
  
  
  
  
  fixed = plm(lwage ~  educ + black + hisp + exper + expersq + married + union , data=wagepan, index = c("nr","year"), model="within")
  fixed.time = plm(lwage ~  educ + black + hisp + exper + expersq + married + union + factor(year) , data=wagepan, index = c("nr","year"), model="within")
  
  summary(fixed.time)
  
  pFtest(fixed.time, fixed)
  plmtest(fixed, c("time"),type=("bp"))
  # p-value >0.05, in this example, no need to use time-fixed effect
  
  
  #_________________________________________________________
  ##f. Now add interactions of the form d81educ, d82educ; ... ; d87educ and estimate the equation by fixed effects. Has the return to education increased over time? 
  
  
  
  fixed.dum = plm(lwage ~educ+black + hisp + exper + expersq + married + union +occ +d81 + d82 + d83 + d85 + d86 + d87 , data=wagepan,index = c("nr","year"), model="within")
  summary(fixed.dum)  
  
  
  fixed.dum$coefficients
  
  #_________________________________________________________
  ## g. Return to the original model estimated by fixed effects in part c. Add a lead of the union variable, unioni;t+1 to the equation, and estimate the model by fixed effects (note that you lose the data for 1987). Is unioni;t+1 significant?
  
  
  # c model
  random = plm(lwage ~ educ + black + hisp + exper + expersq + married + union , data=wagepan, index = c("nr","year"), model="random")
  summary(random)
  
  
  # making union_(t+1)
  View(wagepan)
  length(wagepan$union)
  
  union_lag = c(NA, wagepan$union[1:4359])
  wagepan$union_lag = union_lag  
  
  random_add = plm(lwage ~ educ + black + hisp + exper + expersq + married + union+union_lag , data=wagepan, index = c("nr","year"), model="random")
  summary(random_add)
  
  View(wagepan)
  
  index = c("nr","year","union")
  relationship = wagepan[,index]
  
  relationship
  
  
  
  #_________________________________________________________
  ## h. Comment on which estimation, of pooled OLS, BE, FD, fixed effects, and random effects, is best fit to this research model, with statistical results (e.g., Hausman test or F-test).
  ## testing for random effects: Breusch-Pagan agrange mutiplier(LM)
  
  # 다시 새롭게 열기 그리고 변수 다 넣기.
  wagepan = read.csv(file="wagepan.csv", header=T)
  
  str(wagepan)
  
  pool = plm(lwage ~ educ + black + hisp + exper + expersq + married + union +hours , data=wagepan, index = c("nr","year"), model="pooling")
  summary(pool)  
  
  
  BE = plm(lwage ~  educ + black + hisp + exper + expersq + married + union +hours+ occ +d81+d82+d83+d84+d85+d86+d87, data=wagepan, index = c("nr","year"), model="between")
  summary(BE)
  
  
  wagepan$occ = factor(wagepan$occ)
  wagepan$d81 = factor(wagepan$d81)
  wagepan$d82 = factor(wagepan$d82)
  wagepan$d83 = factor(wagepan$d83)
  wagepan$d84 = factor(wagepan$d84)
  wagepan$d85 = factor(wagepan$d85)
  wagepan$d86 = factor(wagepan$d86)
  FD = plm(lwage ~  educ + black + hisp + exper + expersq + married + union +hours , data=wagepan, index = c("nr","year"), model="fd")
  summary(FD)
  
  fixed = plm(lwage ~  educ + black + hisp + exper + expersq + married + union +hours+ occ +d81+d82+d83+d84+d85+d86+d87, index = c("nr","year"), model="within", data=wagepan)
  summary(fixed)
  
  random = plm(lwage ~ educ + black + hisp + exper + expersq + married + union +hours+ occ  , data=wagepan, index = c("nr","year"), model="random")
  summary(random)
  
  #The LM test helps you decide between a random effects regression and a simple OLS regression. The null hypothesis in the LM test is that variances across entities is zero. This is, no  significant difference across units (i.e. no panel effect).
  
  # # Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better). 
  
  plmtest(pool, type=c("bp"))
  # We reject the null and conclude that random effect is appropriate. This is evidence of significant differences across nr, therefore we can run a LM.
  
  
  
  phtest(BE, random)
  pFtest(BE, random)
  
  
  #_________________________________________________________
  ##i. Detect any heteroscedasticity, autocorrelation, and multi-collinearity issue from any fixed effects model you tried before.
  
  
  #heteroscedasticity
  library(lmtest)
  bptest(lwage ~  educ+ black + hisp + exper + expersq + married + union+hours+occ+d81+d82+d83+d84+d85+d86+d87, data=wagepan, studentize=F)
  
  
  
  fixed = plm(lwage ~  educ + black + hisp + exper + expersq + married + union +hours+ occ +d81+d82+d83+d84+d85+d86+d87, index = c("nr","year"), model="within", data=wagepan)
  coeftest(fixed) #original coefficients
  coeftest(fixed, vcovHC)
  coeftest(fixed, vcovHC(fixed, method = "arellano"))  # Heteroskedasticity consistent coefficients (Arellano) 
  coeftest(fixed, vcovHC(fixed, type = "HC3"))  # Heteroskedasticity consistent coefficients, type 3 
  
  
  # The following shows the HC standard errors of the coefficients
  t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(fixed, type = x))))) #Standard errors given different types of HC
  
  
  
  pbgtest(fixed) # there is serial correlation
  
  #spatial multicollnearity
  install.packages("spdep")
  library(spdep)
  lm.LMtests(fixed, listw=c("nr","year"))
  ?lm.LMtests
  
  
  summary(fixed)
  
  
  #calculate VIF for checking multi-collinearity
  
  
  library(mctest)
  omcdiag(fixed) #computation of overall collinearity measures
  imcdiag(fixed) #computation of individual collinearity measures for each regressor.
  mctest(fixed) # calls overall and individual collinearity measures
  mc.plot(fixed) #Graphical representation of VIF and eifenvalues.
  
  
  str(wagepan)
  # x must be numeric
  index = c("lwage","educ","exper","expersq","married","hours")
  wage = wagepan[,index]
  
  head(wage)
  x = wage[,-1]
  y = wage[,1]
  
  omcdiag(x,y, detr=0.001, red=0.6, conf= 0.95,  theil = 0.6, cn = 15)
  imcdiag(x, y, corr = TRUE)
  imcdiag(x,y)
  mc.plot(x,y)
  

  # The null hypothesis for the Breusch-Pagan test is homoskedasticity.
  library(lmtest)
  bptest(lwage ~  educ + black + hisp + exper + expersq + married + union + factor(nr) , data=wagepan, studentize = F)
  # presence of heteroskedasticity
  # If hetersokedaticity is detected you can use robust covariance matrix to account for it. See the following pages.
  
  
  
  
  
  
  # Controlling for heteroskedasticity: Robust covariance matrix estimation (Sandwich estimator)
  
  random = plm(lwage ~ educ + black + hisp + exper + expersq + married + union , data=wagepan, index = c("nr","year"), model="random")
  coeftest(random)
  
  coeftest(random, vcovHC)  
  #The --vcovHC??? function estimates three heteroskedasticity-consistent covariance estimators:
  #??? "white1" - for general heteroskedasticity but no serial correlation. Recommended for random effects.
  #??? "white2" - is "white1" restricted to a common variance within groups. Recommended for random effects. 
  #??? "arellano" - both heteroskedasticity and serial correlation. Recommended for fixed effects.
  
  
  coeftest(random, vcovHC(random, type = "HC3")) # Heteroskedasticity consistent coefficients, type 3 
  
  
  t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(random, type = x))))) # std errors given different types of HC.
  
  
  
  # Controlling for heteroskedasticity: Fixed effects
  coeftest(fixed) # original coefficients
  coeftest(fixed, vcovHC) # Heteroskedasticity consistent coefficients
  coeftest(fixed, vcovHC(fixed, method = "arellano")) # Heteroskedasticity consistent coefficients (Arellano) 
    coeftest(fixed, vcovHC(fixed, type = "HC3")) # Heteroskedasticity consistent coefficients, type 3 
    
    
    
    # The following shows the HC standard errors of the coefficients
    t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(fixed, type = x))))) 
    # Standard errors given different types of HC.
    
    
    
    
    
    #_________________________________________________________
    ##j. Correct the issues and run any corrected models.  
    
    
    
    # x must be numeric
    index = c("lwage","educ","exper","married","hours")
    wage = wagepan[,index]
    
    head(wage)
    x = wage[,-1]
    y = wage[,1]
    
    omcdiag(x,y, detr=0.001, red=0.6, conf= 0.95,  theil = 0.6, cn = 15)
    imcdiag(x, y, corr = TRUE)
    imcdiag(x,y)
    mc.plot(x,y)
    
    
    fixed = plm(lwage ~  educ + black + hisp + exper +  married + union +hours+ occ +d81+d82+d83+d84+d85+d86+d87, index = c("nr","year"), model="within", data=wagepan)
    summary(fixed)        
    