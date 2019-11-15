

  #_________________________________________________________
  ## Instrumental Variables in R
  
    library(AER)
    library(systemfit)  
  
    # 0. OLS regression
    olsreg = lm(Y1 ~ Y2 + X1)
    summary(olsreg)
    
    # 1. 2SLS regression
    ivreg = ivreg(Y1 ~ Y2 + X1 | X1+X2)
    summary(ivreg)
    
    # 1-1. 2SLS estimation (details)
    olsreg1 = lm(Y2 ~ X1 + X2)
    summary(olsreg1)
    y2hat = fitted(olsreg1)
    
    olsreg2 = lm(Y1 ~ Y2hat + X1)
    summary(olsreg2)

    # 1-2. 2SLS estimation, over-identified case
    ivreg_o = ivreg(Y1 ~ Y2 + X1 | X1 + X2alt)
    summary(ivreg_o)
    
    # 1-3. Husman test for endogeneity of regressors
    cf_diff = coef(ivreg) - coef(olsreg)
    vc_diff = vcov(ivreg) - vcov(olsreg)
    x2_diff = as.vector(t(cf_diff) %*% solve(vc_diff) %*% cf_diff)
    pchisq(x2_diff, df=2, lower.tail=FASLSE)
    
    
    # 2. systems of equations
    # Defining eqatuions for systems of equations (2SLS, 3SLS)
    # (X12 exogenous variable for eq2, x22 instrument for eq2)
    x12 = cbind(illnesses)
    x22 = cbind(firmlocation)
    
    eq1 = Y1 ~ Y2 + X1 + X2
    eq2 = Y2 ~ Y1 + X12 + x22
    
    inst = X1 + X2 + X22
    system = list(eq1 = eq1, eq2 = eq2)
    
    
    # 3. 2SLS estimation
    reg2sls = systemfit(system, "2SLS", inst = inst, data = mydata)
    summary(reg2sls)
    
    # 4. 3SLS estimation
    reg3sls = systemfit(system, "3SLS", inst = isnt, data = mydata)
    summary(reg3sls)