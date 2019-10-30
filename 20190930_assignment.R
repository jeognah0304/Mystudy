setwd("C:\\Users\\parkjeongah\\Desktop\\working\\경영공부\\Business analytics")

# 1번_ data 로드
banksalary = read.csv("banksalary.csv")
head(banksalary)

# salary의 특수문자를 다 제거.
banksalary$Salary = gsub("[^[:alnum:][:blank:]+?&/\\-]", "", banksalary$Salary)
banksalary$Salary = substr(banksalary$Salary,1,5)
head(banksalary) # salary 단위 :달러

library(dplyr)
female = banksalary %>% filter(Gender=="Female")
male = banksalary %>% filter(Gender=="Male")

banksalary$Salary = as.numeric(banksalary$Salary)
banksalary$YrsExper = as.numeric(banksalary$YrsExper)



# 2번 
summary(banksalary)
str(banksalary)
sd(banksalary$EducLev)

table(female$PCJob)
table(male$PCJob)



# 3번
plot(banksalary$Salary, banksalary$YrsExper, main="Correlation between Salary and YrsExper", xlab="Years of Experiences", ylab="Salary",cex=1, pch=1, col="blue")
cor(banksalary$Salary, banksalary$YrsExper)

cor.test(banksalary$Salary, banksalary$YrsExper)

# 4번


summary(female)
summary(male)

female$Salary = as.numeric(female$Salary)
male$Salary = as.numeric(male$Salary)

mean(female$Salary); sd(female$Salary)
mean(male$Salary) ; sd(male$Salary)


t.result01 = t.test(Salary~Gender, data=banksalary)
t.result01

reg = lm(Salary~EducLev+JobGrade+YrsExper+Age+Gender+YrsPrior+PCJob)
summary(reg)
af = step(reg)



# total reg
summary(af)


#5번.
str(banksalary)
index=c("EducLev","JobGrade","YrsExper","Age","YrsPrior","Salary")
num = banksalary[,index]

str(banksalary)

head(num)
cor(num,y=num$Salary)



# 6번

Total_years01 = female_d$YrsExper+female_d$YrsPrior
Total_years02 = male_d$YrsExper + male_d$YrsPrior

reg.res1 = lm(female_d$Salary~Total_years01)
reg.res2 = lm(male_d$Salary~Total_years02)

summary(reg.res1)


banksalary$total_years = banksalary$YrsExper + banksalary$YrsPrior

female = banksalary[banksalary$Gender=="Female",]
male = banksalary[banksalary$Gender=="Male",]
reg.res1 = lm(Salary~total_years, data=female)
reg.res2 = lm(Salary~total_years, data=male)


reg.res1
reg.res2

plot(banksalary$total_years, banksalary$Salary,  col=as.integer(banksalary$Gender),xlim=c(0, 40), ylim=c(20000, 100000), xlab = "Total Years about YrsPrior + YrsExper", ylab = "Salary", main="Scatterplot about Total years and Salary ")

abline(reg=reg.res1, col="blue",lwd=3 ) #female
abline(reg=reg.res2, col="red", lwd=3) #male

legend(x=0, y=90000, c("female", "male"),col=c("blue","red"), pch=c(1,1))

reg.res5 = lm(Salary~total_years+Gender, data=banksalary)
summary(reg.res5)



male = male %>% arrange(desc(Salary))
head(male)


male_d = male[-c(1:5),]
male_d$total_years = male_d$YrsExper + male_d$YrsPrior
reg.res3 = lm(Salary~total_years, data=male_d)

reg.res1
reg.res3

plot(reg.res1)



banksalary_d = banksalary[-c(205,204,207,206,203),]
dim(banksalary_d)
dim(banksalary)

plot(banksalary_d$total_years, banksalary_d$Salary,col=as.integer(banksalary$Gender),xlim=c(0, 40), ylim=c(20000, 80000), xlab = "Total Years about YrsPrior + YrsExper", ylab = "Salary", main="Scatterplot about Total years and Salary ")

abline(reg=reg.res1, col="blue",lwd=3 ) #female
abline(reg=reg.res3, col="red", lwd=3) #male


legend(x=0, y=70000, c("female", "male"),col=c("blue","red"), pch=c(1,1))




# 7번

head(banksalary)
형

# y = 연속형, x변수중에 연속형 범주형 섞여있는 모습.
reg.res3 = lm(Salary ~ EducLev+JobGrade+Gender+PCJob, data=banksalary)
summary(reg.res3)

str(banksalary)

banksalary$EducLev = as.factor(banksalary$EducLev)
banksalary$JobGrade = as.factor(banksalary$JobGrade)

# 범주형자료분석 y = 연속형, x변수 전부 범주혀
reg.res4 = lm(Salary ~ EducLev+JobGrade+Gender+PCJob, data=banksalary)
summary(reg.res4)



banksalary$Salary = as.numeric(banksalary$Salary)

Pcjob_y = banksalary[banksalary$PCJob=="Yes",]
Pcjob_n = banksalary[banksalary$PCJob=="No",]

str(Pcjob_n)

mean(Pcjob_n$Salary,na.rm=T)
mean(Pcjob_y$Salary, na.rm=T)

t.test(Pcjob_n$Salary,Pcjob_y$Salary)



# 8번

banksalary = banksalary %>% arrange(desc(Salary))
head(banksalary)

banksalary = banksalary[-c(1:5),]

reg8 = lm(Salary ~ total_years + Gender, data=banksalary)
summary(reg8) # R-squred 0.2295 밖에 안됨.

b0 = 30981.5
b1 = 513.5
b2 = 5872.7

gender = ifelse(banksalary$Gender=="Female", 0 , 1)

pred = b0 + b1*banksalary$total_years +b2*gender



error = banksalary$Salary - pred


shapiro.test(error)
qqnorm(error, main="Normal Q-Q Plot", xlab="Theoretical Quantiles", ylab="Sample Quantiles")
qqline(error, col=2)

plot(reg8)


# multicollinearity check

head(banksalary)
index8 = c("total_years","Gender","Salary")
X = banksalary[,index8]
X

install.packages("GGally")
library(GGally)

ggpairs(X)

ggpairs(banksalary)





index1 = c("total_years","YrsExper","YrsPrior")
d = banksalary[,index1]

install.packages("corpcor")
library(corpcor)
a = cor2pcor(cov(d))

colnames(a) = c("total_years","YrsExper","YrsPrior")
rownames(a) = c("total_years","YrsExper","YrsPrior")
a



X = as.data.frame(X)

ggpairs(X)
X





index = c("EducLev","JobGrade","YrsExper","Age","Gender","YrsPrior","PCJob","Salary")

X = banksalary[,index]
head(X)
ggpairs(X)

library(corpcor)

X = X[,-c(5:7)]
head(X)
a = cor2pcor(cov(X))
colnames(a) = c("EducLev","JobGrade","YrsExper","Age")
rownames(a) = c("EducLev","JobGrade","YrsExper","Age")


install.packages("mctest")
library(mctest)

head(X)


index = c("EducLev","JobGrade","YrsExper","Age","Salary")
X = X[,index]
head(X)
str(X)

X$Salary = as.numeric(X$Salary)
omcdiag(X,X$Salary)

imcdiag(X,X$Salary)


install.packages("ppcor")
library(ppcor)
pcor(X, method = "pearson")






head(banksalary)

reg.final = lm(Salary~.,data=banksalary)
step(reg.final)


head(banksalary)


banksalary = banksalary %>% arrange(desc(Salary))
head(banksalary)

banksalary = banksalary[-c(1:5),]
head(banksalary)

reg.final = lm(Salary~.,data=banksalary)
step(reg.final)


Gender = ifelse(banksalary$Gender=="Male",1,0)
PCJobYes = ifelse(banksalary$PCJob=="Yes",1,0)

pred.final = 24831.4 + 4111.6*banksalary$JobGrade + 211.5*banksalary$YrsExper + 1742.1*Gender + 4222*PCJobYes

y = as.numeric(banksalary$Salary)
res.final = y - pred.final

plot(y, res.final, xlab="Y value_ Salary", ylab="Residual _error term")


plot(y, res.final, xlab="Y value_ Salary", ylab="Residual _error term", ylim=c(-15000,15000))



cor(y,res.final)
cor.test(y,res.final)
