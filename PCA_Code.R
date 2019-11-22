
#_________________________________________________________________________
#  0. road the data & packages

setwd("C:\\Users\\parkjeongah\\Desktop\\working\\경영공부\\Business analytics")
data = read.csv(file="data.csv")

library(aod)
library(ggplot2)

#_________________________________________________________________________
#  before regression, PCA
# reason : The amount of calls at the peak time -> fractional.

library(MVA)
library(psych)

# made peack time data
X_peak = cbind(data$x.variable_names)

y = data$y.variable_name

dim(X_peak)

#plot
#pairs.panels(X_peak) 그리면 에러남.
cor = cor(X_peak) # correlation이 높아보임.

library(corrplot)
corrplot::corrplot(cor, method= "color", order = "hclust", tl.pos = 'n')


#making normalization

#define function
normalization <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

X_peak <- normalization(X_peak)
y <- normalization(y)

pca_c = prcomp(X_peak) #공분산행렬이용
summary(pca_c) # 누적기여울이 89.7%인거까지 선택->PC7까지선택
pca_c


pca_r = prcomp(X_peak, scale=T) #상관행렬이용
summary(pca_r) # 누적기여울이 85.8%인거까지 선택->PC6까지선택


pca_c$sdev # 고유값.
pca_r$sdev # 고유값.


pca_c$rotation[,1] # 고유벡터=각 주성분의 rotation 값.
pc1 = pca_c$x[,1]
pc2 = pca_c$x[,2]

cor(pc1,pc2) # pc1, pc2의 공분산은 거의 0에수렴

X_peak_s = scale(X_peak)
summary(X_peak_s)

rot1 = pca_c$rotation[,1]
rot1

plot(X_peak_s%*%rot1, pca_c$x[, 1]) # 상관관계체크-> 상관관계를 보임.



screeplot(pca_c, type = "l") # 어디까지 주성분을 선택할지 체크 =3개면될듯.
#2-3 정도에 체크?

install_github("vqv/ggbiplot")
library(devtools)
library(ggbiplot)
biplot(pca_c)
biplot(pca_r)

par(mfrow=c(1,2))
barplot(pca_c$rotation[,1], col = rainbow(8), ylim = c(-0.1,0.8), las = 2, main = "PC1")
abline(h = -0.3, col="blue")
barplot(pca_c$rotation[,2], col = rainbow(8), ylim = c(-0.6,0.8), las = 2, main = "PC2")
abline(h = 0.3, col="blue")

