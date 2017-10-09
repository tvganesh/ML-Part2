library(dplyr)
ca
cancer <- read.csv("cancer.csv")
X_cancer <- as.matrix(cancer[,1:30])
y_cancer <- cancer[,31]
glm.fits=glm(y_cancer~X_cancer,family=binomial,control = list(maxit = 50))
