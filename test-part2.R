library(dplyr)
library(caret)
library(e1071)
source('RFunctions-1.R')
cancer <- read.csv("cancer.csv")
names(cancer) <- c(seq(1,30),"output")
X_cancer <- as.matrix(cancer[,1:30])
y_cancer <- cancer[,31]
glm.fits=glm(y_cancer~X_cancer,family=binomial,control = list(maxit = 50))

train_idx <- trainTestSplit(cancer,trainPercent=75,seed=5)
train <- cancer[train_idx, ]
X_train <- as.matrix(train[,1:30])
y_train <- train[,31]
test <- cancer[-train_idx, ]
X_test <- as.matrix(test[,1:30])
y_test <- test[,31]

fit=glm(y_train~X_train,family=binomial,control = list(maxit = 50))
nullmod <- glm(y_train~1, family="binomial")
R2logit(fit,nullmod)

nullmod1 <- glm(y_test~1, family="binomial")
#http://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/

R2logit(fit,nullmod1)

####################################
#2
train_idx <- trainTestSplit(cancer,trainPercent=75,seed=5)
train <- cancer[train_idx, ]
test <- cancer[-train_idx, ]

fit=glm(output~.,family=binomial,data=train,control = list(maxit = 50))
a=predict(fit,newdata=train,type="response")
b=ifelse(a>0.5,1,0)
confusionMatrix(b,train$output)




m=predict(fit,newdata=test,type="response")
n=ifelse(m>0.5,1,0)
confusionMatrix(n,test$output)

########################
# Read Adult data
install.packages("dummies")
library(dummies)
df <- read.csv("adult.csv")
names(df) <- c("age","workclass","fnlwgt","education","educationNum","maritalStatus",
                "occupation","relationship","race","sex","capitalGain","capital-loss",
               "hours-per-week","native-country","salary")


df$salary <-gsub("<=50K",0,df$salary)
df$salary <-gsub(">50K",1,df$salary)
df$salary <- as.numeric(df$salary)
write.csv(df,"adult1.csv")

###################################Actual
# Read the data
df <- read.csv("adult1.csv",stringsAsFactors = FALSE,na.strings = c(""," "," ?"))

# Remove rows which have NA
df1 <- df[complete.cases(df),]
dim(df1)
adult <- df1 %>% dplyr::select(age,occupation,education,educationNum,capitalGain,
                        capital.loss,hours.per.week,native.country,salary)


train_idx <- trainTestSplit(adult,trainPercent=75,seed=1111)
train <- adult[train_idx, ]
test <- adult[-train_idx, ]


xtrain <- train[,1:8]
ytrain <- train[,9]
xtrain1 <- dummy.data.frame(xtrain, sep = ".")
xtrain2 <- as.matrix(xtrain1)

xtest <- test[,1:8]
ytest <- test[,9]
xtest1 <- dummy.data.frame(xtest, sep = ".")
xtest2 <- as.matrix(xtest1)

fit=glm(ytrain~xtrain2,family=binomial)
a=predict(fit,newdata=xtrain1,type="response")

####///////////////////////////////////////////////////////////////////


adult <- df1 %>% dplyr::select(age,occupation,education,educationNum,capitalGain,
                               capital.loss,hours.per.week,salary)

train_idx <- trainTestSplit(adult,trainPercent=75,seed=1111)
train <- adult[train_idx, ]
test <- adult[-train_idx, ]


xtrain <- train[,1:7]
ytrain <- train[,8]
xtrain1 <- dummy.data.frame(xtrain, sep = ".")
xtrain2 <- as.matrix(xtrain1)

xtest <- test[,1:7]
ytest <- test[,8]
xtest1 <- dummy.data.frame(xtest, sep = ".")
xtest2 <- as.matrix(xtest1)




fit=glm(ytrain~xtrain1,family=binomial)
a=predict(fit,newdata=xtrain1,type="response")
b=ifelse(a>0.5,1,0)
confusionMatrix(b,ytrain)


m=predict(fit,xtest1,type="response")
n=ifelse(m>0.5,1,0)
confusionMatrix(n,ytest)


########################################################################################
#3
df <- read.csv("adult1.csv",stringsAsFactors = FALSE,na.strings = c(""," "," ?"))

# Remove rows which have NA
df1 <- df[complete.cases(df),]
dim(df1)

adult <- df1 %>% dplyr::select(age,occupation,education,educationNum,capitalGain,
                               capital.loss,hours.per.week,native.country,salary)

adult1 <- dummy.data.frame(adult, sep = ".")

train_idx <- trainTestSplit(adult1,trainPercent=75,seed=1111)
train <- adult1[train_idx, ]
test <- adult1[-train_idx, ]




fit=glm(salary~.,family=binomial,data=train)
a=predict(fit,newdata=train,type="response")
b=ifelse(a>0.5,1,0)
confusionMatrix(b,train$salary)




m=predict(fit,newdata=test,type="response")
n=ifelse(m>0.5,1,0)
confusionMatrix(n,test$salary)








#####################

probs=predict(fit,type="response")

contrasts(occupation)
X_adult_train=X_adult_test

xtrain2=xtest1


xtrain1=xtest1



1 - fit$deviance/fit$null.deviance




###########################



R2logit(fit,nullmod1)
#######################################################
b <- read.table("adult.names",header=FALSE,sep=" ")
b1 <-as.character(b[,2])

names(crimesDF) <- b1
