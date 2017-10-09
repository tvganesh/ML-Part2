library(dplyr)
source('RFunctions-1.R')
cancer <- read.csv("cancer.csv")
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




train_idx <- trainTestSplit(adult,trainPercent=75,seed=5)
train <- adult[train_idx, ]
test <- adult[-train_idx, ]


X_train <- train[,1:8]
y_train <- train[,9]
X_train1 <- dummy.data.frame(X_train, sep = ".")


X_adult_train <- as.matrix(X_train1)
fit=glm(y_train~X_adult_train,family=binomial)
1 - fit$deviance/fit$null.deviance




###########################



R2logit(fit,nullmod1)
#######################################################
b <- read.table("adult.names",header=FALSE,sep=" ")
b1 <-as.character(b[,2])

names(crimesDF) <- b1
