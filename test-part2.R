library(dplyr)
library(caret)
library(e1071)
source('RFunctions-1.R')
cancer <- read.csv("cancer.csv")
names(cancer) <- c(seq(1,30),"output")


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








