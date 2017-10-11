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

########################

train.X <- train[,1:76]
train.y <- train[,77]
test.X <- test[,1:76]
test.y <- test[,77]

cMat <- NULL
neighbors <-c(1,3,5,10,15)
for(i in seq_along(neighbors)){
    fit =knn(train.X,test.X,train.y,k=i)
    table(fit,test.y)
    a<-confusionMatrix(fit,test.y)
    cMat[i] <- a$overall[1]
    print(a$overall[1])
    print(a$byClass)
}

df <- data.frame(neighbors,Accuracy=cMat)
ggplot(df,aes(x=neighbors,y=Accuracy)) + geom_point() +geom_line(color="blue") +
    xlab("Number of neighbors") + ylab("Accuracy") +
    ggtitle("KNN regression - Accuracy vs Number of Neighors (Unnormalized)")

######################################
#K-fold

set.seed(17)
df=read.csv("auto_mpg.csv",stringsAsFactors = FALSE) # Data from UCI
df1 <- as.data.frame(sapply(df,as.numeric))

df2 <- df1 %>% dplyr::select(cylinder,displacement, horsepower,weight, acceleration, year,mpg)
df3 <- df2[complete.cases(df2),]

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
    glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
    cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10
folds <- seq(1,10)
df <- data.frame(folds,cvError=cv.error.10)
ggplot(df,aes(x=folds,y=cvError)) + geom_point() +geom_line(color="blue") +
    xlab("Degree of poylnomial") + ylab("CV Error") +
    ggtitle("K Fold CV - Degree of polynomial vs CV Error")
#################LOOCV
library(boot)
glm.fit=glm(mpg~horsepower,data=df)
coef(glm.fit)
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)
library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)$delta

cv.error=rep(0,5)
for (i in 1:8){
    glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
    cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

a=matrix(rep(0,30),nrow=3,ncol=10)
set.seed(17)
folds<-c(3,5,10)
for(i in seq_along(folds)){
        cv.error.10=rep(0,10)
        for (j in 1:10){
            glm.fit=glm(mpg~poly(horsepower,j),data=Auto)
            #cv.error=cv.glm(Auto,glm.fit,K=i)$delta[1]
            a[i,j]=cv.glm(Auto,glm.fit,K=folds[i])$delta[1]
            print(cv.error)
        }
        
}
b <- t(a)
df <- data.frame(b)
df1 <- cbind(seq(1,10),df)
names(df1) <- c("PolynomialDegree","3-fold","5-fold","10-fold")

df2 <- melt(df1,id="PolynomialDegree")
ggplot(df2) + geom_line(aes(x=PolynomialDegree, y=value, colour=variable),size=2) 
cv.error

matrix(rep(0,30),nrow=3,ncol=10)

folds <- seq(1,10)
df <- data.frame(folds,cvError=cv.error.10)
ggplot(df,aes(x=folds,y=cvError)) + geom_point() +geom_line(color="blue") +
    xlab("Degree of poylnomial") + ylab("CV Error") +
    ggtitle("K Fold CV - Degree of polynomial vs CV Error")




##############################
a=matrix(rep(0,30),nrow=3,ncol=10)
set.seed(17)
folds<-c(5,7,10)
for(i in seq_along(folds)){
    cv.error.10=rep(0,10)
    for (j in 1:10){
        glm.fit=glm(mpg~poly(horsepower,j),data=df3)
        #cv.error=cv.glm(Auto,glm.fit,K=i)$delta[1]
        a[i,j]=cv.glm(Auto,glm.fit,K=folds[i])$delta[1]
        print(cv.error)
    }
    
}

folds <- seq(1,10)
df <- data.frame(folds,cvError=cv.error.10)
ggplot(df,aes(x=folds,y=cvError)) + geom_point() +geom_line(color="blue") +
    xlab("Degree of poylnomial") + ylab("CV Error") +
    ggtitle("K Fold CV - Degree of polynomial vs CV Error")
"