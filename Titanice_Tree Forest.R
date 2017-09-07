setwd("C:\\MICA\\Trimester 4\\AMMA\\2017_AMMA Data\\data_2017\\Titanic")
titanic_data= read.csv("train.csv")

View(titanic_data)
str(titanic_data)
head(titanic_data)


###selecting necessary variables
titanic_final= titanic_data[c("Pclass" ,"Sex" ,"Age" ,"SibSp", "Parch","Survived")]
View(titanic_final)
str(titanic_final)
summary(titanic_final)

install.packages("rpart")
library("rpart")

###Missing value imputation
titanic_final$Age[is.na(titanic_final$Age)]= mean(titanic_final$Age[!is.na(titanic_final$Age)])
summary(titanic_final)


###Splitting data 
set.seed(1234)
df.titanic <- runif(nrow(titanic_final))
titanic_train <- titanic_final[df.titanic <= 0.7,]
titanic_test<- titanic_final[df.titanic> 0.7,]

install.packages("rpart.plot")
library("rpart.plot")

fit<-rpart(Survived ~ Pclass+Age+Sex, titanic_train, method="class")
plot_fit=plot(fit, uniform= T, main="classification tree for Titanic")

text(fit, use.n= TRUE, all = TRUE, cex=.8)

rpart.plot(fit)
install.packages("randomForest")
library("randomForest")
rf=randomForest(Survived ~Pclass+Age+Sex, data=titanic_train, mtry=2, importance=TRUE)
print(rf)
summary(rf)
varImpPlot(rf)

###probabilities of training data 
titanic_train$prob = predict(rf, newdata = titanic_train ,type=c("response"))
titanic_train$rfpred = ifelse(titanic_train$prob>=.5,'pred_yes','pred_no')
View(titanic_train)

### confusion matrix of training data 
table(titanic_train$rfpred,titanic_train$Survived)


install.packages("e1071")
library ("e1071")



