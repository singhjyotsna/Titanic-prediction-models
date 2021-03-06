setwd("C:\\MICA\\Trimester 4\\AMMA\\2017_AMMA Data\\data_2017\\Titanic")
titanic_data= read.csv("train.csv")

###understanding data
View(titanic_data)
str(titanic_data)
head(titanic_data)

###selecting necessary variables
titanic_final= titanic_data[c("Pclass" ,"Sex" ,"Age" ,"SibSp", "Parch","Survived")]
View(titanic_final)
str(titanic_final)
summary(titanic_final)

###Missing value imputation
titanic_final$Age[is.na(titanic_final$Age)]= mean(titanic_final$Age[!is.na(titanic_final$Age)])
summary(titanic_final)

###Splitting data 
set.seed(1234)
df.titanic <- runif(nrow(titanic_final))
titanic_train <- titanic_final[df.titanic <= 0.7,]
titanic_test<- titanic_final[df.titanic> 0.7,]

###understanding normality of the variables
hist(titanic_final$Age)


###Crosstabluation of dependent categorical variable
CrossTable(titanic_final$Survived)
CrossTable(titanic_train$Survived)
CrossTable(titanic_test$Survived)

###Checking multicolinearity
titanic_equ= lm(formula = Survived ~ Pclass+Sex+Age+SibSp+Parch,data = titanic_final)
vif(titanic_equ)

###Running a logistic regression model
titanic_equ_final= glm(formula = Survived ~ Pclass+Sex+Age+SibSp+Parch,data = titanic_train,family = binomial)
summary(titanic_equ_final)

###removing variables which are not significant when confidence interval in 95% (p value = 0.05)
new_titanic_equ = glm(formula = Survived ~ Pclass+Sex+Age+SibSp,data = titanic_train,family = binomial)
summary(new_titanic_equ)

###probabilities of training data 
titanic_train$prob = predict(new_titanic_equ,type=c("response"))
titanic_train$pred = ifelse(titanic_prob>=.5,'pred_yes','pred_no')
View(titanic_train)

###Roc curve of training data
titanic_train_roc <- roc(Survived ~ prob , data = titanic_train)
plot(titanic_train_roc)
auc(titanic_train_roc)


### confusion matrix of training data 
table(titanic_train$ypred,titanic_train$Survived)


### index for measuring trainning data
accuracy_train = (343+177)/(343+177+63+70)
print(accuracy_train)
precision_train = (177/(177+70))
print(precision_train)
Recall_train = (177/(177+63))
print(Recall_train)
f1_train = (2*0.7165992*0.7375)/(0.7165992+0.7375)
print(f1_train)


###_____________________________________________

###probabilities of test data 
titanic_test$prob= predict(new_titanic_equ,titanic_test,c=("response"))
titanic_test$ypred = ifelse(Titanic_test_predict>=.5,'pred_yes','pred_no')
View(titanic_test)


###Roc curve of test data
titanic_test_roc <- roc(Survived ~ prob , data = titanic_test)
plot(titanic_test_roc)
auc(titanic_test_roc)

### confusion matrix of test data 
table(titanic_test$ypred,titanic_test$Survived)

### index for measuring trainning data
accuracy_test = (46+134)/(46+134+9+49)
print(accuracy_test)
precision_test = (46/(46+49))
print(precision_test)
Recall_test = (343/(343+9))
print(Recall_test)
f1_test = (2* 0.4842105*0.9744318)/(0.4842105+0.9744318)
print(f1_test)

###________________________________________________________________________


##predicting survival of Jack and Rose
survival=read.csv("book1.csv")
predict_survival= predict(new_titanic_equ,survival,c=("response"))
survival$pred = ifelse(predict_survival>=.5,'pred_yes','pred_no')
View(survival)


