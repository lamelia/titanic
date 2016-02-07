
########Pre-processing data############

#1 Missing data

#Both train and test data has missing data 
#in predictor $Age$, $Fare$, and $Embarked$. 
#To address this problem, we do single imputation 
#using MICE package in R. Before doing multiple imputation, 
#we combine train and test data to be one set of data named **all**.

#train and test data is combined
#get all data
all <- read.csv(("all.csv"))

#factoring variables
all$Pclass <- factor(all$Pclass)
all$Sex <- factor(all$Sex)
all$Embarked <- factor(all$Embarked)

#convert factor in Embarked to numeric factor C=1, Q=2, S=3
#then create variable new_Embarked which factor is now numeric 
all$new_Embarked <- ifelse(all$Embarked=="C",1,NA)
all$new_Embarked <- ifelse(all$Embarked=="Q",2,all$new_Embarked)
all$new_Embarked <- ifelse(all$Embarked=="S",3,all$new_Embarked)

#factor variable new_embarked
all$new_Embarked <- factor(all$new_Embarked)

#multiple imputation for Age, Fare, and new_embarked using MICE
#install mice package
library(mice)

#create new table contain age, fare, and new_embarked variable only 
age_emb <- all[c(6,10,13)] 
#multiple imputation for age, fare, and new_embarked
age_emb_imp <- mice(age_emb,me=c("pmm","pmm","polyreg"))
#check complete data for age, fare, and embarked
com_age_emb <- complete(age_emb_imp,1)
#combine all data
combine1 <- cbind(all,com_age_emb)
alls <- combine1[c(-6,-10,-12,-13)] #final data after imputation
summary(alls)
#export data to csv format
write.csv(alls,file="all_complete_titanic.csv")


#2. Variable transformation and variable extraction

#2.1 Make variable *TicketCount* 
#and will treat passenger's names as variable in our model

#get data
alls <- read.csv(("all_complete_titanic.csv"))


#transform variable *Ticket* into *TicketCount*. 
#*TicketCount* is the number of passengers with the same ticket number.  

#create variable TicketCount
alls$TicketCount<-apply(alls, 1, function(x) sum(as.numeric(alls$Ticket == x["Ticket"])))


#2.2 using bag-of-words technique for variable *Name*.
#we treat passengers name as variable

#install text mining package
require(tm)

# build a corpus
alls.corpus <- Corpus(VectorSource(alls$Name))

# make each letter lowercase
alls.corpus <- tm_map(alls.corpus, content_transformer(tolower)) 

# remove punctuation 
alls.corpus <- tm_map(alls.corpus, removePunctuation)

# build a term-document matrix
alls.dtm <- TermDocumentMatrix(alls.corpus)

# inspect the document-term matrix
alls.dtm

# inspect most popular words
findFreqTerms(alls.dtm, lowfreq=30)

alls.dtm2 <- removeSparseTerms(alls.dtm, sparse=0.999)

bag.df <- t(as.data.frame(inspect(alls.dtm2)))

#Now we have a data frame containing all 
#bag-of-words variable, we name this **bag.df**.

###############Train and Test data set##########################
#3. Train and test data set

#single imputation, variable transformation, and variable extraction,
#next step is to split data into the origin train and test data. 
#Titanic train data consists of observation from 1 to 891, 
#and Titanic test data consists of observation from 892 to 1309. 


#split data frame **bag.df** into **bag_train** and **bag_test**.
#divide bag.df into train and test
bag_train <- bag.df[1:891,]
bag_test <- bag.df[892:1309,]


#split data **alls** into train and test data.  
#exclude unwanted variables in train and test data. 
#And factoring the categorical variables in both train and test data.
#name our train and test data as **cT_train** and **cT_test**.

#divide alls into train and test data set
titanic_train <- alls[1:891,]
titanic_test <- alls[892:1309,]

#remove unwanted predictors/columns in both train and test data set
cT_train <- titanic_train[c(-1,-2,-5,-9,-10)] 
cT_test <- titanic_test[c(-1,-2,-5,-9,-10)]

#factoring Pclass, Sex, new_embarked for cT_train and cT_test
cT_train$Pclass <- factor(cT_train$Pclass)
cT_train$Sex <- factor(cT_train$Sex)
cT_train$new_Embarked <- factor(cT_train$new_Embarked)
cT_test$Pclass <- factor(cT_test$Pclass)
cT_test$Sex <- factor(cT_test$Sex)
cT_test$new_Embarked <- factor(cT_test$new_Embarked)


#Now we combine **cT_train** with **bag_train**, 
#and combine **cT_test** with **bag_test**.
#Our final train data is **c_train** and test data is **c_test**.

#combine data train/test with bag.df
c_train <- cbind(cT_train,bag_train)
c_test <- cbind(cT_test,bag_test)

########################predictive modelling##################

#4. Penalized Logistic Regression

#use L1-penalized logistic regression to build 
#predictive model in train data **c_train**. 

#installing package boot and glmnet
library(boot)
library(glmnet)
set.seed(123)

#formula for model
f <- as.formula(Survived~.+log(Age)+Pclass*SibSp+Pclass*Sex+Parch*Pclass+
                  SibSp*Parch+Parch*Sex+Age*Sex*Pclass+Age*Parch*Pclass+Age*SibSp*Pclass+
                  Age*Fare*Pclass+Sex*Fare*Pclass)

#transform data to matrices 
x <- model.matrix(f,c_train)
y <- as.matrix(c_train$Survived,ncol=1)

#run penalized logistic regression with alpha=1, number of folds=20 in train data (c_train)
cv.fit <- cv.glmnet(x,y,alpha=1,family="binomial", type.measure = "class",nfolds = 20)
#check all mean cross validation error
cv.fit$cvm
#the minimum mean cross validation error
min(cv.fit$cvm)

#The minimum mean cross validation error is 0.1739618. 
#Next is applying model cv.fit to test data, **c_test**. 
#Then predicting $Survived$ in test data based on model cv.fit.  

#applying model to test data (c_test)
c_test$Survived <- 0
xtest <- model.matrix(f,c_test)
glm.pred <- rep(0,418)
#predict test data based on the cv.fit
glm.prob <- predict(cv.fit,newx = xtest,type="class", s="lambda.min")
glm.pred[glm.prob > 0.5]=1

#The result of prediction in test data will be exported to csv file. 
#We name it as predT.csv. This file will be submitted to 
#https://www.kaggle.com/c/titanic. 

#create new data frame containing only passenger id and survival prediction
Survived <- c(glm.pred)
PassengerId <- alls$PassengerId[892:1309]
prediction <- data.frame(PassengerId,Survived)
#next export to csv file
write.csv(prediction,file="predT.csv")

#######################################################################