#export data to csv format
all_complete <- read.csv(("all_complete_titanic.csv"))

#text mining
require(tm)

# build a corpus
all_complete.corpus <- Corpus(VectorSource(all_complete$Name))

# make each letter lowercase
all_complete.corpus <- tm_map(all_complete.corpus, content_transformer(tolower)) 

# remove punctuation 
all_complete.corpus <- tm_map(all_complete.corpus, removePunctuation)

# remove generic and custom stopwords
# my_stopwords <- c(stopwords('english'), 'prolife', 'prochoice')
# cT.corpus <- tm_map(cT.corpus, removeWords, my_stopwords)

# build a term-document matrix
all_complete.dtm <- TermDocumentMatrix(all_complete.corpus)

# inspect the document-term matrix
all_complete.dtm

# inspect most popular words
findFreqTerms(all_complete.dtm, lowfreq=30)

all_complete.dtm2 <- removeSparseTerms(all_complete.dtm, sparse=0.999)

bag.df <- t(as.data.frame(inspect(all_complete.dtm2)))

#divide bag.df to train and test
bag_train <- bag.df[1:891,]
bag_test <- bag.df[892:1309,]

#divide all to train and test dataset
titanic_train <- all_complete[1:891,]
titanic_test <- all_complete[892:1309,]

#remove unwanted predictors in train and test data
cT_train <- titanic_train[c(-1,-2,-5,-9,-10)]
cT_test <- titanic_test[c(-1,-2,-5,-9,-10)]

#factor Pclass,new_embarked for cT_train and cT_test
cT_train$Pclass <- factor(cT_train$Pclass)
cT_train$new_Embarked <- factor(cT_train$new_Embarked)

cT_test$Pclass <- factor(cT_test$Pclass)
cT_test$new_Embarked <- factor(cT_test$new_Embarked)

str(cT_train)
str(cT_test)

#merge data train/test with bag.df
c_train <- cbind(cT_train,bag_train)
c_test <- cbind(cT_test,bag_test)

#glmnet

library(boot)
library(glmnet)
set.seed(123)
#formula from data
f <- as.formula(Survived~.+log(Age)+Pclass*SibSp+Pclass*Sex+Parch*Pclass+SibSp*Parch+Parch*Sex+log(Age)+Age*Sex*Pclass+Age*Parch*Pclass+Age*SibSp*Pclass+Age*Fare*Pclass+Sex*Fare*Pclass)

#transform data to matrices required by glmnet
x <- model.matrix(f,c_train)
y <- as.matrix(c_train$Survived,ncol=1)
cv.fit <- cv.glmnet(x,y,alpha=1,family="binomial", type.measure = "class",nfolds = 20)
cv.fit$cvm
min(cv.fit$cvm)


#apply model to test data (c_test)
c_test$Survived <- 0
xtest <- model.matrix(f,c_test)
glm.pred <- rep(0,418)
glm.prob <- predict(cv.fit,newx = xtest,type="class", s="lambda.min")
glm.pred[glm.prob > 0.5]=1

#create new data frame containing only passenger id and survival prediction
Survived <- c(glm.pred)
PassengerId <- all$PassengerId[892:1309]
prediction <- data.frame(PassengerId,Survived)
#next export to csv file
write.csv(prediction,file="prediction8.csv")


#3rd submission
#sparse=0.996, alpha=0.5, fold=20

#error=16.16 aplha=0.5 sparse=0.995, fold=20
#error=16.04 aplha=0.5 sparse=0.995, fold=30

#4th submission
#error=15.93 aplha=0.5 sparse=0.995, fold=40

#6th submission and 7th submission
#error=16.9 alpha=0.5 sparese=0.999 fold=20 -> best 80% correctly predicted
#error=18.6 alpha=0 sparese=0.999 fold=20

#8th submission
#error=17.39 alpa=1 sparse=0.999 fold=20 -> best 0.80861 correctly predicted 


