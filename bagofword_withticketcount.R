#Preprocessing data

###################################################################
#train and test data is combined
#get combined data
all <- read.csv(("all.csv"))

#factoring variables
all$Pclass <- factor(all$Pclass)
all$Sex <- factor(all$Sex)
all$Embarked <- factor(all$Embarked)

#########################################
#variable Ticket

#1. create variable ticketcount, is number of passengers that have the same ticket number
all$TicketCount<-apply(all, 1, function(x) sum(as.numeric(all$Ticket == x["Ticket"])))

#2. create variable ticket_room_number and ticket_category
# ticket_room_number is first digit from ticket number before five last digits
# ticket_category is the first last digits of ticket number

#delete all punctuation
library(qdap)
sent_detect(all$Ticket)
nopunc <- gsub("[?.;!¡¿·'/=,]", "", all$Ticket)
df <- as.data.frame(nopunc)
df <- cbind(df,all)
df <- df[c(-3:-14)]


#split variable by space
df$nopunc <- as.character(df$nopunc)
df.split <- strsplit(df$nopunc,split = " ")
tmp <- do.call(rbind,df.split)
cek <- data.frame(df,tmp)

#1. divide 1 column AND (2 or 3) column 
c <- cek[as.character(cek$X1) == as.character(cek$X2),] #all 3 col are the same
d <- cek[as.character(cek$X1) != as.character(cek$X2),] #only col 1 equal to col 3, and 
#AND col 1 <> col 2 <> col 3

#2. manipulate d
#2.1 manipulate e
e <- d[as.character(d$X1) != as.character(d$X3),]
e$join12 <- paste(e$X1, e$X2,sep="")
e <- e[c(-3,-4)] 
colnames(e) <- c("nopunc","PassengerId","X2","X1") #e is done
#take first digits before five last digit of e$X2
e$var1 <- sapply(e$X2, function(x) str_sub(x,1,max(str_length(x)-5, 0)))
#take 5 last digit as var2 and previous digits as var1
e$var2 <- sapply(e$X2, function(x) str_sub(x,max(1,str_length(x)-4),str_length(x)))
#join X1 and var1
e$joinx1var1 <- paste(e$X1,e$var1,sep="")
e <- e[c(-3:-5)]
colnames(e) <- c("nopunc","PassengerId", "ticket_room_number","ticket_category")

#2.2 manipulate f
f<- d[as.character(d$X1) == as.character(d$X3),]
f <- f[c(-5)] #f is done
#take first digits before five last digit of f$X2
f$var1 <- sapply(f$X2, function(x) str_sub(x,1,max(str_length(x)-5, 0)))
#take 5 last digit as var2 and previous digits as var1
f$var2 <- sapply(f$X2, function(x) str_sub(x,max(1,str_length(x)-4),str_length(x)))
#join X1 and var1
f$joinx1var1 <- paste(f$X1,f$var1,sep="")
f <- f[c(-3:-5)]
colnames(f) <- c("nopunc","PassengerId","ticket_room_number","ticket_category")

#3. manipulate c
c <- c[c(-4,-5)]
#take first digits before five last digit
c$var1 <- sapply(c$X1, function(x) str_sub(x,1,max(str_length(x)-5, 0)))
#take 5 last digit as var2 and previous digits as var1
c$var2 <- sapply(c$X1, function(x) str_sub(x,max(1,str_length(x)-4),str_length(x)))
c <- c[c(-3)]
colnames(c) <- c("nopunc","PassengerId","ticket_category","ticket_room_number")

#join e, f, and c
ticket_var <- rbind(e,f,c)
ticket_split <- ticket_var[order(ticket_var$PassengerId),]

as.numeric(ticket_split$ticket_room_number)
as.factor(ticket_split$ticket_category)

#join table
alls <- cbind(all,ticket_split)
alls <- alls[c(-14:-15)]
#########################################

#variable Embarked
#convert factor in embarked to numeric factor C=1, Q=2, S=3
all$new_Embarked <- ifelse(all$Embarked=="C",1,NA)
all$new_Embarked <- ifelse(all$Embarked=="Q",2,all$new_Embarked)
all$new_Embarked <- ifelse(all$Embarked=="S",3,all$new_Embarked)
#factor variable new_embarked
all$new_Embarked <- factor(all$new_Embarked)

str(all)

#multiple imputation for Age and new_embarked with mice package in R
library(mice)
#create new table contain age, fare, and new_embarked variable only 
age_emb <- all[c(6,10,13)] 
#multiple imputation for age, fare, and new_embarked
age_emb_imp <- mice(age_emb,me=c("pmm","pmm","polyreg"))
#check complete data for age, fare, and embarked
com_age_emb <- complete(age_emb_imp,1)
#combine all data
combine1 <- cbind(all,com_age_emb)
c_all <- combine1[c(-6,-10,-12,-13)] #final data after imputation in dataset
summary(c_all)
#export data to csv format
write.csv(c_all,file="c_all.csv")

#bagofword (include variable name)

#text mining
require(tm)

# build a corpus
c_all.corpus <- Corpus(VectorSource(c_all$Name))

# make each letter lowercase
c_all.corpus <- tm_map(c_all.corpus, content_transformer(tolower)) 

# remove punctuation 
c_all.corpus <- tm_map(c_all.corpus, removePunctuation)

# remove generic and custom stopwords
# my_stopwords <- c(stopwords('english'), 'prolife', 'prochoice')
# cT.corpus <- tm_map(cT.corpus, removeWords, my_stopwords)

# build a term-document matrix
c_all.dtm <- TermDocumentMatrix(c_all.corpus)

# inspect the document-term matrix
c_all.dtm

# inspect most popular words
findFreqTerms(c_all.dtm, lowfreq=30)

c_all.dtm2 <- removeSparseTerms(c_all.dtm, sparse=0.999)

bag.df <- t(as.data.frame(inspect(c_all.dtm2)))

#divide bag.df to train and test
bag_train <- bag.df[1:891,]
bag_test <- bag.df[892:1309,]

#divide all to train and test dataset
titanic_train <- c_all[1:891,]
titanic_test <- c_all[892:1309,]

#remove unwanted predictors in train and test data
cT_train <- titanic_train[c(-1,-4,-8,-9)]
cT_test <- titanic_test[c(-1,-4,-8,-9)]

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

#export titanic_train and titanic_test to csv format
write.csv(titanic_train,file="titanic_train_ticketcount.csv")
write.csv(titanic_test,file="titanic_test_ticketcount.csv")

###################################################################

#glmnet

library(boot)
library(glmnet)
set.seed(123)
#formula from data
f <- as.formula(Survived~.+log(Age)+Pclass*SibSp+Pclass*Sex+Parch*Pclass+SibSp*Parch+Parch*Sex+log(Age)+Age*Sex*Pclass+Age*Parch*Pclass+Age*SibSp*Pclass+Age*Fare*Pclass+Sex*Fare*Pclass)

#transform data to matrices required by glmnet
x <- model.matrix(f,c_train)
y <- as.matrix(c_train$Survived,ncol=1)
cv.fit <- cv.glmnet(x,y,alpha=0.5,family="binomial", type.measure = "class",nfolds = 20)
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
write.csv(prediction,file="prediction6.csv")

