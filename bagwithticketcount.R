all_complete <- read.csv(("all_complete_titanic.csv"))
all_complete <- all_complete[c(-1)]

#########################################
#variable Ticket
#1. create variable ticketcount, is number of passengers that have the same ticket number
all_complete$TicketCount<-apply(all_complete, 1, function(x) sum(as.numeric(all_complete$Ticket == x["Ticket"])))

#2. create variable ticket_room_number and ticket_category
# ticket_room_number is first digit from ticket number before five last digits
# ticket_category is the first last digits of ticket number

#delete all punctuation
library(qdap)
sent_detect(all_complete$Ticket)
nopunc <- gsub("[?.;!¡¿·'/=,]", "", all_complete$Ticket)
df <- as.data.frame(nopunc)
df <- cbind(df,all_complete)
df <- df[c(-2,-4:-15)]


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
library(stringr)
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

ticket_split$ticket_room_number <- as.numeric(ticket_split$ticket_room_number)
ticket_split$ticket_category <- as.factor(ticket_split$ticket_category)

#join table
alls <- cbind(all_complete,ticket_split)
alls <- alls[c(-14:-15)]
write.csv(alls,file="alls.csv")

#ticket_room_number to be factor



#factor ticket_category and ticket_room_number
alls$ticket_room_number <- as.numeric(alls$ticket_room_number)
alls$ticket_category <- factor(alls$ticket_category)
levels(alls$ticket_category)
levels(alls$ticket_room_number)
#convert/rename all 41 different ticket category 1=1, 2=2, 3=3, 31=4, etc
library(plyr)
alls$ticket_category_rename <- mapvalues(alls$ticket_category,from=c("","1","2","3","31","A2","A4", "A5","AQ3","AQ4",
                                           "AS","C", "CA","CASOTON","Fa2","FC","FCC","LP","PC","PP",
                                           "PPP","SC","SCA3","SCA4","SCAH","SCAHBasle","SCOW","SCParis","SCPARIS","SOC",
                                           "SOP","SOPP","SOTONO231","SOTONOQ3","SOTONOQ31","SP","STONO231","STONOQ3","SWPP","WC",
                                           "WEP"),to=
                                         c(NA,"1","2","3","3","4","5","6","7","8",
                                           "6","9","9","10","11","12","12","13","14","15",
                                           "15","16","17","18","19","20","21","22","22","23",
                                           "24","24","25","26","26","27","25","26","27","28",
                                           "29"))

# alls$ticket_category_rename <- mapvalues(alls$ticket_category,from=c("","1","2","3","31","A2","A4", "A5","AQ3","AQ4",
#                                            "AS","C", "CA","CASOTON","Fa2","FC","FCC","LP","PC","PP",
#                                            "PPP","SC","SCA3","SCA4","SCAH","SCAHBasle","SCOW","SCParis","SCPARIS","SOC",
#                                            "SOP","SOPP","SOTONO231","SOTONOQ3","SOTONOQ31","SP","STONO231","STONOQ3","SWPP","WC",
#                                            "WEP"),to=
#                  c(NA,"1","2","3","4","5","6","7","8","9",
#                    "7","10","11","12","13","14","15","16","17","18",
#                    "19","20","21","22","23","24","25","26","26","27",
#                    "28","29","30","31","32","33","34","35","36","37",
#                    "38"))
as.numeric(alls$ticket_category_rename)
alls$ticket_category_rename <- factor(alls$ticket_category_rename)
#multiple imputation for NA in ticke_room_number
library(mice)
#create new table contain ticket_room_number and ticket_category variable only 
tix <- alls[c(14,16)] 
#multiple imputation 
tix_imp <- mice(tix,me=c("sample","polyreg"))
#check complete data for age, fare, and embarked
com_tix <- complete(tix_imp,1)
#combine all data
combine_tix <- cbind(alls,com_tix)
c_all <- combine_tix[c(-14:-16)] #final data after imputation in dataset
summary(c_all)
#export data to csv format
write.csv(c_all,file="alls.csv")
#########################################

alls <- read.csv(("alls.csv")) #new alls, different compared to previous one, this one with complete ticket_number_room


#factor variables
alls$Pclass <- factor(alls$Pclass)
alls$Sex <- factor(alls$Sex)
alls$new_Embarked <- factor(alls$new_Embarked)
alls$ticket_category_rename <- factor(alls$ticket_category_rename)
alls$SibSp <- as.numeric(alls$SibSp)
alls$Parch <- as.numeric(alls$Parch)
alls$ticket_room_number <- as.numeric(alls$ticket_room_number)

str(alls)



#text mining
require(tm)

# build a corpus
alls.corpus <- Corpus(VectorSource(alls$Name))

# make each letter lowercase
alls.corpus <- tm_map(alls.corpus, content_transformer(tolower)) 

# remove punctuation 
alls.corpus <- tm_map(alls.corpus, removePunctuation)

# remove generic and custom stopwords
# my_stopwords <- c(stopwords('english'), 'prolife', 'prochoice')
# cT.corpus <- tm_map(cT.corpus, removeWords, my_stopwords)

# build a term-document matrix
alls.dtm <- TermDocumentMatrix(alls.corpus)

# inspect the document-term matrix
alls.dtm

# inspect most popular words
findFreqTerms(alls.dtm, lowfreq=30)

alls.dtm2 <- removeSparseTerms(alls.dtm, sparse=0.999)

bag.df <- t(as.data.frame(inspect(alls.dtm2)))

#divide bag.df to train and test
bag_train <- bag.df[1:891,]
bag_test <- bag.df[892:1309,]

#divide alls to train and test dataset
titanic_train <- alls[1:891,]
titanic_test <- alls[892:1309,]

#remove unwanted predictors in train and test data
cT_train <- titanic_train[c(-1,-2,-5,-9,-10)] 
cT_test <- titanic_test[c(-1,-2,-5,-9,-10)]

# #factor Pclass,new_embarked for cT_train and cT_test
# cT_train$Pclass <- factor(cT_train$Pclass)
# cT_train$new_Embarked <- factor(cT_train$new_Embarked)
# cT_train$ticket_category_rename <- factor(cT_train$ticket_category_rename)
# 
# 
# cT_test$Pclass <- factor(cT_test$Pclass)
# cT_test$new_Embarked <- factor(cT_test$new_Embarked)
# cT_test$ticket_category_rename <- factor(cT_test$ticket_category_rename)


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
cv.fit <- cv.glmnet(x,y,alpha=1,family="binomial", type.measure = "class",nfolds = 10)
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
PassengerId <- alls$PassengerId[892:1309]
prediction <- data.frame(PassengerId,Survived)
#next export to csv file
write.csv(prediction,file="prediction17.csv")

#error= 0.1705948 sparse=0.999 alpha=0.5 fold=20 ->  0.79426 correctly predicted
#error= 0.1907969 sparse=0.999 alpha=0 fold=20 

#10th submission
#error= 0.1739618 sparse=0.999 alpha=1 fold=20-> 0.80861 correctly predicted


#error= 0.1784512 sparse=0.997 alpha=1 fold=20-> correctly predicted
