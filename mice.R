#get data
T_train <- read.csv("train.csv")
T_test <- read.csv("test.csv")

#variable factors for T_train dataset
T_train$Pclass <- factor(T_train$Pclass)
T_train$Sex <- factor(T_train$Sex)
T_train$Embarked <- factor(T_train$Embarked)

#convert factor in embarked to numeric factor C=1, Q=2, S=3
T_train$new_Embarked <- ifelse(T_train$Embarked=="C",1,NA)
T_train$new_Embarked <- ifelse(T_train$Embarked=="Q",2,T_train$new_Embarked)
T_train$new_Embarked <- ifelse(T_train$Embarked=="S",3,T_train$new_Embarked)

#factor variable new_embarked
T_train$new_Embarked <- factor(T_train$new_Embarked)

str(T_train)

#multiple imputation for Age and embarked with mice package in R
library(mice)

#create new table contain age and new_embarked variable only 
age_emb <- T_train[c(6,13)] 
#multiple imputation for age and new_embarked
age_emb_imp <- mice(age_emb,me=c("pmm","polyreg"))
#check complete data for age and embarked
com_age_emb <- complete(age_emb_imp,1)
#combine all data
combine1 <- cbind(T_train,com_age_emb)
cT_train <- combine1[c(-6,-12,-13)] #final data after imputation is cT_train
#export data to csv format
write.csv(cT_train,file="cT_train.csv")
```