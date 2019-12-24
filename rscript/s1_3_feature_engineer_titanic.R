
# Load Library -----------------------------------------------------------
library(data.table)
library(dplyr)
library(funModeling)
library(rpart)

# Load Data ---------------------------------------------------------------
# srvived    : 생존여부(1: 생존, 0 : 사망)
# pclass      : 승선권 클래스(1 : 1st, 2 : 2nd ,3 : 3rd)
# name        : 승객 이름
# sex         : 승객 성별
# age         : 승객 나이 
# sibsp       : 동반한 형제자매, 배우자 수
# patch       : 동반한 부모, 자식 수
# ticket      : 티켓의 고유 넘버
# fare        : 티켓의 요금
# cabin       : 객실 번호
# embarked    : 승선한 항구명(C : Cherbourg, Q : Queenstown, S : Southampton)

#train <- read.csv("./data/titanic_train.csv", 
#                  stringsAsFactors = FALSE, 
#                  na.strings=c("","NA"))

train <- fread("./data/titanic_train.csv", 
                stringsAsFactors = FALSE, 
                na.strings = c("", NA))

#test <- read.csv("./data/titanic_test.csv", 
#                 stringsAsFactors = FALSE, 
#                 na.strings=c("","NA"))

test <- fread("./data/titanic_test.csv", 
               stringsAsFactors = FALSE, 
               na.strings = c("", NA))


df_combined <- bind_rows(train, test) 


# find missing value --------------------------------------------------------
colSums(is.na(df_combined))

# Confirming the columns with missing data using a different technique 
miss_cols <- colnames(df_combined)[colSums(is.na(df_combined)) > 0]
miss_cols

# 1. Fare var - missing value
subset(df_combined, is.na(Fare) == TRUE)
which(is.na(df_combined$Fare))

# 
df_combined$Fare[1044] <- median(df_combined[df_combined$Pclass == '3' & df_combined$Embarked == 'S', ]$Fare, na.rm = TRUE)


# 2. Age - missing value
sum(is.na(df_combined$Age))
# nrow(subset(df_combined, is.na(Age) == TRUE))
set.seed(930203)

# using decision tree
age_model <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked , 
                   data = df_combined[!is.na(df_combined$Age),], method ="anova") 

df_combined$Age[is.na(df_combined$Age)] <- predict(age_model, df_combined[is.na(df_combined$Age),]) 

# 3. Embarked - missing value
sum(is.na(df_combined$Embarked))
# subset(df_combined, is.na(Embarked) == TRUE) 

table(df_combined$Embarked)
table(subset(df_combined, Fare >= 80, select = c("Embarked"))) 
df_combined$Embarked[c(62, 830)] <- 'C'

# 4. Cabin - missing value
sum(is.na(df_combined$Cabin))
# nrow(subset(df_combined, is.na(Cabin) == TRUE))

miss_cols <- colnames(df_combined)[colSums(is.na(df_combined)) > 0]
miss_cols


# Feature Engineering - 1. Name  ------------------------------------------------------
# regexpr
# df_combined$Title <- gsub('(.*, )|(\\..*)', '', df_combined$Name)
df_combined[, Title := gsub('(.*, )|(\\..*)', '', df_combined$Name)]
tab <- prop.table(table(df_combined$Title))
df <- data.frame(Title=names(tab), proportion=as.numeric(tab))
df <- df[order(-df$proportion),]
df 

# proportion < 0.04 -> other
other_title <- c('Dr','Rev','Col',
                 'Major','Capt','Don',
                 'Dona','Jonkheer','Lady',
                 'Sir','the Countess')
df_combined$Title[df_combined$Title == 'Mlle'] <- 'Miss' # 'miss' = 'mlle' in france 
df_combined$Title[df_combined$Title == 'Ms'] <- 'Miss' 
df_combined$Title[df_combined$Title == 'Mme'] <- 'Mrs' # 'miss' = 'mlle' in france 
df_combined$Title[df_combined$Title %in% other_title] <- 'Other' 

# df_combined[Title == 'Mrs', 13] <- 'Mrs'
table(df_combined$Title)


# Feature Engineering - 2. Family ------------------------------------------------------------------------
df_combined$Family_Member_Count <- df_combined$SibSp + df_combined$Parch + 1 

# df_combined$Parent <- 'Not Parent'
# df_combined$Parent[df_combined$Sex == 'female' & df_combined$Parch > 0 & df_combined$Age > 18 & df_combined$Title != 'Miss'] <- 'Mother'
# df_combined$Parent[df_combined$Sex == 'male' & df_combined$Parch > 0 & df_combined$Age > 18] <- 'Father'

df_combined[, Parent := 'Not Parent']
df_combined[df_combined$Sex == 'female' & df_combined$Parch > 0 & df_combined$Age > 18 & df_combined$Title != 'Miss', Parent := 'Mother']
df_combined[df_combined$Sex == 'male' & df_combined$Parch > 0 & df_combined$Age > 18, Parent := 'Father']



# export data -------------------------------------------------------------

write.table(df_combined, "./data/df_combined.csv",
          col.names = TRUE, na = "NA",
          row.names = FALSE, sep = ",")


rm(list = ls()) ; gc()


# end of document ---------------------------------------------------------

