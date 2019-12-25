library(caret) # http://topepo.github.io/caret/available-models.html
library(lime)
library(data.table)
library(dplyr)

# ref code s3_1_h2o_tutorial ----------------------------------------------
# read data ---------------------------------------------------------------
df_combined <- fread('./data/df_combined.csv', stringsAsFactors = FALSE)


# update var type ---------------------------------------------------------
# remove var : PassengerId, Name, Ticket, Cabin
# fact_var : Survived, Pclass, Sex, Embarked, Title, Parent
# numeric : Age, SibSp, Parch, Fare, Family_Member_Count
rm_var <- c("PassengerId", "Name", "Ticket", "Cabin")
fact_var <- c("Pclass", "Sex", "Embarked", "Title", "Parent", "Survived")
num_var <- setdiff(names(df_combined), c(fact_var, rm_var)) 

final_dat <- df_combined 
final_dat[, (fact_var) := lapply(.SD, factor), .SDcols = fact_var]
final_dat[, (num_var) := lapply(.SD, as.numeric), .SDcols = num_var]
final_dat[, (rm_var) := lapply(.SD, as.null), .SDcols = rm_var]


# split Train & test set  ----------------------------------------------
test_dat <- final_dat[is.na(final_dat$Survived),]
train_dat <- final_dat[!is.na(final_dat$Survived),]

target <- as.factor(train_dat$Survived)
levels(target) <- c("Not_Survive", "Survive")


# Control paramters for train with CV ---------------------------------------------

myControl <- trainControl(
  method = "repeatedcv",
  number = 3,
  repeats = 1,
  classProbs = TRUE, 
  verboseIter = FALSE # print log 
)


# train model -----------------------------------------------

model <- train(train_dat[,-1], target,
               method = 'rf', 
               metric = "Accuracy", 
               ntree = 200, 
               trControl = myControl)

# tune grid reference : https://lovetoken.github.io/r/machinelearning/2017/04/23/caret_package.html

# preds <- predict(model, test_dat, type = "raw")
preds <- predict(model, test_dat, type = "prob")


# LIME 패키지 이용¶ --------------------------------------------------------------------
explainer <- lime(train_dat[,-1], model) 
explanation <- explain(test_dat[c(1,5,7,16), -1],
                       explainer, 
                       n_labels = 1, 
                       n_features = 10)


plot_features(explanation) 
plot_explanations(explanation)


# image lime --------------------------------------------------------------


# Having trouble to install imagemick in version 6.8.8 or higher on TravisCI, 
# which would be required for this code. So running only locally and added the
# image manually.
# For running locally, set eval = TRUE and make sure lime is installed.
library("lime")
explanation <- .load_image_example()
plot_image_explanation(explanation)


# text lime --------------------------------------------------------------------

# https://github.com/marcotcr/lime/blob/master/doc/notebooks/Lime%20-%20basic%20usage%2C%20two%20class%20case.ipynb

# Reference ---------------------------------------------------------------

# LIME interpretablilty
# https://stats.stackexchange.com/questions/353322/h2o-interpretability-lime

# code 
# ref : https://uc-r.github.io/lime#fn:lime_paper

# hanguel / theory
# ref : https://rpubs.com/stat17_hb/442427

# official pdf
# ref : https://cran.r-project.org/web/packages/lime/lime.pdf



# end of document ---------------------------------------------------------


