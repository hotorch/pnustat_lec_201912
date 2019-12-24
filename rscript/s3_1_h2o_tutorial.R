
# Load Library -----------------------------------------------------------
library(data.table)
library(dplyr)
library(h2o)
h2o.init(nthreads = -1)

# read data ---------------------------------------------------------------
df_combined <- fread('./data/df_combined.csv', stringsAsFactors = FALSE)


# update var type ---------------------------------------------------------
# remove var : PassengerId, Name, Ticket, Cabin
# fact_var : Survived, Pclass, Sex, Embarked, Title, Parent
# numeric : Age, SibSp, Parch, Fare, Family_Member_Count

rm_var <- c("PassengerId", "Name", "Ticket", "Cabin")
# "Survived"
fact_var <- c("Survived","Pclass", "Sex", "Embarked", "Title", "Parent")
num_var <- setdiff(names(df_combined), c(fact_var, rm_var)) 

final_dat <- df_combined 
final_dat[, (fact_var) := lapply(.SD, as.factor), .SDcols = fact_var]
final_dat[, (num_var) := lapply(.SD, as.numeric), .SDcols = num_var]
final_dat[, (rm_var) := lapply(.SD, as.null), .SDcols = rm_var]

# split Train & test set  ----------------------------------------------
test <- final_dat[is.na(final_dat$Survived),]
train <- final_dat[!is.na(final_dat$Survived),]

# split train & valid -----------------------------------------------------
set.seed(12345)

idx <- sample(nrow(train), nrow(train)*0.8)

train_dat <- train[idx]
valid_dat <- train[-idx]


# h2o start ---------------------------------------------------------------

train.hex <- as.h2o(train_dat)
valid.hex <- as.h2o(valid_dat)
test.hex <- as.h2o(test[,-"Survived"])

# training  ------------------------------------------------------------
?h2o.randomForest
rf_model <- h2o.randomForest(x = setdiff(names(train.hex), "Survived"),
                          y = "Survived",
                          training_frame = train.hex, 
                          validation_frame = valid.hex,
                          ntrees = 200,
                          max_depth = 5,
                          mtries = 3,
                          seed = 12345) 
rf_model

?h2o.gbm
gbm_model <- h2o.gbm(x = setdiff(names(train.hex), "Survived"),
                     y = "Survived",
                     training_frame = train.hex, 
                     validation_frame = valid.hex,
                     ntrees = 200,
                     max_depth = 5,
                     learn_rate = 0.09,
                     col_sample_rate = 0.33,
                     seed = 12345)
gbm_model

# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/xgboost.html 

xg_model <- h2o.xgboost(x = setdiff(names(train.hex), "Survived"),
                        y = "Survived",
                        training_frame = train.hex, 
                        validation_frame = valid.hex,
                        ntrees = 200,
                        max_depth = 5,
                        learn_rate = 0.09,
                        col_sample_rate = 0.33,
                        seed = 12345)


lgb_model <- h2o.xgboost(x = setdiff(names(train.hex), "Survived"),
                         y = "Survived",
                         training_frame = train.hex, 
                         validation_frame = valid.hex,
                         ntrees = 200,
                         max_depth = 5,
                         learn_rate = 0.09,
                         col_sample_rate = 0.33,
                         tree_method="hist", 
                         grow_policy="lossguide",
                         seed = 12345)

# predict & auc & confusion matrix -----------------------------------------------------------------
rf_pred <- h2o.predict(rf_model, test.hex)
gbm_pred <- h2o.predict(gbm_model, test.hex)

h2o.confusionMatrix(rf_model)
h2o.confusionMatrix(gbm_model)

h2o.auc(rf_model)
h2o.auc(gbm_model)

h2o.varimp(rf_model)
h2o.varimp(gbm_model)

h2o.varimp_plot(rf_model)
h2o.varimp_plot(gbm_model)

# lift --------------------------------------------------------------------

h2o.gainsLift(rf_model, test.hex)
h2o.gainsLift(gbm_model, test.hex)


# h2o.shutdown(prompt = F)
# end of document ---------------------------------------------------------

