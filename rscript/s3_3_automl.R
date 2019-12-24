# ref : http://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html

# autoML -----------------------------------------------------------------

aml <- h2o.automl(x = setdiff(names(train.hex), "Survived"),
                  y = "Survived",
                  training_frame = train.hex,
                  validation_frame = valid.hex,
                  max_models = 20, 
                  seed = 12345, 
                  max_runtime_secs = 60)

# View the AutoML Leaderboard
lb <- aml@leaderboard 
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)


# The leader model is stored here
aml@leader

# use @ !!
aml@leader@parameters
aml@leader@allparameters
aml@leader@model

# auc
h2o.auc(aml@leader)

# predict(aml, test) also works
pred <- h2o.predict(aml, test.hex)  

# or:
pred <- h2o.predict(aml@leader, test)

# h2o.shutdown(prompt = F)
# end of document ---------------------------------------------------------


