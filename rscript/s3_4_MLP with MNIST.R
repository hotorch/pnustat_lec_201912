

library(h2o)
h2o.init(nthreads = -1)

#  Load the MNIST and prepare the data ------------------------------------
# This step takes a few seconds bc we have to download the data from the internet...
# train_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/train.csv.gz"
test_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/test.csv.gz"
# train <- h2o.importFile(train_file)
test <- h2o.importFile(test_file)
dim(train);dim(test)

y <- "C785"  #response column: digits 0-9 / 1 ~ 784
x <- setdiff(names(test), y)  # vector of predictor column names

# Since the response is encoded as integers, we need to tell H2O that
# the response is in fact a categorical/factor column.  Otherwise, it 
# will train a regression model instead of multiclass classification.
# train[,y] <- as.factor(train[,y]) 
test[,y] <- as.factor(test[,y])


# Modeling ----------------------------------------------------------------

dl_fit1 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = test,
                            model_id = "dl_fit1",
                            hidden = c(20,20),
                            seed = 1)
dl_fit1

# too much spend time
dl_fit2 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = test,
                            model_id = "dl_fit2",
                            epochs = 50,
                            hidden = c(20,20),
                            stopping_rounds = 0,  # disable early stopping
                            seed = 1)
dl_fit2

# recommend!!!
dl_fit3 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = test,
                            model_id = "dl_fit3",
                            epochs = 50,
                            hidden = c(20,20),
                            nfolds = 3,                            # used for early stopping
                            score_interval = 1,                    # used for early stopping
                            stopping_rounds = 5,                   # used for early stopping
                            stopping_metric = "misclassification", # used for early stopping
                            stopping_tolerance = 1e-3,             # used for early stopping
                            seed = 1)


# Evaluation ----------------------------------------------------------------

# dl_perf1 <- h2o.performance(model = dl_fit1, newdata = test)
# dl_perf2 <- h2o.performance(model = dl_fit2, newdata = test)
dl_perf3 <- h2o.performance(model = dl_fit3, newdata = test)

# Retreive test set MSE
# h2o.mse(dl_perf1) # 0.01881584
# h2o.mse(dl_perf2) # 0.00155582
# h2o.mse(dl_perf3) # 0.0002706472

# python ~= report 
h2o.scoreHistory(dl_fit3)
h2o.confusionMatrix(dl_fit3)

# plot a model ----------------------------------------------------------------

# We can also “plot a model”, which will graph the performance of some metric over the training process.
plot(dl_fit3, 
     timestep = "epochs", 
     metric = "classification_error")


# Get the CV models from the `dl_fit3` object
cv_models <- sapply(dl_fit3@model$cross_validation_models, 
                    function(i) h2o.getModel(i$name))

# Plot the scoring history over time
plot(cv_models[[1]], 
     timestep = "epochs", 
     metric = "classification_error")


# Deep Learning Grid Search -----------------------------------------------
# do not run!! 
activation_opt <- c("Rectifier", "Maxout")# , "Tanh")
l1_opt <- c(0, 0.001)
l2_opt <- c(0, 0.001)
# l1_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01)
# l2_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01)

hyper_params <- list(activation = activation_opt, l1 = l1_opt, l2 = l2_opt)
search_criteria <- list(strategy = "RandomDiscrete", max_runtime_secs = 600) # 10 min

splits <- h2o.splitFrame(test, ratios = 0.8, seed = 1)

dl_grid <- h2o.grid("deeplearning", x = x, y = y,
                    grid_id = "dl_grid",
                    training_frame = splits[[1]],
                    validation_frame = splits[[2]],
                    seed = 1,
                    hidden = c(20,20),
                    hyper_params = hyper_params,
                    search_criteria = search_criteria)

dl_gridperf <- h2o.getGrid(grid_id = "dl_grid", 
                           sort_by = "accuracy", 
                           decreasing = TRUE)
print(dl_gridperf)



# Best Model --------------------------------------------------------------

# Grab the model_id for the top DL model, chosen by validation error.
best_dl_model_id <- dl_gridperf@model_ids[[1]]
best_dl <- h2o.getModel(best_dl_model_id)

best_dl_perf <- h2o.performance(model = best_dl, newdata = test)
best_dl_perf 

h2o.mse(best_dl_perf)

# Reference ---------------------------------------------------------------
# https://htmlpreview.github.io/?https://github.com/ledell/sldm4-h2o/blob/master/sldm4-deeplearning-h2o.html


# h2o.shutdown(prompt = F)
# end of document ---------------------------------------------------------


