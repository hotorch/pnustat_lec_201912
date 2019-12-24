# cross validation --------------------------------------------------------

model_cv <- h2o.gbm(x = setdiff(names(train.hex), "Survived"),
                     y = "Survived",
                     training_frame = train.hex, 
                     validation_frame = valid.hex,
                     ntrees = 200,
                     max_depth = 5,
                     learn_rate = 0.09,
                     col_sample_rate = 0.33,
                     seed = 12345, 
                     nfolds = 5, 
                     keep_cross_validation_predictions = TRUE) 
model_cv

# AUC of cross-validated holdout predictions
h2o.auc(model_fit, xval = TRUE)

# This is where list of cv preds are stored (one element per fold): 
model_cv@model[["cross_validation_predictions"]]

# However you most likely want a single-column frame including all cv preds 
cvpreds <- h2o.getFrame(model_cv@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])
cvpreds


# grid search -------------------------------------------------------------

# GBM Hyperparamters
learn_rate_opt <- c(0.01, 0.03)
max_depth_opt <- c(3, 4, 5, 6, 9)
sample_rate_opt <- c(0.7, 0.8, 0.9, 1.0)
col_sample_rate_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)

hyper_params <- list(learn_rate = learn_rate_opt,
                     max_depth = max_depth_opt,
                     sample_rate = sample_rate_opt,
                     col_sample_rate = col_sample_rate_opt)

search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 3,
                        seed = 1)

gbm_grid <- h2o.grid(algorithm = "gbm",
                     grid_id = "gbm_grid_binomial",
                     x = setdiff(names(train.hex), "Survived"),
                     y = "Survived",
                     training_frame = train.hex,
                     ntrees = 200,
                     seed = 12345,
                     nfolds = 5,
                     fold_assignment = "Modulo",
                     keep_cross_validation_predictions = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)
gbm_grid

# Train a stacked ensemble using the GBM grid
ensemble <- h2o.stackedEnsemble(x = setdiff(names(train.hex), "Survived"),
                                y = "Survived",
                                training_frame = train.hex,
                                validation_frame = valid.hex,
                                model_id = "ensemble_gbm_grid_binomial",
                                base_models = gbm_grid@model_ids)

h2o.auc(gbm_model)
h2o.auc(ensemble)

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = valid.hex)
perf

# h2o.shutdown(prompt = F)
# end of document ---------------------------------------------------------
