
library(h2o)
h2o.init(nthreads = -1)

# h2o.no_progress()  # Disable progress bars for Rmd

#  Load the MNIST and prepare the data ------------------------------------

train_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/train.csv.gz"
test_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/test.csv.gz"
train <- h2o.importFile(train_file)
test <- h2o.importFile(test_file)
y <- "C785"  #response column: digits 0-9
x <- setdiff(names(test), y)  #vector of predictor column names
# train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])


splits <- h2o.splitFrame(test, 0.5, seed = 1)

# first part of the data, without labels for unsupervised learning
train_unsupervised <- splits[[1]]

# second part of the data, with labels for supervised learning
train_supervised <- splits[[2]]

dim(train_supervised)

dim(train_unsupervised)


# Create Model ------------------------------------------------------------

hidden <- c(128, 64, 128)

ae_model <- h2o.deeplearning(x = x, 
                             training_frame = train_unsupervised,
                             model_id = "mnist_autoencoder",
                             ignore_const_cols = FALSE,
                             activation = "Tanh",  # Tanh is good for autoencoding
                             hidden = hidden,
                             autoencoder = TRUE)
ae_model

# fit1 <- h2o.deeplearning(x = x, y = y,
#                          training_frame = train_supervised,
#                          ignore_const_cols = FALSE,
#                          hidden = hidden,
#                          pretrained_autoencoder = "mnist_autoencoder") # 
# 
# perf1 <- h2o.performance(fit1, newdata = test)
# h2o.mse(perf1) 


# fit2 <- h2o.deeplearning(x = x, y = y,
#                          training_frame = train_supervised,
#                          ignore_const_cols = FALSE,
#                          hidden = hidden)
# 
# perf2 <- h2o.performance(fit2, newdata = test)
# h2o.mse(perf2) # 

# Deep Features -----------------------------------------------------------

# convert train_supervised with autoencoder model to lower-dimensional space
train_reduced_x <- h2o.deepfeatures(ae_model, train_supervised, layer = 1)
dim(train_reduced_x) # 1st layer -> dimension reduction

# Now train DRF on reduced feature space, first need to add response back
train_reduced <- h2o.cbind(train_reduced_x, train_supervised[,y])
train_reduced

rf1 <- h2o.randomForest(x = names(train_reduced_x), y = y, 
                        training_frame = train_reduced,
                        ntrees = 100, seed = 1)
rf1 

test_reduced_x <- h2o.deepfeatures(ae_model, test, layer = 1)
test_reduced <- h2o.cbind(test_reduced_x, test[,y])

rf_perf <- h2o.performance(rf1, newdata = test_reduced)
h2o.mse(rf_perf)


# Anomaly Detection -------------------------------------------------------

# We can also use a deep learning autoencoder to identify outliers in a dataset. 
# The h2o.anomaly() function computes the per-row reconstruction error 
# for the test data set (passing it through the autoencoder model 
# and computing mean square error (MSE) for each row).
test_rec_error <- as.data.frame(h2o.anomaly(ae_model, test)) 
test_recon <- predict(ae_model, test)


# helper functions for display of handwritten digits
# adapted from http://www.r-bloggers.com/the-essence-of-a-handwritten-digit/
plotDigit <- function(mydata, rec_error) {
  len <- nrow(mydata)
  N <- ceiling(sqrt(len))
  par(mfrow = c(N,N), pty = 's', mar = c(1,1,1,1), xaxt = 'n', yaxt = 'n')
  for (i in 1:nrow(mydata)) {
    colors <- c('white','black')
    cus_col <- colorRampPalette(colors = colors)
    z <- array(mydata[i,], dim = c(28,28))
    z <- z[,28:1]
    class(z) <- "numeric"
    image(1:28, 1:28, z, main = paste0("rec_error: ", round(rec_error[i],4)), col = cus_col(256))
  }
}
plotDigits <- function(data, rec_error, rows) {
  row_idx <- sort(order(rec_error[,1],decreasing=F)[rows])
  my_rec_error <- rec_error[row_idx,]
  my_data <- as.matrix(as.data.frame(data[row_idx,]))
  plotDigit(my_data, my_rec_error)
}

# Now we plot the 6 digits with the lowest reconstruction error 
plotDigits(test_recon, test_rec_error, c(1:6))
plotDigits(test, test_rec_error, c(1:6))


# Now we plot the 6 digits with the highest reconstruction error – these are the biggest outliers.
plotDigits(test_recon, test_rec_error, c(9995:10000))
plotDigits(test, test_rec_error, c(9995:10000))


# h2o.shutdown(prompt = F)
# end of document ---------------------------------------------------------







