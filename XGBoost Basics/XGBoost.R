# XGBoost Basics
# 1. XGBoost algorithm is better than random forests and neural networks in terms of accuracy, efficiency and feasibility 
# 2. Users need not spend more time feature engineering as this algorithm helps to save time and effort
# 3. XGBoost stands for eXtreme Gradient Boosting 
# 4. It can do parallel computing on a single machine
# 5. This algorithm works only with numeric vectors
# 6. If the dataset consists of categorical variables, then the user can convert such attributes into numerical data by one hot coding method
# 7. This is an example of xgboost model using the iris data available in base R.
# 8. Predict the Species from the 4 features of iris data.
# 9. The data contains numeric predictors. Our target column is Species, with 3 classes.

# Note: This uses a two step process.
# Step 1 performs cross-validation to find the number of iterations needed to get the minimum loss.
# Step 2 creates the final model using the nround identified in Step 1, and makes the prediction.

rm(list=ls()) # clear all environment variables

library(xgboost) # xgboost library
library(caret) # for confusion matrix
library(e1071)

#Check the data structure
data(iris)
print(str(iris))

set.seed(100)

#Split the iris data into training (75%) and testing(30%).
smp_size <- floor(0.75 * nrow(iris))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(iris)), size = smp_size)

train <- iris[train_ind, ]
test <- iris[-train_ind, ]

# Set the parameters for cross-validation and xgboost.
# Note: This is a multi-class classification problem, and the evaluation metric is "mlogloss".
#      The same parameters are used by Step 1 and Step 2.
#      You can try different values for nthread, max_depth, eta, gamma, etc., and see if you get lower prediction error.

param       <- list("objective" = "multi:softmax", # multi class classification
                   "num_class"= 3 ,  		# Number of classes in the dependent variable.
                   "eval_metric" = "mlogloss",  	 # evaluation metric 
                   "nthread" = 8,   			 # number of threads to be used 
                   "max_depth" = 16,    		 # maximum depth of tree 
                   "eta" = 0.3,    			 # step size shrinkage 
                   "gamma" = 0,    			 # minimum loss reduction 
                   "subsample" = 0.7,    		 # part of data instances to grow tree 
                   "colsample_bytree" = 1, 		 # subsample ratio of columns when constructing each tree 
                   "min_child_weight" = 12  		 # minimum sum of instance weight needed in a child 
)

# Identify the Predictors and the dependent variable, aka label.
predictors = colnames(train[-ncol(train)])

# xgboost works only if the labels are numeric. Hence, convert the labels (Species) to numeric.
label = as.numeric(train[,ncol(train)])
print(table (label))

# Alas, xgboost works only if the numeric labels start from 0. Hence, subtract 1 from the label.
label = as.numeric(train[,ncol(train)])-1
print(table (label))

#########################################################################################################
# Step 1: Run a Cross-Validation to identify the round with the minimum loss or error.
# Note: xgboost expects the data in the form of a numeric matrix.

set.seed(100)

cv.nround <- 200;  # Number of rounds. This can be set to a lower or higher value, if you wish, example: 150 or 250 or 300  
bst.cv <- xgb.cv( # xgb.cv is xgboost cross validation function
  param=param,
  data = as.matrix(train[,predictors]),
  label = label,
  nfold = 3,
  nrounds=cv.nround,
  prediction=T)

#Find where the minimum logloss occurred
min_loss_idx <- which.min(bst.cv$pred[, bst.cv$evaluation_log$test_mlogloss_mean]) 
cat ("Minimum logloss occurred in round : ", min_loss_idx, "\n")

# Minimum logloss
print(bst.cv$pred[min_loss_idx,])

# Step 2: Train the xgboost model using min.loss.idx found above.
#         Note, we have to stop at the round where we get the minumum error.
set.seed(100)

bst = xgboost(
  param=param,
  data =as.matrix(train[,predictors]),
  label = label,
  nrounds=min_loss_idx)

# Make prediction on the testing data.
test$prediction = predict(bst, as.matrix(test[,predictors]))

#Translate the prediction to the original class or Species.
test$prediction = ifelse(test$prediction==0,"setosa",ifelse(test$prediction==1,"versicolor","virginica"))

#Compute the accuracy of predictions.
confusionMatrix(test$prediction,test$Species)
