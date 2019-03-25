# Installed the required packages
gc()
rm(list = ls(all = TRUE))

packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(caret)
packages(caTools)
packages(pROC)
packages(mlbench)
packages(e1071)

# Here we are using Boston Housing data from package 'mlbench'

data("BostonHousing")

df = BostonHousing

str(df)

# Data Partitioning
set.seed(123)
id = sample.split(Y = df$medv, SplitRatio = 0.75)
train_df = subset(df, id == "TRUE")
test_df = subset(df, id == "FALSE")

fit = svm(formula = medv ~., data = train_df,
          type  = 'eps-regression')

pred1 = predict(fit, train_df)

points(train_df$medv, pred1, col = 'blue', pch = 4)

# Lets compute a function to calculate RMSE

rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- fit$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)
predictionRMSE

# Tuning your support vector regression model
# In order to improve the performance of the support vector regression 
#we will need to select the best parameters for the model.

# In our previous example, we performed an epsilon-regression, we did not set any value for epsilon ( ?? ), but it took a default value of 0.1.
# There is also a cost parameter which we can change to avoid overfitting.
# The process of choosing these parameters is called hyperparameter optimization, or model selection.
# The standard way of doing it is by doing a grid search. 
# It means we will train a lot of models for the different couples of ?? and cost, and choose the best one.

# perform a grid search
tuneResult <- tune(svm, medv ~., data = train_df,
                   ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
)
print(tuneResult)
summary(tuneResult)
# Draw the tuning graph
plot(tuneResult)

# On this graph we can see that the darker the region is the better our model is (because the RMSE is closer to zero in darker regions).
# This means we can try another grid search in a narrower range we will try with ?? values between 0 and 0.4. 
# It does not look like the cost value is having an effect for the moment so we will keep it as it is to see if it changes.

# perform a grid search
tuneResult <- tune(svm, medv ~., data = train_df,
                   ranges = list(epsilon = seq(0,0.4,0.01), cost = 2^(2:8))
)
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)

summary(tuneResult)


tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, test_df) 

error = tunedModel$residuals
# this value can be different on your computer
# because the tune method  randomly shuffles the data
tunedModelRMSE <- rmse(error)  # 2.219642  

plot(tunedModelY, test_df$medv)
