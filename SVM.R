# Lets installed the required libraries for this problem

gc()
rm(list = ls(all = TRUE))

packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(e1071)
packages(caTools)
packages(caret)

# We will be considering iris data set for practicing this problem
# SInce iris is the in build data availble in R, we don't need to set up directory for this.
str(iris)

# Lets create the model here

m1 = svm(Species ~., data = iris)
summary(m1)

# If we look at the summary of the model, we will find that the kernel used is 'radial'.
# the cost is 1 and gamma is 0.25. Cost c, Gamma are the parameter we can tune to make the best model
# Lets plot the m1 first

plot(m1, data = iris,
     Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))
# Lets prepare a confusion matrix
pred = predict(object = m1, newdata = iris)

confusionMatrix(pred,iris$Species)

# As I have mentioned above, Kernel used in this problem is 'radial'. We can change the kernel which is suitable to our problem and tune the model as well.
# Lets use linear as kernel

m2 = svm(Species ~., data = iris, kernel = 'linear')
summary(m2)

plot(m2, data = iris,
     Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))
# Lets prepare a confusion matrix
pred = predict(object = m2, newdata = iris)

confusionMatrix(pred,iris$Species)

# Misclassification has increase after chnaging the kernel to linear, lets change it to polinomial

m3 = svm(Species ~., data = iris, kernel = 'polynomial')
summary(m3)

plot(m3, data = iris,
     Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))
# Lets prepare a confusion matrix
pred = predict(object = m3, newdata = iris)

confusionMatrix(pred,iris$Species)

# Even after chnaging the kernel to polynomial, the misclassification has been increase.Lets try sigmodi as well

m4 = svm(Species ~., data = iris, kernel = 'sigmoid')
summary(m4)

plot(m4, data = iris,
     Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))
# Lets prepare a confusion matrix
pred = predict(object = m4, newdata = iris)

confusionMatrix(pred,iris$Species)

# Lets do the Hyper Parameter optimization ( Hyper parameter Tuning)

set.seed(123)
tuned_model = tune(svm, Species ~., data = iris, 
     ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
# epsilon value will be like 0,0.1, 0.2, 0.3, ..., 1.0
# cost - this is the cost of constraint violetion, if is it more, there will be chances of overfitting the model. Less cost can be responsible for underfitting

# Lets plot the tuned model

plot(tuned_model)

# The simplest way to understand the plot is to see the darker shed of the colour. Select the cost value where the colour is very dark.
# Darker region (In the plot) = Lower misclassification error
# In this case, I can see, dark shed of the colour is within cost is 200.
# Lets consider 200 as cost and check the tuned model again, just change 2^ to 8

set.seed(123)
tuned_model = tune(svm, Species ~., data = iris, 
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:8)))
# epsilon value will be like 0,0.1, 0.2, 0.3, ..., 1.0
# cost - this is the cost of constraint violetion, if is it more, there will be chances of overfitting the model. Less cost can be responsible for underfitting
# The epsilon values gives us total 11 values and cost gives us total 7 values, which is making a total combination of 77.
# Since we have only 150 data points in the data, its OK to have 77 combinations, in case of the bigger data, we should be careful as it takes time.

# Lets plot the tuned model

plot(tuned_model)

summary(tuned_model)

# With this result, we can choose our best model

final_Model = tuned_model$best.model


# We can also plot this model

plot(final_Model, data = iris,
     Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))

# Lets see the confusion metrix
pred = predict(object = final_Model, newdata = iris)

confusionMatrix(pred,iris$Species)
