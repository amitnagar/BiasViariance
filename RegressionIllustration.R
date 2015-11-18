#Enter file contents here

#Study the impact of fitting regression models of varying degrees - linear, quadratic, and cubic to a data set 
#consisting of randomly generated predictor variables (X) and response variables (y) generated from a known function 
#with added random noise.

#We start by creating a train and a test sample from the data set using holdout cross validation technique. 
#Fit the linear, quadratic, and cubic models to the training set. Compute the training mean squared errors (MSE) and then
#compare with the MSE for the test set using the derived regeression coefficients.  

#We plot the scatter plot overlaid with various regression fits.

#We do note that this exercise can be made more generic by using multiple training and test sets 
#as suggested in the literature. 

source("utils.R")

#generate sample data set
#Parameters: degree = 2, sample.size = 100, replace = true
sample.data = generate.sample(2, 100, 1)

#create the Train & Test data set
#Parameters: sample = sample.data, 0.75 = fraction of sample as training data
indices = cross.validation(sample.data, 0.75)
train   = sample.data[indices, ]
test    = sample.data[-indices, ]

#fit following regression models
model.type=c("linear", "quadratic", "cubic")

errors = fit.model(model.type, train, test)

errors[[1]] 

par(mfrow=c(1,2))
plot(1:3,errors[[1]], type="l", col="RED", lwd=2)

errors[[2]]
plot(1:3,errors[[2]], type ="l",col="BLUE", lwd=2 )

