#Enter file contents here#Parameters: poly.degree - degree of the polynomial, sample.size - sample size to be generated, sample.w.replacement - whether or not replacement is allowed when generating sample
generate.sample<-function(poly.degree, sample.size, sample.w.replacement){
  set.seed(2010)
  population = -5:5
  
  if (sample.w.replacement==1)
    x = c(sample(population,sample.size,replace=TRUE))
  else
    x = c(sample(popuation,sample.size,replace=FALSE))
  
  y = c()
  for (i in 1:sample.size){
    delta = rnorm(1,0,1)
    y[i]  = sin(x[i]) #x[i]^poly.degree
    y[i]  = (y[i] + delta)/100
  }
  
  data = cbind(x,y)  
#  data = scale(data)
  colnames(data) = c("x", "y")

  return(data)
}

#cross validation: "Holdout method"
#Parameters: data - sample data, fraction - deteremines size of train/test sets
cross.validation<-function(data, fraction){
  sample_size = floor(fraction * nrow(data))   
  train_ind = sample(seq_len(nrow(data)), size = sample_size)
  return(train_ind)
}

fit.model<-function(mtype, train, test){
   
  par(mfrow=c(length(mtype),2))
  
  train.error = c()
  test.error  = c()
  
  for ( i in 1:length(mtype)){
    model.type = mtype[i]

    if (model.type=="linear"){
      fit = lm(train[,2] ~ train[,1] )
    } else if (model.type=="quadratic"){
      fit = lm(train[,2] ~ train[,1] + I(train[,1]^2))
    } else{
#      fit = lm(train[,2] ~ train[,1] + I(train[,1]^2) + I(train[,1]^3))
      fit = lm(train[,2] ~ train[,1] + I(train[,1]^3))
    }
  
  coeff = coefficients(fit)
  
  errors = model.estimations(model.type, coeff, train,test)
  
  train.error = append(train.error,errors[[1]][1])
  test.error  = append(test.error, errors[[1]][2])
  
  }
  
  return(list(train.error, test.error))
}

model.estimations<-function(type, coeff, train, test,train.error, test.error){  
  
  if (type =='linear'){
    fitted = coeff[1] + coeff[2]*train[,1]
    predictions = coeff[1] + coeff[2]*test[,1]
    
    #plot fitted and predicted  
    show.plot(train[,1], train[,2], fitted)
    show.plot(test[,1], test[,2],predictions)
    
    errors = calculate.MSE(fitted, predictions)   
  }

  if (type =='quadratic'){
    fitted = coeff[1] + coeff[2]*train[,1] + coeff[3]*train[,1]^2
    predictions = coeff[1] + coeff[2]*test[,1] + coeff[3]*test[,1]^2
    
    #plot fitted and predicted  
    show.plot(train[,1], train[,2], fitted)
    show.plot(test[,1], test[,2],predictions)
    
    errors = calculate.MSE(fitted, predictions)
  }

  if (type =='cubic'){
#     fitted = coeff[1] + coeff[2]*train[,1] + coeff[3]*train[,1]^2 + coeff[4]*train[,1]^3
#     predictions = coeff[1] + coeff[2]*test[,1] + coeff[3]*test[,1]^2 + coeff[4]*test[,1]^3 
    
    fitted = coeff[1] + coeff[2]*train[,1] + coeff[3]*train[,1]^3
    predictions = coeff[1] + coeff[2]*test[,1] + coeff[3]*test[,1]^3
    
    #plot fitted and predicted  
    show.plot(train[,1], train[,2], fitted)
    show.plot(test[,1], test[,2],predictions)
    
    errors = calculate.MSE(fitted, predictions)
  }
  
  return (list(errors))
  
}

calculate.MSE<-function(fitted.values, predictions){
  #training error
  mse.train = (fitted.values - train[,2])^2
  meanTrainError = mean(mse.train)             
  
  #test error
  mse.test = (predictions - test[,2])^2
  meanTestError = mean(mse.test)
    
  return(list(meanTrainError, meanTestError))
}

#plotting functions
show.plot<-function(x,y,dataSet){
  ss = smooth.spline(x,dataSet)
  plot(x,y)   
  lines(ss)
}

show.w.spline<-function(x,y){ 
  ss = smooth.spline(x,y)
  plot(x,y)   
  lines(ss)
}

add.lines<-function(xdata,ydata,col,lwd){
  lines(xdata, ydata, col=col, lwd=lwd)
}
