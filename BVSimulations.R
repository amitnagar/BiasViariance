#sample size
size = 100

#generate random numbers between min/max values
set.seed(123)
x = as.data.frame(sample(100,size)/100)
delta = sample(16,replace=TRUE) #?
y = as.data.frame((2 + 4*x + 4*x^0.5 + delta)/100)

data = cbind(x,y)

#correct column names
colnames(data) = c("x", "y")

#split data into train and test sets (75% of the sample size)
smp_size = floor(0.75 * nrow(data))   
train_ind = sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <-  data[-train_ind, ]

#fit linear regression
fit = lm(train$y~train$x)
lmSummary = summary(fit)

#graph the scatter and regression line
plot(train$x, train$y)
abline(fit, lwd=2)