# In this file, we use simulated data to better understand the bias-variance tradeoff in statistical learning

pred_error = matrix(0, nrow = 300, ncol = 100) # empty container for MSE
BIAS2 = matrix(0, nrow = 300, ncol = 100) # empty container for BIAS2
variance = matrix(0, nrow = 300, ncol = 100) # empty container for variance
irr_error = matrix(0, nrow = 300, ncol = 100) # empty container for irr_error

beta = matrix(0, nrow = 100, ncol = 1)
for (var in c(1:100)){
  beta[var,] = 0.9^var
}

# data simulation, to fasten testing, run 20 simulations first
set.seed(452)
for (iter in c(1:20)){
  X_train = matrix(rnorm(300*100,mean=0,sd=1), 300, 100) 
  y_train = matrix(0, nrow = 300, ncol = 1)
  y_true = matrix(0, nrow = 300, ncol = 1)
  y_test = matrix(0, nrow = 300, ncol = 1)
  error1 = matrix(rnorm(300*1,mean=0,sd=1), 300, 1)
  error2 = matrix(rnorm(300*1,mean=0,sd=1), 300, 1)
  
  for (sample in c(1:300)){
    for (var in c(1:100)){
      y_true[sample] =  y_train[sample]+0.9^var*X_train[sample,var]
    }
    y_train[sample] = y_true[sample] + error1[sample] # generate y_train
    y_test[sample] = y_true[sample] + error2[sample] # generate y_test
  }
  
  # run 100 different models
  for (n_feature in c(1:100)){
    tmp_df <- data.frame(y = y_train, X_train[,1:n_feature])
    fit <- lm(y~., data = tmp_df)
    variance[,n_feature] = variance[,n_feature] + 1*n_feature/300 # theoretical variance
    pred_error[,n_feature]= pred_error[,n_feature]+(fit$fitted.values-y_test)^2
    irr_error[,n_feature] = irr_error[,n_feature] + error2^2
    
    X <-  X_train[,1:n_feature]
    BIAS2[,n_feature] = BIAS2[,n_feature] + 
      mean((X_train %*% beta - X %*% solve(t(X) %*% X) %*% t(X) 
            %*% X_train %*% beta)^2)
  }
}

variance = variance / 20 # average across simulations
pred_error = pred_error / 20 # average across simulations
irr_error = irr_error / 20 # average across simulations
BIAS2 = BIAS2 / 20 # average across simulations

variance_mean <- colMeans(variance)
irr_error_mean <- colMeans(irr_error)
pred_error_mean <- colMeans(pred_error)
BIAS2_mean <- colMeans(BIAS2)

sum_of_three = variance_mean + irr_error_mean + BIAS2_mean

# Create a dataframe for plotting
tmp <- c(1:100)
plot_data <- cbind(tmp,pred_error_mean,BIAS2_mean,variance_mean,sum_of_three)
plot_data <- data.frame(plot_data)
colnames(plot_data) <- c("number_of_variables","prediction_error","Squared_bias","variance","sum")
library(ggplot2)
ggplot(plot_data, aes(number_of_variables)) + 
  geom_line(aes(y = prediction_error, colour = 'prediction error')) + 
  geom_line(aes(y = Squared_bias, colour = 'Squared bias')) +
  geom_line(aes(y = variance, colour = "variance")) +
  geom_line(aes(y = sum, colour = "sum of bias^2 + variance + irreducible error"))  
```