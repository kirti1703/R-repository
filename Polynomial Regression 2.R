library(ggplot2)
library(plyr)

data = read.csv("C://Users//Yashanjali//Documents//Praxis//ML//cars.csv")
data

set.seed(0)
rand = sample(1:nrow(data),352)
train = data[rand,]
test = data[-rand,]
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7 for sample size 10
#=============================================================================================

residual_sum2 <- c( )
test_sum2 <- c( )
test_RMSE <- c( )
train_RMSE <- c( )
x <- c(1:10)
for (i in 1:4) 
{
  set.seed(i*3)
  rand_train = sample(1:nrow(train),20, replace = FALSE)
  train_data = train[rand_train,]
#MODEL 1 
  m1 <- lm(MPG ~ Weight, train_data)
  #TRAIN AND TEST ACCURACY
  residual_sum2 <- c(residual_sum2,sum(m1$residuals^2))
  pred2 = predict(m1, newdata=test)
  test_sum2 <- c(test_sum2,sum((pred2-test$MPG)^2))
  train_RMSE <- c(train_RMSE, sqrt((1/(20-2))*residual_sum2))
  test_RMSE <- c(test_RMSE, sqrt((1/(20-2))*test_sum2))
#MODEL 2  
  m2 <- lm(MPG ~ Weight + I(Weight^2), train_data)
  #TRAIN AND TEST ACCURACY
  residual_sum2 <- c(residual_sum2,sum(m2$residuals^2))
  pred2 = predict(m2, newdata=test)
  test_sum2 <- c(test_sum2,sum((pred2-test$MPG)^2))
  train_RMSE <- c(train_RMSE, sqrt((1/(20-3))*residual_sum2))
  test_RMSE <- c(test_RMSE,sqrt((1/(20-3))*test_sum2))
#MODEL 3  
  m3 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3), train_data)
  #TRAIN AND TEST ACCURACY
  residual_sum2 <- c(residual_sum2,sum(m3$residuals^2))
  pred2 = predict(m3, newdata=test)
  test_sum2 <- c(test_sum2,sum((pred2-test$MPG)^2))
  train_RMSE <- c(train_RMSE, sqrt((1/(20-4))*residual_sum2))
  test_RMSE <- c(test_RMSE,sqrt(1/(20-4))*test_sum2)
#MODEL 4 
  m4 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4), train_data)
  #TRAIN AND TEST ACCURACY
  residual_sum2 <- c(residual_sum2,sum(m4$residuals^2))
  pred2 = predict(m4, newdata=test)
  test_sum2 <- c(test_sum2,sum((pred2-test$MPG)^2))
  train_RMSE <- c(train_RMSE, sqrt((1/(20-5))*residual_sum2))
  test_RMSE <- c(test_RMSE,sqrt((1/(20-5))*test_sum2))
#MODEL 5 
  m5 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4), train_data)
  #TRAIN AND TEST ACCURACY
  residual_sum2 <- c(residual_sum2,sum(m5$residuals^2))
  pred2 = predict(m5, newdata=test)
  test_sum2 <- c(test_sum2,sum((pred2-test$MPG)^2))
  train_RMSE <- c(train_RMSE, sqrt((1/(20-6))*residual_sum2))
  test_RMSE <- c(test_RMSE,sqrt((1/(20-6))*test_sum2))
#MODEL 6 
  m6 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4), train_data)
  #TRAIN AND TEST ACCURACY
  residual_sum2 <- c(residual_sum2,sum(m6$residuals^2))
  pred2 = predict(m6, newdata=test)
  test_sum2 <- c(test_sum2,sum((pred2-test$MPG)^2))
  train_RMSE <- c(train_RMSE, sqrt((1/(20-7))*residual_sum2))
  test_RMSE <- c(test_RMSE,sqrt((1/(20-7))*test_sum2))
#MODEL 7 
  m7 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4), train_data)
  #TRAIN AND TEST ACCURACY
  residual_sum2 <- c(residual_sum2,sum(m7$residuals^2))
  pred2 = predict(m7, newdata=test)
  test_sum2 <- c(test_sum2,sum((pred2-test$MPG)^2))
  train_RMSE <- c(train_RMSE, sqrt((1/(20-8))*residual_sum2))
  test_RMSE <- c(test_RMSE,sqrt((1/(20-8))*test_sum2))
#MODEL 8 
  m8 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4), train_data)
  #TRAIN AND TEST ACCURACY
  residual_sum2 <- c(residual_sum2,sum(m8$residuals^2))
  pred2 = predict(m8, newdata=test)
  test_sum2 <- c(test_sum2,sum((pred2-test$MPG)^2))
  train_RMSE <- c(train_RMSE, sqrt((1/(20-9))*residual_sum2))
  test_RMSE <- c(test_RMSE,sqrt((1/(20-9))*test_sum2))
#MODEL 9 
  m9 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4), train_data)
  #TRAIN AND TEST ACCURACY
  residual_sum2 <- c(residual_sum2,sum(m9$residuals^2))
  pred2 = predict(m9, newdata=test)
  test_sum2 <- c(test_sum2,sum((pred2-test$MPG)^2))
  train_RMSE <- c(train_RMSE, sqrt((1/(20-10))*residual_sum2))
  test_RMSE <- c(test_RMSE,sqrt((1/(20-10))*test_sum2))
#MODEL 10 
  m10 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4), train_data)
  #TRAIN AND TEST ACCURACY
  residual_sum2 <- c(residual_sum2,sum(m10$residuals^2))
  pred2 = predict(m10, newdata=test)
  test_sum2 <- c(test_sum2,sum((pred2-test$MPG)^2))
  train_RMSE <- c(train_RMSE, sqrt((1/(20-11))*residual_sum2))
  test_RMSE <- c(test_RMSE,sqrt((1/(20-11))*test_sum2))
  
  jpeg(paste("Polynomail Regression for sample", i,".jpeg", sep = ""))
  #PLOTTING THE MODEL OVER THE DATA
  plot(train_data$Weight,train_data$MPG, pch=19, cex=0.5, main =paste("Polynomial Regression for Sample ",  i, ")", sep = ""), xlab = "Weight", ylab = "MPG")
  lines(sort(train_data$Weight), fitted(m1)[order(train_data$Weight)], col='magenta', type='l',pch=20)
  lines(sort(train_data$Weight), fitted(m2)[order(train_data$Weight)], col='magenta', type='l',pch=20)
  lines(sort(train_data$Weight), fitted(m3)[order(train_data$Weight)], col='magenta', type='l',pch=20)
  lines(sort(train_data$Weight), fitted(m4)[order(train_data$Weight)], col='magenta', type='l',pch=20)
  lines(sort(train_data$Weight), fitted(m5)[order(train_data$Weight)], col='magenta', type='l',pch=20)
  lines(sort(train_data$Weight), fitted(m6)[order(train_data$Weight)], col='magenta', type='l',pch=20)
  lines(sort(train_data$Weight), fitted(m7)[order(train_data$Weight)], col='magenta', type='l',pch=20)
  lines(sort(train_data$Weight), fitted(m8)[order(train_data$Weight)], col='magenta', type='l',pch=20)
  lines(sort(train_data$Weight), fitted(m9)[order(train_data$Weight)], col='magenta', type='l',pch=20)
  lines(sort(train_data$Weight), fitted(m10)[order(train_data$Weight)], col='magenta', type='l',pch=20)
  dev.off()
  
  
}

test_err_1 <- test_sum2[1:10]
test_err_2 <- test_sum2[11:20]
test_err_3 <- test_sum2[21:30]
test_err_4 <- test_sum2[31:40]

jpeg(paste("Model Complexity_1 Vs Test Error for sample size of 20",".jpeg", sep = ""))
plot(x,test_err_1, type = "l",main ="Model Complexity Vs Test Error for first sample of size 20",
     xlab = "Model Complexity", ylab = "Test RSS", cex = 0.5, col = 'red')
dev.off()

jpeg(paste("Model Complexity_2 Vs Test Error for sample size of 20",".jpeg", sep = ""))
plot(x,test_err_2, type = "l",main ="Model Complexity Vs Test Error for second sample of size 20", 
     xlab = "Model Complexity", ylab = "Test RSS", cex = 0.5, col = 'red')
dev.off()

jpeg(paste("Model Complexity_3 Vs Test Error for sample size of 20",".jpeg", sep = ""))
plot(x,test_err_3, type = "l",main = "Model Complexity Vs Test Error for third sample of size 20",
     xlab = "Model Complexity", ylab = "Test Error", cex = 0.5, col = 'red')
dev.off()

jpeg(paste("Model Complexity_4 Vs Test Error for sample size of 20",".jpeg", sep = ""))
plot(x,test_err_4, type = "l",main = "Model Complexity Vs Test Error for fourth sample of size 20",
     xlab = "Model Complexity", ylab = "Test RSS", cex = 0.5, col = 'red')
dev.off()

test_RMSE_1 = test_RMSE[1:10]
test_RMSE_2 = test_RMSE[11:20]
test_RMSE_3 = test_RMSE[21:30]
test_RMSE_4 = test_RMSE[31:40]

jpeg(paste("Model Complexity_1 Vs Test RMSE for sample size of 20",".jpeg", sep = ""))
plot(x,test_RMSE_1, type = "l",main = "Model Complexity Vs Test RMSE for first sample of size 20",
     xlab = "Model Complexity", ylab = "Test RMSE", cex = 0.5, col = 'red')
dev.off()

jpeg(paste("Model Complexity_2 Vs Test RMSE for sample size of 20",".jpeg", sep = ""))
plot(x,test_RMSE_2, type = "l",main = "Model Complexity Vs Test RMSE for second sample of size 20",
     xlab = "Model Complexity", ylab = "Test RMSE", cex = 0.5, col = 'red')
dev.off()

jpeg(paste("Model Complexity_3 Vs Test RMSE for sample size of 20",".jpeg", sep = ""))
plot(x,test_RMSE_3, type = "l",main = "Model Complexity Vs Test RMSE for third sample of size 20",
     xlab = "Model Complexity", ylab = "Test RMSE", cex = 0.5, col = 'red')
dev.off()

jpeg(paste("Model Complexity_4 Vs Test RMSE for sample size of 20",".jpeg", sep = ""))
plot(x,test_RMSE_4, type = "l",main = "Model Complexity Vs Test RMSE for fourth sample of size 20",
     xlab = "Model Complexity", ylab = "Test RMSE", cex = 0.5, col = 'red')
dev.off()


train_RMSE_1 = test_RMSE[1:10]
train_RMSE_2 = test_RMSE[11:20]
train_RMSE_3 = test_RMSE[21:30]
train_RMSE_4 = test_RMSE[31:40]

jpeg(paste("Model Complexity_1 Vs Train RMSE for sample size of 20",".jpeg", sep = ""))
plot(x,train_RMSE_1, type = "l",main = "Model Complexity Vs Train RMSE for first sample of size 20",
     xlab = "Model Complexity", ylab = "Train RMSE", cex = 0.5, col = 'red')
dev.off()

jpeg(paste("Model Complexity_2 Vs Train RMSE for sample size of 20",".jpeg", sep = ""))
plot(x,train_RMSE_2, type = "l",main = "Model Complexity Vs Train RMSE for second sample of size 20",
     xlab = "Model Complexity", ylab = "Train RMSE", cex = 0.5, col = 'red')
dev.off()

jpeg(paste("Model Complexity_3 Vs Train RMSE for sample size of 20",".jpeg", sep = ""))
plot(x,train_RMSE_3, type = "l",main = "Model Complexity Vs Train RMSE for third sample of size 20",
     xlab = "Model Complexity", ylab = "Train RMSE", cex = 0.5, col = 'red')
dev.off()

jpeg(paste("Model Complexity_4 Vs Train RMSE for sample size of 20",".jpeg", sep = ""))
plot(x,train_RMSE_4, type = "l",main = "Model Complexity Vs Train RMSE for fourth sample of size 20",
     xlab = "Model Complexity", ylab = "Train RMSE", cex = 0.5, col = 'red')
dev.off()

