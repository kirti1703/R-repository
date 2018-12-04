library(ggplot2)

data = read.csv("C://Users//Yashanjali//Documents//Praxis//ML//cars.csv")
data

cor(data[,unlist(lapply(data,is.numeric))])

set.seed(0)
rand = sample(1:nrow(data),352)
train = data[rand,]
test = data[-rand,]
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7 for different sample sizes
#=============================================================================================
residual_sum <- c( )
test_sum <- c( )
x <- c(10,20,30,50,70,100,200,350)
for (i in x)
{
  rand_train = sample(1:nrow(train),i)
  train_data = train[rand_train, ]
    
  m_order7 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5) + I(Weight^6) +
                 I(Weight^7), train_data)
  m_order7
    
  jpeg(paste("Polynomail Regression of Degree 7 with sample size",i, ".jpeg", sep = ""))
#PLOTTING THE MODEL OVER THE DATA
  plot(train_data$Weight,train_data$MPG, pch=19, cex=0.5, main =paste("Polynomial Regression of Order 7 (Sample size ",  i, ")", sep = ""), xlab = "Weight", ylab = "MPG")
  lines(sort(train_data$Weight), fitted(m_order7)[order(train_data$Weight)], col='magenta', type='l',pch=20)
  dev.off()
    
#TRAIN AND TEST ACCURACY
  residual_sum <- c(residual_sum, sum(m_order7$residuals^2))
  pred = predict(m_order7, newdata=test)
  test_sum <- c(test_sum, sum((pred-test$MPG)^2))
}

jpeg(paste("Test Error Vs Sample(Order 7)", ".jpeg", sep = ""))
#PLOTTING THE MODEL OVER THE DATA
plot(x,test_sum, type = "l",main = "Polynomial Regression of Order 7,(Test Error vs Sample Size)", 
     xlab = "Sample Size", ylab = "Test Error", cex = 0.5, col = 'red')
dev.off()
