library(readr)
library(nnet)
library(caret)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(caTools)
library(dlm)
rm(list=ls())

# i -----------------------------------------------------------------------
x <- data.frame(read_csv("xvalsSine.csv"))
y <- data.frame(read_csv("cleanSine.csv"))
noisyy <- data.frame(read_csv("noisySine.csv"))
data <- cbind(x,y,noisyy)
colnames(data) <- c("x", "y","noisyy")
summary(data)

# Splitting data
set.seed(123) 
split <- sample.split(data$y, SplitRatio = 0.7)
training_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)

# OLS
ols_model <- lm(y ~ poly(x, 10,raw = TRUE), data = training_set)
summary(ols_model)
ols_pred <- data.frame(predict(ols_model, test_set))

# Bonus: Stochastic gradient descent
training_set$x2 <- training_set$x^2
training_set$x3 <- training_set$x^3
training_set$x4 <- training_set$x^4
training_set$x5 <- training_set$x^5
training_set$x6 <- training_set$x^6
training_set$x7 <- training_set$x^7
training_set$x8 <- training_set$x^8
training_set$x9 <- training_set$x^9
training_set$x10 <- training_set$x^10
training_set$intercept <- 1
X <- as.matrix(training_set[,c("intercept","x","x2","x3","x4","x5","x6","x7","x8","x9","x10")])
starting_values = ols_model$coefficients
source("sgd.R")
fit_sgd = sgd(starting_values,X = X,y = training_set$y,stepsize = 0.001,average = FALSE, type = "adagrad")
str(fit_sgd)

# Compare coefficients
table <- cbind(fit_sgd$par, ols_model$coefficients)
table

# ii ----------------------------------------------------------------------
# MA(5)
window_length <- 5
data$mafiltered <- filter(data$noisyy, rep(1/window_length, window_length), sides=2)
mse_ma <- mean((data$mafiltered - data$y)^2, na.rm=TRUE)
print(paste("Mean Squared Error: ", mse_ma))

ggplot() +
  geom_line(data = data, aes(x = x, y = y, color = "Clean Sine")) +
  geom_line(data = data, aes(x = x, y = noisyy, color = "Noisy Sine")) +
  geom_line(data = data, aes(x = x, y = mafiltered, color = "Filtered Noisy Sine")) +
  labs(title = "Sine Wave Filtering", x = "x", y = "y") +
  scale_color_manual(values = c("Clean Sine" = "blue", "Noisy Sine" = "red", "Filtered Noisy Sine" = "green")) +
  theme_minimal()

# Kalman
# Build the model
buildModel <- function(theta) {
  dlmModPoly(order = 3, dV = exp(theta[1]), dW = c(exp(theta[2]), exp(theta[3]), exp(theta[4])))
}

# Maximum likelihood estimation of the parameters
fit <- dlmMLE(y = data$noisyy, parm = c(0,0,0,0), build = buildModel)

# Update the model with the estimated parameters
model <- buildModel(fit$par)

# Filter the data using the Kalman filter
filtered <- dlmFilter(y = data$noisyy, mod = model)
data$kalman <- filtered$f
mse_kalman <- mean((data$kalman - data$y)^2, na.rm=TRUE)
print(paste("Mean Squared Error: ", mse_kalman))

ggplot() +
  geom_line(data = data, aes(x = x, y = y, color = "Clean Sine")) +
  geom_line(data = data, aes(x = x, y = noisyy, color = "Noisy Sine")) +
  geom_line(data = data, aes(x = x, y = kalman, color = "Filtered Noisy Sine")) +
  labs(title = "Sine Wave Using Kalman Filter", x = "x", y = "y") +
  scale_color_manual(values = c("Clean Sine" = "blue", "Noisy Sine" = "red", "Filtered Noisy Sine" = "green")) +
  theme_minimal()

# Forecast 10 steps ahead
forecast <- dlmForecast(filtered, nAhead = 10)
print(forecast$f)
