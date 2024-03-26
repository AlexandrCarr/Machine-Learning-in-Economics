# R-script for Problem Set 5
# Author: B158582
# Date: 19/11/2023


##################
##### Set up #####
##################

hp <- read.csv("AmesHousePrices.csv", stringsAsFactors = T)
x <- hp[sapply(hp,is.numeric)]
drop <- c("price","MS.SubClass","Overall.Qual","Overall.Cond",
          "BsmtFin.SF.2","Bsmt.Full.Bath","Bsmt.Half.Bath",
          "Full.Bath", "Half.Bath", "Mo.Sold", "Yr.Sold")
x = x[,!(names(x) %in% drop)]


##################
### Question 1 ###
##################

# sequence of 100 equally spaced numbers between the min and max of log price
i <- seq(min(log(hp$price)), max(log(hp$price)), length=100)

# mean and variance + vector of densities using the calculated mean and variance
meanlogprice <- mean(log(hp$price))
varlogprice <- var(log(hp$price))
j <- dnorm(i, mean = meanlogprice, sd = sqrt(varlogprice))

# plotting histogram with overlay of normal density plot
hist(log(hp$price),
     breaks = 50,
     probability = TRUE,
     main = "Histogram of Log Prices with Normal Density Overlay",
     xlab = "Log Price")
lines(i, j, col = "red", lwd = 2)
legend("topright", legend=c("Empirical Distribution of Log Prices", "Normal Density"), fill=c("black", "red"), cex =0.50)

##################
### Question 2 ###
##################


# Performing PCA
pca_result <- prcomp(x, scale = TRUE)

# scree plot
plot(pca_result, main ="Scree Plot")
mtext(side=1, "Numerical Features PC Direction",  line=1, font=2)

# Extracting variances of PCs
variances <- pca_result$sdev^2
variance_first_pc <- variances[1]
variance_last_pc <- variances[length(variances)]

variances[2]/sum(variances)

# Proportion of total variance captured by the first PC
prop_variance_first_pc <- variance_first_pc / sum(variances)

# Print results
cat("Number of PCs:", length(variances), "\n")
cat("Variance of the first PC (PC1):", variance_first_pc, "\n")
cat("Variance of the last PC:", variance_last_pc, "\n")
cat("Proportion of total variance captured by PC1:", prop_variance_first_pc, "\n")

##################
### Question 3 ###
##################

# listing rotations for first 5 PCs in the PCA
round(pca_result$rotation[,1:5],1) 

##################
### Question 4 ###
##################

predicted_pcs <- predict(pca_result)

# Extract PC1 and PC2
predicted_pc1 <- predicted_pcs[, 1]
predicted_pc2 <- predicted_pcs[, 2]

# Calculate the interquartile range for PC1 and PC2
iqr_pc1 <- IQR(predicted_pc1)
iqr_pc2 <- IQR(predicted_pc2)

# Set the threshold for identifying outliers
threshold_pc1 <- 1.5 * iqr_pc1
threshold_pc2 <- 1.5 * iqr_pc2

scatter_data <- data.frame(PC1 = predicted_pc1, PC2 = predicted_pc2)

library(ggplot2)

# Identify outliers
outliers <- scatter_data[
  (predicted_pc1 > quantile(predicted_pc1, 0.75) + threshold_pc1) |
    (predicted_pc1 < quantile(predicted_pc1, 0.25) - threshold_pc1) |
    (predicted_pc2 > quantile(predicted_pc2, 0.75) + threshold_pc2) |
    (predicted_pc2 < quantile(predicted_pc2, 0.25) - threshold_pc2),
]

# Scatter plot with outliers highlighted in red
ggplot(scatter_data, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = ifelse(row.names(scatter_data) %in% row.names(outliers), "Outlier", "Non-outlier")), size = 0.5) +
  scale_color_manual(values = c("Non-outlier" = "blue", "Outlier" = "red"), name = "Observation") +
  xlab("Predicted PC1") +
  ylab("Predicted PC2") +
  ggtitle("Scatter Plot of Predicted PC1 vs Predicted PC2") +
  theme(
    legend.position = "top",
    panel.grid.major = element_line(color = "white", size = 0.2),
    panel.grid.minor = element_line(color = "white", size = 0.2),
    panel.background = element_rect(fill = "white", color = "black")
  )


# Scatter plot without outliers highlighted in red a
ggplot(scatter_data, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = ifelse(row.names(scatter_data) %in% row.names(outliers), "Outlier", "Non-outlier")), size = 0.5) +
  scale_color_manual(values = c("Non-outlier" = "blue", "Outlier" = "red"), name = "Observation") +
  xlab("Predicted PC1") +
  ylab("Predicted PC2") +
  ggtitle("Scatter Plot of Predicted PC1 vs Predicted PC2 With Limited Outliers") +
  xlim(-6.5, NA) +
  ylim(NA, 5) +
  theme(
    legend.position = "top",
    panel.grid.major = element_line(color = "white", size = 0.2),
    panel.grid.minor = element_line(color = "white", size = 0.2),
    panel.background = element_rect(fill = "white", color = "black")
  )

##################
### Question 5 ###
##################

library(caret)
library(caTools)

# K-fold cross-validation setup
set.seed(1)
num_folds <- 10
folds <- createFolds(log(hp$price), k = num_folds)

# Initialize vectors to store MSE values
mse_train <- rep(NA, num_folds)
mse_test <- rep(NA, num_folds)

# Store the MSE output 

MSE_output1 <- matrix (nrow = 1, ncol = 2)

# Loop over the folds
for (fold in 1:num_folds) {
  train <- hp[-folds[[fold]], ]
  test <- hp[folds[[fold]], ]
  
  fullreg <- glm(log(train$price) ~ ., data = train)
  
  train_preds_fullreg <- predict(fullreg, newdata = train)
  test_preds_fullreg <- predict(fullreg, newdata = test)
  
  mse_train[fold] <- mean((log(train$price) - train_preds_fullreg)^2)
  mse_test[fold] <- mean((log(test$price) - test_preds_fullreg)^2)
}

# Display the mean MSEs across folds
mse_fullreg <- data.frame(Model = c("Full Linear Regression"), 
                          "IS MSE" = mean(mse_train),
                          "OOS MSE" = mean(mse_test))
mse_fullreg

##################
### Question 6 ###
##################


set.seed(1)
num_folds <- 10
folds <- createFolds(hp$price, k = num_folds)

# Initialize vectors to store MSE values
mse_train_interact <- rep(NA, num_folds)
mse_test_interact <- rep(NA, num_folds)

# Loop over the folds
for (fold in 1:num_folds) {
  # Split the data into training and testing sets for the current fold
  trainQ6 <- hp[-folds[[fold]], ]
  testQ6 <- hp[folds[[fold]], ]
  
  interact_neigh_reg <- glm(log(trainQ6$price) ~ . * Neighborhood, data = trainQ6)
  
  train_preds_interact <- predict(interact_neigh_reg, newdata = trainQ6)
  # Make predictions on the testing set
  test_preds_interact <- predict(interact_neigh_reg, newdata = testQ6)
  
  # Calculate Mean Squared Error (MSE) for training set
  mse_train_interact[fold] <- mean((log(trainQ6$price) - train_preds_interact)^2)
  # Calculate Mean Squared Error (MSE) for testing set
  mse_test_interact[fold] <- mean((log(testQ6$price) - test_preds_interact)^2)
}

# Display the mean MSEs across folds for the interaction model
mse_interact <- data.frame(Model = c("Interaction Model"), 
                           "IS MSE" = mean(mse_train_interact),
                           "OOS MSE" = mean(mse_test_interact))

combined_mses <- rbind(mse_fullreg, mse_interact)
combined_mses
##################
### Question 7 ###
##################

library(glmnet)
# preparing data for Lasso regression
drop <- c("price")
feat <- hp[, !(names(hp) %in% drop)]
xmat <- model.matrix(~ Neighborhood*., data=feat)

# K-fold cross-validation setup
set.seed(1)
n <- 10
folds <- createFolds(hp$price, k = n)

# initializing vectors to store MSE values
oos_mse_lasso <- rep(NA, n)

for (fold in 1:n) {
  train_indices <- unlist(folds[-fold])
  test_indices <- unlist(folds[fold])
  
  x_train <- xmat[train_indices, ]
  y_train <- log(hp$price[train_indices])
  
  x_test <- xmat[test_indices, ]
  y_test <- log(hp$price[test_indices])
  
  lasso_model <- glmnet(x_train, y_train, alpha = 1)
  
  # cross-validation to find the lambda that minimizes MSE
  cv_lasso_q7 <- cv.glmnet(x_train, y_train, alpha = 1)
  lasso_prediction <- predict(lasso_model, s = cv_lasso_q7$lambda.min, newx = x_test)
  
  oos_mse_lasso[fold] <- mean((y_test - lasso_prediction)^2)
}

# calculating OOS MSE
oos_mse_q7 <- mean(oos_mse_lasso)

# comparing OOS MSE for models
mse_output <- data.frame(
  `Out-of-Sample MSE` = c(mean(mse_test), mean(mse_test_interact), oos_mse_q7)
)
rownames(mse_output) <- c("Full Linear Regression", "Interaction Model", "Lasso Model")
print(mse_output)


##################
### Question 9 ###
##################

hpPC <- data.frame(hp, as.data.frame( predicted_pcs[, 1:ncol(predicted_pcs) - 1]))

# K-fold cross-validation setup
set.seed(1)
n <- 10 
folds <- createFolds(hpPC$price, k = n)

# initializing vectors to store MSE values
is.mse.pcr <- rep(NA, n)
oos.mse.pcr <- rep(NA, n)

for (fold in 1:n) {
  train.indices <- unlist(folds[-fold])
  test.indices <- unlist(folds[fold])
  
  train <- hpPC[train.indices, c("price", paste0("PC", 1:25))]
  test <- hpPC[test.indices, c("price", paste0("PC", 1:25))]
  
  model.pcr <- glm(log(train$price) ~ ., data = train)
  
  is.prediction <- predict(model.pcr, newdata = train)
  is.mse.pcr[fold] <- mean((log(train$price) - is.prediction)^2)
  
  oos.prediction <- predict(model.pcr, newdata = test)
  oos.mse.pcr[fold] <- mean((log(test$price) - oos.prediction)^2) 
}

# calculating IS and OOS MSE
is.mse.q9 <- mean(is.mse.pcr)
oos.mse.q9 <- mean(oos.mse.pcr)

# comparing IS and OOS MSE for models
mse.output <- data.frame(
  `In-Sample MSE` = c(mean(mse_train), mean(mse_train_interact), is.mse.q9),
  `Out-of-Sample MSE` = c(mean(mse_test), mean(mse_test_interact), oos.mse.q9)
)
rownames(mse.output) <- c("Full Linear Regression", "Interaction Model", "OLS PCR Model")
print(mse.output)

#####################
#### Question 10 ####
#####################

# adding PCs to model matrix for PCR lasso
xmat2 <- cbind(xmat, predicted_pcs[,1:ncol(predicted_pcs)-1])

# K-fold cross-validation setup
set.seed(1)
n <- 10 
folds <- createFolds(hpPC$price, k = n)

# initializing vectors to store OOS MSE value
oos.mse.pcr.lasso <- rep(NA, n)

for (fold in 1:n) {
  train.indices <- unlist(folds[-fold])
  test.indices <- unlist(folds[fold])
  
  x.train <- xmat2[train.indices, ]
  y.train <- log(hpPC$price[train.indices])
  
  x.test <- xmat2[test.indices, ]
  y.test <- log(hpPC$price[test.indices])
  
  lasso.model <- glmnet(x.train, y.train, alpha = 1)
  
  # cross-validation to find the lambda that minimizes MSE
  cv.lasso.q10 <- cv.glmnet(x.train, y.train, alpha = 1)
  lasso.prediction <- predict(lasso.model, s = cv.lasso.q10$lambda.min, newx = x.test)
  
  oos.mse.pcr.lasso[fold] <- mean((y.test - lasso.prediction)^2)
}

# calculating OOS MSE
oos.mse.q10 <- mean(oos.mse.pcr.lasso)

# comparing OOS MSE for models
mse.output <- data.frame(
  `Out-of-Sample MSE` = c(oos_mse_q7, oos.mse.q10)
)
rownames(mse.output) <- c("Lasso Model", "PCR Lasso Model")
print(mse.output)

#####################
#### Question 11 ####
#####################

# extracting the index of PC1 from the glmnet model
pc1.index <- match("PC1", dimnames(cv.lasso.q10$glmnet.fit$beta)[[1]])

# plotting coefficients along lasso regularization path
plot(cv.lasso.q10$glmnet.fit, xvar="lambda", label = T)
if (!is.na(pc1.index)) {
  with(cv.lasso.q10$glmnet.fit, matlines(log(lambda), t(beta[pc1.index, , drop = FALSE]),
                                         lwd = 3, col = "blue"))
}
title("Coefficients along Lasso Regularizations Path", line = 2.5)
legend("topright", legend = paste("PC1", "path"), col = "blue", lwd = 2, cex = 0.8)







