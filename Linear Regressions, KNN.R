rm(list=ls())
data <- read.csv("insurance.csv", header = TRUE)

# creating bin splits for BMI 
data$bmi_group <- cut(
  data$bmi,
  breaks = c(0, 18.4, 24.9, 29.9, Inf),
  labels = c("underweight (<18.5)", "healthy weight (18.5-24.9)", "overweight (25-29.9)", "obese (>30)"),
  include.lowest = FALSE
)

library(kknn)
set.seed(111)
tr <- sample(1:nrow(data), 1000)
train <- data[tr,]
test <- data[-tr,]


knn_model <- train.kknn(charges ~ . - region, data = train, kmax = 100, kernel = "rectangular")

# Plot MSE vs k
plot((1:100), knn_model$MEAN.SQU, type = "l", col = "blue", 
     main = "LOOCV MSE", xlab = "Number of neighbors", ylab = "MSE")

# Get best k
kbest <- knn_model$best.parameters$k
cat("Best k:", kbest, "\n")
# Best k = 21

# Make predictions on TEST data (not training data)
knnreg <- kknn(charges ~ . - region, train, test, k = kbest, kernel = "biweight")

# Get TEST predictions (this is the key fix!)
test_pred <- fitted(knnreg)  # These are predictions on test set

# Performance matrix
rmse_knn <- sqrt(mean((test$charges - test_pred)^2))
r2_knn <- cor(test$charges, test_pred)^2  # Most robust method
cat("KNN Test RMSE:", round(rmse_knn, 4), "KNN Test R²:", round(r2_knn, 4),"\n")
# KNN Test RMSE: 4623.623 KNN Test R²: 0.8611 

# Performance plots
# 1. Predicted vs Actual
plot(test$charges, test_pred, main = "Predicted vs Actual", 
     xlab = "Actual", ylab = "Predicted", pch = 19, col = "blue")
abline(a = 0, b = 1, col = "red", lwd = 2)
r2 <- cor(test$charges, test_pred)^2
text(min(test$charges), max(test_pred), paste("R² =", round(r2, 3)), pos = 4)

# 2. Residuals vs Fitted
# Get predictions on test set
test_pred <- fitted(knnreg)  # or predict(knnreg, newdata = test)
residuals <- test$charges - test_pred

plot(test_pred, residuals, main = "Residuals vs Fitted", 
     xlab = "Fitted", ylab = "Residuals", pch = 19, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)

# 3. Residuals histogram
hist(residuals, main = "Residuals Distribution", 
     xlab = "Residuals", col = "lightblue", breaks = 20)

# Linear Reg
library(dummies)  # for dummyVars
library(pls)      # for PCR
library(caret)    # for train/test split

simple_lm <- lm(charges ~ ., train) 
simple_lm$coefficients
predict_simple_lm <- predict(simple_lm, newdata = test)
rmse_basic_lm <- sqrt(mean((predict_simple_lm - test$charges)^2))
rsquared_basic_lm <- cor(test$charges, predict_simple_lm)
cat("RMSE", rmse_basic_lm, "rsquared lm", rsquared_basic_lm)
# RMSE 6410.051 rsquared lm 0.8554602, big difference in in sample and out of sample Rsquared

# 10-CV
train_control <- trainControl(method = "cv", number = 10)
model_cv <- train(charges ~ ., data = data, method = "lm", trControl = train_control)
model_cv$results
# RMSE: 6043.65, rsquared: 0.751

# model comparison plot 
# Compare with other models (e.g., linear model)

# Create comparison plot
plot(test$charges, test_pred, 
     main = "Model Comparison: Predicted vs Actual",
     xlab = "Actual Charges", 
     ylab = "Predicted Charges",
     col = "blue", 
     pch = 19, 
     cex = 0.6)

points(test$charges, predict_simple_lm, 
       col = "red", 
       pch = 17, 
       cex = 0.6)

abline(a = 0, b = 1, col = "black", lwd = 2)


# Add squared terms for important continuous variables
train$age_sq <- train$age^2
train$bmi_sq <- train$bmi^2

# Test the same
test$age_sq <- test$age^2
test$bmi_sq <- test$bmi^2

# Refit model
improved_model <- lm(charges ~ . , data = train)
pred_improved <- predict(improved_model, newdata = test)

# Calculate metrics
rmse_new <- sqrt(mean((test$charges - pred_improved)^2))
r2_new <- cor(test$charges, pred_improved)^2
cat("Improved model - RMSE:", round(rmse_new, 4), "R²:", round(r2_new, 4), "\n")
# Improved model - RMSE: 6366.218 R²: 0.7356 

# Convert factors to numeric for interaction terms
train$smoker_binary <- as.numeric(train$smoker == "yes")  
test$smoker_binary <- as.numeric(test$smoker == "yes")

# Create interaction terms
train$smoker_bmi <- train$smoker_binary * train$bmi
train$smoker_age <- train$smoker_binary * train$age
train$smoker_children <- train$smoker_binary * train$children

# Test the same
test$smoker_bmi <- test$smoker_binary * test$bmi
test$smoker_age <- test$smoker_binary * test$age
test$smoker_children <- test$smoker_binary * test$children

# Refit model 
interaction_model <- lm(charges ~ . , data = train)

# 4️⃣ Predict and evaluate
pred_int <- predict(interaction_model, newdata = test)
RMSE_int <- sqrt(mean((test$charges - pred_int)^2))
r2_int <- cor(test$charges, pred_int)^2
cat("Model with interactions - RMSE", round(RMSE_int, 6), "\n")
cat("Model with interactions - R²:", round(r2_int, 4), "\n")
# Model with interactions - RMSE 4858.298 
# Model with interactions - R²: 0.8493 
