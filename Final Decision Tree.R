##############################################
# 1. Setup & Data Preprocessing
##############################################
rm(list = ls())

# Load dataset
data <- read.csv("insurance.csv", header = TRUE)

# Convert categorical variables to factors
data$region <- as.factor(data$region)
data$sex <- as.factor(data$sex)
data$smoker <- as.factor(data$smoker)

# Split into training and test sets
set.seed(111)
tr <- sample(1:nrow(data), 1000)
train <- data[tr, ]
test <- data[-tr, ]


##############################################
# 2. Build Decision Tree
##############################################
library(MASS)
library(rpart)

set.seed(666)

# Fit regression tree (method = "anova" since target is continuous)
big.tree2 <- rpart(
  charges ~ ., 
  method = "anova", 
  data = train, 
  minsplit = 5, 
  cp = 0.0005
)

# Number of leaf nodes
length(unique(big.tree2$where))


##############################################
# 3. Cross-Validation & Model Pruning
##############################################
# Plot cross-validation results
plotcp(big.tree2)

# Extract best complexity parameter (cp)
bestcp2 <- big.tree2$cptable[which.min(big.tree2$cptable[, "xerror"]), "CP"]

# Prune the tree using optimal cp
best.tree2 <- prune(big.tree2, cp = bestcp2)


##############################################
# 4. Visualise the Tree
##############################################
# Basic plot
plot(best.tree2, uniform = TRUE)
text(best.tree2, digits = 4, use.n = TRUE, fancy = FALSE, bg = 'lightblue')

##############################################
# 5. Model Evaluation on Test Set
##############################################
# Predictions
treefit2 <- predict(best.tree2, newdata = test, type = "vector")
actual_tree2 <- test$charges

# Evaluation metrics
rmse <- sqrt(mean((treefit2 - test$charges)^2))
mae <- mean(abs(treefit2 - test$charges))
ss_res <- sum((treefit2 - test$charges)^2)
ss_tot <- sum((test$charges - mean(test$charges))^2)
r2 <- 1 - (ss_res / ss_tot)

cat("RMSE:", rmse, "\nMAE:", mae, "\nR-squared:", r2, "\n")


##############################################
# 6. Feature Importance
##############################################
# Extract and normalise variable importance
imp2 <- best.tree2$variable.importance
imp_percent2 <- imp2 / sum(imp2) * 100
print(round(imp_percent2, 1))

# Display leaf node summaries
summary(best.tree2)


##############################################
# 7. Feature Importance Visualisation
##############################################
# Convert to data frame for plotting
imp_df <- data.frame(
  Feature = names(imp_percent2),
  Importance = as.numeric(imp_percent2)
)

# Plotting bar chart
barplot(
  imp_df$Importance,
  names.arg = imp_df$Feature,
  las = 2,                # vertical labels
  col = "steelblue",
  main = "Feature Importance (%)",
  ylab = "Importance (%)"
)