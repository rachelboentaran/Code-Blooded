# Simple Decision Tree Regression on insurance dataset using tree package

# --- Load required library ---
library(tree)

# --- Load data ---
data_path <- "insurance.csv"
insurance <- read.csv(data_path, stringsAsFactors = FALSE)

# --- Preprocess ---
insurance$smoker <- factor(insurance$smoker, levels = c("no", "yes"))
insurance$sex    <- factor(insurance$sex)
insurance$region <- factor(insurance$region)

# --- Train/test split ---
set.seed(123)
n <- nrow(insurance)
train_index <- sample(1:n, size = 0.8 * n)
train <- insurance[train_index, ]
test  <- insurance[-train_index, ]

# --- Fit tree model ---
temp <- tree(charges ~ ., data = train, mindev = 0.001)

# Number of leaves and total nodes
cat("Number of leaves:", length(unique(temp$where)), "\n")
cat("Total number of nodes:", nrow(temp$frame), "\n")

# --- Cross-validation ---
set.seed(1357089)
cv_tree_model <- cv.tree(temp, , prune.tree)  # 10-fold cross-validation

# Determine optimal tree size
best_leaves <- cv_tree_model$size[max(which(cv_tree_model$dev == min(cv_tree_model$dev)))]
cat("Best number of leaves:", best_leaves, "\n")

# --- Prune the tree ---
pruned_tree <- prune.tree(temp, best = best_leaves)
cat("Pruned tree leaves:", length(unique(pruned_tree$where)), "\n")

# --- Plot the pruned tree ---
plot(pruned_tree, type = "uniform")
text(pruned_tree, col = "blue", label = c("yval"), cex = 0.8)

# --- Summary of pruned tree ---
summary(pruned_tree)

# --- Predict on test data and compute RMSE ---
tree_predictions <- predict(pruned_tree, newdata = test, type = "vector")
rmse_tree <- sqrt(mean((test$charges - tree_predictions)^2))
cat(sprintf("RMSE of pruned tree: %.2f\n", rmse_tree))

levels(insurance$smoker)
