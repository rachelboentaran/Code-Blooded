rm(list=ls())
data <- read.csv("insurance.csv", header = TRUE)

data$region <- as.factor(data$region)
data$sex <- as.factor(data$sex)
data$smoker <- as.factor(data$smoker)

set.seed(111)
tr <- sample(1:nrow(data), 1000)
train <- data[tr,]
test <- data[-tr,]

library(MASS)
library(rpart)

# tree
set.seed(666)

big.tree2 = rpart(charges~.,method="anova",data=train, minsplit=5,cp=.0005)
#NB: here method="anova" since we are doing a regression tree, so loss is MSE

#split until 5 observations per node or no improvement in loss give a very small
#penalty on complexity (cp=0.0005 is quite small)

#Count the leaves
length(unique(big.tree2$where))

#We now examine the CV plot, invoked by plotcp() function on the fitted tree:
#par(mfrow=c(1,1)) #back to one graph per window
plotcp(big.tree2) #CV plot

bestcp2=big.tree2$cptable[which.min(big.tree2$cptable[,"xerror"]),"CP"] #extract best cp value

#Finally, let's replot the best tree using the Fancy option:
best.tree2 = prune(big.tree2,cp=bestcp2) #get tree for best cp on CV
plot(best.tree2,uniform=TRUE)
text(best.tree2,digits=4,use.n=TRUE,fancy=FALSE,bg='lightblue') 
#note fancy=TRUE option would give a "nicer" look 
# (see end of file for even nicer looking plot using rpart.plot)

treefit2 = predict(best.tree2,newdata=test,type="vector")#prediction on test data
actual_tree2 <- test$charges

rmse <- sqrt(mean((treefit2 - test$charges)^2))

# Mean Absolute Error (MAE)
mae <- mean(abs(treefit2 - test$charges))

# R-squared
ss_res <- sum((treefit2 - test$charges)^2)
ss_tot <- sum((test$charges - mean(test$charges))^2)
r2 <- 1 - (ss_res / ss_tot)

cat("RMSE:", rmse, "\nMAE:", mae, "\nR-squared:", r2)

# plotting a nicer tree
library(rpart.plot)
prp(best.tree2)

#plot decision tree using custom arguments
prp(best.tree2,
    faclen=0, #use full names for factor labels
    extra=1, #display number of observations for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output

# Variable importance (in percent)
imp2 <- best.tree2$variable.importance
imp_percent2 <- imp2 / sum(imp2) * 100
print(round(imp_percent2, 1))

# exact leaf values 
summary(best.tree2)


# feature importance bar chart

# Convert to data frame for plotting
imp_df <- data.frame(
  Feature = names(imp_percent2),
  Importance = as.numeric(imp_percent2)
)

# Horizontal bar chart
barplot(
  imp_df$Importance,
  names.arg = imp_df$Feature,
  las = 2,               # rotate labels vertically
  col = "steelblue",
  main = "Feature Importance",
  ylab = "Importance (%)"
)
