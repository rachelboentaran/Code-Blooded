# --- Load data ---
#rm(list=ls())
#insurance <- read.csv("insurance", header = TRUE)

# Log Charges
insurance$charges_log <- log(insurance$charges)

# Factors
insurance$sex    <- factor(insurance$sex)
insurance$smoker <- factor(insurance$smoker)
insurance$region <- factor(insurance$region)

# Train/Test split
set.seed(123)
n <- nrow(insurance)
ix <- sample.int(nrow(insurance), floor(0.8 * nrow(insurance)))
train <- insurance[ix, ]
test  <- insurance[-ix, ]

# Log-linear model
base <- lm(charges_log ~ age + bmi + children + sex + smoker + region + children, data = train)
summary(base)$adj.r.squared #Adj R squared is 0.7621

# predict on test and back-transform
pred_log <- predict(base, newdata = test)
pred_usd <- expm1(pred_log)

# Visualise predicted vs actual charges
plot(test$charges, pred_usd,
     xlab = "Actual charges ($)", ylab = "Predicted charges ($)",
     main = "Test set: Actual vs Predicted", pch = 16)
abline(0, 1, lty = 2)  # 45° reference line 
# Can see that the predictions work well for charges around $10-15k, not so much for above that 

# Testing Key interactions
full_int <- lm(
  charges_log ~ age + bmi + children + sex + smoker + region +
    age:factor(children) + bmi:factor(children) + factor(children):sex + #Treating children as a factor
    factor(children):smoker + factor(children):region + age:bmi + age:sex +
    age:smoker + age:region + bmi:sex + bmi:smoker + bmi:region + sex:smoker +
    sex:region + smoker:region,
  data = train
)
 
# Drop all non-significant interactions 
#install.packages("MASS")
#library(MASS)
pruned <- stepAIC(full_int, direction = "backward", trace = FALSE)
summary(pruned) 

final_terms <- attr(terms(pruned), "term.labels")
final_terms
final_interactions <- final_terms[grepl(":", final_terms)]
final_interactions

m_final <- lm(
  charges_log ~ age + bmi + factor(children) + sex + smoker + region +
    age:factor(children) + smoker:factor(children) + age:bmi +
    age:sex + age:smoker + age:region + bmi:smoker + bmi:region +
    sex:smoker,
  data = train
)
summary(m_final)$adj.r.squared
#Adj R squared is 0.8320

# predict on test and back-transform
pred_log_final <- predict(m_final, newdata = test)
pred_usd_final <- expm1(pred_log_final)

plot(test$charges, pred_usd_final,
     xlab = "Actual charges ($)", ylab = "Predicted charges ($)",
     main = "Test set: Actual vs Predicted (Final)", pch = 16)
abline(0, 1, lty = 2)


