rm(list=ls())
insurance <- read.csv("insurance", header = TRUE)

BMI = insurance$bmi
Charges = insurance$charges
Smoker = insurance$smoker

# 1) Target
hist(Charges, breaks=40, main="Distribution of Charges", xlab="Charges")

# 2) Charges by smoker
boxplot(Charges ~ smoker, data=insurance, ylab="Charges", xlab="Smoker", main="Charges by smoker status")

# 3) BMI vs charges 
plot(bmi, charges, col=insurance$smoker, pch=16, ylab="Charges",xlab = 'BMI',
     main="BMI, smoker status vs Charges"); legend("topleft", c("no","yes"), pch=16, col=1:2)

# 4) Age vs charges 
plot(insurance$age, insurance$charges, col=insurance$smoker, pch=16, ylab= 'Charges', xlab = 'Age',
     main="Age, smoker status vs Charges"); legend("topleft", c("no","yes"), pch=16, col=1:2)

