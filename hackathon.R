rm(list=ls())
data <- read.csv("insurance.csv", header = TRUE)

grp1 = kmeans(scale(data[,c("age","charges")]), centers = 11, nstart = 20)
grp1$cluster

#visualise
plot(data$age, data$charges, main = "K = 11", xlab="age", ylab="charges", type="n") 
text(data$age, data$charges, labels=data$age, col = rainbow(11)[grp1$cluster]) 

#Choosing K with AICc and BIC 

n = nrow(data) #get sample size

d=2 #dimension of the data (for naive degrees of freedom estimate)

kt = 1:20 #consider K from 1 to 20
bic = rep(0,20) #blanks for BIC
aicc = rep(0,20) #blanks for AIC
for(ii in 1:20) { #loop over K
  fit = kmeans(scale(data[,c("age","charges")]), centers = ii, nstart = 20) #do k-means with ii clusters
  df = d*ii #estimate for degrees of freedom
  #The measure of sum of squares here is total within sum of squares: 
  bic[ii] = fit$tot.withinss + log(n)*df #BIC from the slides
  aicc[ii] = fit$tot.withinss + 2*df*n/(n-df-1) #AICc from the slides
}

#Get selected K from the IC:
bicsel=which.min(bic) #K=11
aiccsel=which.min(aicc) #K=17

#Plot the AICc and BIC curves
plot(kt, bic, main = "IC", xlab="K", ylab="IC", type="l", col = "red", ylim = c(40,200))
lines(kt, aicc, type="l", col = "blue")
lines(kt, bic, type="l", col = "red")
legend("topleft", c("BIC","AICc"), lty=c(1,1) ,col=c("red","blue"))


# after clustering using N = 11, realise that charges have a distinct category in terms of its bin
# Example: create bins for 'charges'
data$charge_group <- cut(
  data$charges,
  breaks = c(0, 13000, 30000, 50000, Inf),
  labels = c("<13K", "13-30K", "30-50K", ">50K"),
  include.lowest = TRUE
)

# create bin for BMI
data$bmi_group <- cut(
  data$bmi,
  breaks = c(0, 18.4, 24.9, 29.9, Inf),
  labels = c("underweight (<18.5)", "healthy weight (18.5-24.9)", "overweight (25-29.9)", "obese (>30)"),
  include.lowest = FALSE
)

library(ggplot2)
ggplot(data, aes(x = charge_group, fill = charge_group)) +
  geom_bar() +
  labs(title = "Distribution of Insurance Charges by Group", x = "Charge Range", y = "Count")
# most people in the 13k range, but not very useful information


ggplot(data, aes(x = charge_group, y = age)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Charge Range", x = "Charge Range", y = "Age")
# not very big difference, but generally increase age points to higher charges


ggplot(data, aes(x = bmi_group, y = log(charges), fill = bmi_group)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Log(Charges) by BMI Group",
       x = "BMI Group", y = "Log(Charges)")

aggregate(charges ~ bmi_group, data = data, mean)

anova_result <- aov(charges ~ bmi_group, data = data)
summary(anova_result)

library(dplyr)

bmi_summary <- data %>%
  group_by(bmi_group) %>%
  summarise(
    mean_charge = mean(charges, na.rm = TRUE),
    median_charge = median(charges, na.rm = TRUE)
  )

ggplot(bmi_summary, aes(x = bmi_group, y = median_charge, fill = bmi_group)) +
  geom_col() +
  geom_text(aes(label = round(median_charge, 0)), vjust = -0.5) +
  labs(title = "Median Insurance Charges by BMI Group",
       x = "BMI Group", y = "Median Charges") +
  theme_minimal(base_size = 15) +   # changes overall font size
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.2),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
         
  )



# decision tree

