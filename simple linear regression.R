#1) Delivery_time -> Predict delivery time using sorting time 

# Load Delivery time data set
library(readr)

delivery_data <- read_csv(file.choose())

head(delivery_data)

delivery_data <- as.data.frame(delivery_data)
head(delivery_data)
dim(delivery_data)

#Changing column names
colnames(delivery_data)[1] <- "Delivery_time"
colnames(delivery_data)[2] <- "Sorting_time"

head(delivery_data)

#Correlation Coefficient

cor(delivery_data$Delivery_time,delivery_data$Sorting_time)

#Scatter plot

plot(delivery_data$Sorting_time,delivery_data$Delivery_time)

attach(delivery_data)

#Model 

delivery.model <- lm(Delivery_time~Sorting_time)
summary(delivery.model)

#R^2=0.6823, Adj,R^2=0.6655

# Logarithmic Model

# x = log(Sorting time); y = Delivery time
plot(log(Sorting_time), Delivery_time)
cor(log(Sorting_time), Delivery_time)

deliverymodel_log <- lm(Delivery_time ~ log(Sorting_time))   # lm(Y ~ X)
summary(deliverymodel_log)

#R^2=0.6954, Adj.R^2=0.6794

# Exponential Model

# x = Sorting time and y = log(Delivery time)

plot(Sorting_time, log(Delivery_time))

cor(Sorting_time, log(Delivery_time))

deliverymodel_exp <- lm(log(Delivery_time) ~ Sorting_time)  #lm(log(Y) ~ X)
summary(deliverymodel_exp)

#R^2 = 0.7109, Adj.R^2=0.6957


# Polynomial model with 2 degree (quadratic model)

plot(Sorting_time*Sorting_time, log(Delivery_time))

cor(Sorting_time*Sorting_time, log(Delivery_time))

deliverymodel_2degree <- lm(log(Delivery_time)~Sorting_time+I(Sorting_time*Sorting_time))
summary(deliverymodel_2degree)

#R^2 = 0.7649, Adj.R^2= 0.7387

#  Polynomial model with 3 degree

deliverymodel_3degree<-lm(log(Delivery_time)~Sorting_time + I(Sorting_time*Sorting_time) + I(Sorting_time*Sorting_time*Sorting_time))
summary(deliverymodel_3degree)

#R^2 =0.7819, Adj.R^2=0.7434



#2) Salary_hike -> Build a prediction model for Salary_hike

library(readr)

salarydata <- read_csv(file.choose())

salarydata <-as.data.frame(salarydata)
head(salarydata)

plot(salarydata$YearsExperience,salarydata$Salary)
cor(salarydata$YearsExperience,salarydata$Salary)

summary(salarydata)
dim(salarydata)

hist(salarydata$YearsExperience)
hist(salarydata$Salary)

boxplot(salarydata$YearsExperience)
boxplot(salarydata$Salary)

attach(salarydata)
salary.model <- lm(Salary~YearsExperience)
summary(salary.model)

#R^2=0.957, Adj.R^2=0.9554

