

getwd()
setwd("*Download the Mall_Customers dataset and copy paste the path*")

#1 a)	Investigate data by using head(), summary() and class() functions.

mydata = read.csv(file = "Mall_Customers.csv")
mall = data.frame(mydata)
mall
head(mall)
summary(mall)
class(mall)
str(mall)
dim(mall)

#b)	Create a boxplot of Spending score by Genre (male, female
data.frame(mall$Genre)  
mall$Genre = as.factor(mall$Genre)
class(mall$Genre)
library(ggplot2)
mall
boxplot(mall$Spending.Score..1.100. ~ mall$Genre, xlab = "Gender", ylab = "Spending Score", main = "Expenditure by male/female")

#c)Create a new variable called  age_group which will divide customers into 3 different age groups based on their age.

mall$Age= with(mall,ifelse(mall$Age<30, "Age Group_1", ifelse(mall$Age>=30 & mall$Age<50,"Age Group_2", ifelse(mall$Age>=50, "Age Group_3", "0"))))
names(mall)[names(mall)=="Age"] = "Age Group"
head(mall)


#d)Create another new variable called high_spender which will indicate if a person has spending score greater than or equal to 80 or not. 

high_spender <- ifelse(mall$Spending.Score>=80, "Yes", "No")
table(high_spender)
mall$high_spender = high_spender
head(mall)
library(MASS)
library(ISLR)
mall$high_spender = as.factor(mall$high_spender)
str(mall)
mall$high_spender = as.factor(mall$high_spender)


#2)Build a logistic regression model to predict whether a person is a high spender based on their age_group, Genre, and Annual income. 

glm_fit = glm(mall$high_spender ~ mall$`Age Group`+ mall$Genre + mall$Annual.Income..k.., data = mall, family = "binomial")

summary(glm_fit)



#3) a)	Predict probability of being high spender using the model you fitted in Question 2, assuming a cutoff probability of 0.5

predict_probability = predict(glm_fit, data = mall, type = "response")
predicted_probability_glm_fit = ifelse(predict_probability>0.5,"Yes","No")


table(high_spender,predicted_probability_glm_fit)
mean(high_spender!=predicted_probability_glm_fit)
mean(high_spender==predicted_probability_glm_fit)
1- mean(high_spender==predicted_probability_glm_fit)

head(predict_probability)
head(predicted_probability_glm_fit)

#b) Predict probability of being high spender using the model you fitted in Question 3, assuming a cutoff probability of 0.3 
predict_probability = predict(glm_fit, data = mall, type = "response")
predicted_probability_glm_fit = ifelse(predict_probability>0.3,"Yes","No")
table(high_spender,predicted_probability_glm_fit)
mean(high_spender!=predicted_probability_glm_fit)
mean(high_spender==predicted_probability_glm_fit)
1- mean(high_spender==predicted_probability_glm_fit)
head(predict_probability)
head(predicted_probability_glm_fit)

