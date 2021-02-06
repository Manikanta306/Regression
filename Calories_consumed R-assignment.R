#Calories_consumed-> predict weight gained using calories consumed

library(readr)
CC <- read_csv("Assignment files/calories_consumed.csv")
View(CC)

plot(CC)
summary(CC)
names(CC)[names(CC) == "Weight gained (grams)"] <- "WG"
names(CC)[names(CC) == "Calories Consumed"] <- "Cal.C"
View(CC)
cor(CC$WG, CC$Cal.C)

# I) First model without transformation
reg <- lm(CC$WG ~ CC$Cal.C)
summary(reg)
confint(reg, llevel = 0.95)
predict(reg, interval = "predict")

#Here the P-value is less than 0.05. Multiple R-Square value is 0.8968. We say that 89.68% this model will predict the correct output.

#II) We transform the variables to check the predicted values are better
reg1 <- lm(CC$WG ~ sqrt(CC$Cal.C))
summary(reg1)
confint(reg1, level = 0.95)
predict(reg1, interval = "predict")

#Here the P-value is less than 0.05. Multiple R-Square value is 0.8567. We say that 85.67% this model will predict the correct output.

#III) We transform the variables to check the predicted values are better

reg2 <- lm(CC$WG ~ log(CC$Cal.C))
summary(reg2)
confint(reg2, level = 0.95)
predict(reg2, interval = "predict")

#Here the P-value is less than 0.05. Multiple R-Square value is 0.8077. We say that 80.77% this model will predict the correct output.

#IV) We transform the variables to check the predicted values are better

reg3 <- lm(log(CC$WG) ~ CC$Cal.C)
summary(reg3)
confint(reg3, level = 0.95)
predict(reg3, interval = "predict")

# Here the P-value is less than 0.05. Multiple R-Square value is 0.8776. We say that 80.77% this model will predict the correct output.
# By applying transformation we see that the R squared value is decreasing. The model does not need further transformation. Hence multiple R squares 0.8968 is the best fit.
