install.packages("experimentr")
library(experimentr)
data()
data("easton")


#Q1: how political orientations affect attractiveness of a person
#    experiment uses individual survey on a person's political orientation and according basic demographic information(age,gender, orientation) on a dating match
#    treatment:subjects' political party preference
#    explanatory variable:subjects' political party preference
#    response variable:subjects' dating match political party preference

#Q2: regression analysis
s1 <- lm(treatment_republican_profile ~ republican, data=easton)
s2 <- lm(treatment_republican_profile ~ republican + age + male, data=easton)

summary(s1)
summary(s2)

## Visualize the relationship
#single variable regression
plot(easton$republican, easton$treatment_republican_profile,
     main = "subject orientation vs dating subject orientation",
     xlab = "subject orientation",
     ylab = "dating subject orientation",
     pch = 19,
     col = adjustcolor("blue", alpha=0.5))
abline(s1, col="red", lwd=2)
#multiple variable regression
plot(easton$republican + easton$age + easton$male, easton$treatment_republican_profile,
     main = "subject orientation with controls vs dating subject orientation",
     xlab = "subject orientation",
     ylab = "dating subject orientation",
     pch = 19,
     col = adjustcolor("green", alpha=0.5))
abline(s2, col="purple", lwd=2)

robust_s1 <- coeftest(s1, vcov=vcovCH(s1,type="HC1"))
robust_s2 <- coeftest(s2, vcov=vcovCH(s2, type="HC1"))

#Q2:regression analysis result
#In regression without controls, on average, 
#     a person with republican orientation will match 
#     with a person less than 0.2% probability of republican in dates

#In regression with controls of the person's demographics, on average,
#     a person with republican orientation will math
#     with a person 