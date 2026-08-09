# Load required packages
library(AER)
library(sandwich)
library(lmtest)


# Load Swiss Labor data
data("SwissLabor")

# Examine the structure of the data
str(SwissLabor)
# Convert participation to numeric 
SwissLabor$participation_num <- as.numeric(SwissLabor$participation) - 1  # Convert "yes"/"no" to 1/0


## Create variables
education_years <- SwissLabor$education
nonlabor_income <- SwissLabor$income
labor_participation <- SwissLabor$participation
kid_number <- SwissLabor$youngkids+SwissLabor$oldkids


# conditional mean of participation (mean of participate and mean of not participate)
mean(SwissLabor[SwissLabor$participation=='yes', 'education'])
mean(SwissLabor[SwissLabor$participation=='no', 'education'])

## Regression
# Basic regression (education only)
mod1 <- lm(participation_num ~ education, data=SwissLabor)
# Multiple regression (adding controls)
mod2 <- lm(participation_num ~ education + income + age + youngkids +oldkids, data=SwissLabor)

# Model with education + income only
mod3 <- lm(participation_num ~ education + income, data=SwissLabor)
# Model with education + age only
mod4 <- lm(participation_num ~ education + age, data=SwissLabor)
# Model with education + youngkids only
mod5 <- lm(participation_num ~ education + youngkids, data=SwissLabor)
# Model with education + child-related variables
mod6 <- lm(participation_num ~ education + kid_number, data=SwissLabor)

summary(mod3)
summary(mod4)
summary(mod5)
summary(mod6)

#attempt to compare the effect of omitting age squared on the basic model
summary(mod1)
summary(mod4)
mod7 <- lm(participation_num ~ education + age*age, data=SwissLabor)
summary(mod7)

## Visualize the relationship
#single variable regression
plot(SwissLabor$education, SwissLabor$participation_num,
     main = "Education vs Labor Force Participation",
     xlab = "Years of Education",
     ylab = "Labor Force Participation (0/1)",
     pch = 19,
     col = adjustcolor("blue", alpha=0.5))
abline(mod1, col="red", lwd=2)
#multiple variable regression
plot(SwissLabor$education+SwissLabor$income+SwissLabor$age+SwissLabor$youngkids+SwissLabor$oldkids, SwissLabor$participation_num,
     main = "Education vs Labor Force Participation",
     xlab = "Years of Education with controls",
     ylab = "Labor Force Participation (0/1)",
     pch = 19,
     col = adjustcolor("green", alpha=0.5))
abline(mod1, col="purple", lwd=2)


## Sensitivity analysis
# Get robust standard errors for both models
robust_mod1 <- coeftest(mod1, vcov = vcovHC(mod1, type = "HC1"))
robust_mod2 <- coeftest(mod2, vcov = vcovHC(mod2, type = "HC1"))

# Print results
cat("\nBasic Model (with robust standard errors):\n")
print(robust_mod1)

cat("\nFull Model (with robust standard errors):\n")
print(robust_mod2)

# Compare models with full summary
cat("\nBasic Model Summary:\n")
print(summary(mod1))

cat("\nFull Model Summary:\n")
print(summary(mod2))
