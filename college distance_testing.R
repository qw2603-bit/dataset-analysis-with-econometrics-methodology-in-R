#problem:estimate the effect of education (years of schooling) on test scores. 
#However, education can be endogenous due to factors like family background, ability, and motivat

#instrumental variable: college distance
   #relevance: college proximity can affect education quality
   #independence: college proximity has no influence on the effect of education to test scores
   #exclusion restriction: college proximity can only influence test score through influencing education



library(AER)
data("CollegeDistance")

# check the correlation between college distance and education
cor(CollegeDistance$education, CollegeDistance$distance)


# perform the first stage regression

#simple regression of college distance and education
s1 <- lm(education ~ distance, data = CollegeDistance)
# calculate coefficient
coeftest(s1, vcov = vcovHC, type = "HC1")

# inspect the R^2 of the first stage regression (how relevant is college distance to education)
summary(s1)$r.squared
# store the predicted values
lcigp_pred <- s1$fitted.values


#perform second stage regression

# perform TSLS using 'ivreg()'
iv <- ivreg(score ~ education | distance, data = CollegeDistance)

coeftest(iv, vcov = vcovHC, type = "HC1")