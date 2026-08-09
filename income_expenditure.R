library(AER)
help(CreditCard)
data("CreditCard")

# income-expenditure
cor(CreditCard$expenditure,CreditCard$income)       #treatment effect X on Y
CreditCard$owner_numeric <- as.numeric(factor(CreditCard$owner,levels = c("yes", "no"),
                                      ordered = TRUE))      #change from dummy to numeric

#check IV conditions
cor(CreditCard$expenditure,CreditCard$income)       #treatment effect X on Y
cor(CreditCard$expenditure,CreditCard$owner_numeric)   #exclusion restriction Z and Y
cor(CreditCard$income,CreditCard$owner_numeric)    #relevance Z to D
    #independence is met from the exogenous nature of owning a house to treatment effect
      #The fact of owning a house doesn't affect how income will influence expenditure 


#normal regression
#simple regression of x on y
s1 <- lm(expenditure ~ income, data = CreditCard)
s2 <- lm(expenditure ~ income
         +active+selfemp+age, 
         data=CreditCard)
# calculate robust coefficient
summary(s1)
summary(s2)
plot(s2)
plot(CreditCard$expenditure~ CreditCard$income)

cutoff <- 3
rdd <- lm(expenditure ~ card + income + I(income - cutoff)*(income >= cutoff), data = CreditCard)
plot(rdd)

#iv regression
#first stage regression of z on x
r1 <- lm(income ~ owner_numeric , data= CreditCard)
summary(r1)

#second stage regression of x on y
# perform TSLS using 'ivreg()'
iv <- ivreg(expenditure ~ income | owner_numeric, data = CreditCard)

iv2 <- ivreg(expenditure ~ income
             +active+selfemp+age
             | .-income+owner_numeric, 
             data = CreditCard)
summary(iv)
summary(iv2)


