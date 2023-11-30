#Assignment 2, Question 3

#Clearing workspace
rm(list=ls())
options(digits=4,scipen=999) 

#Loading libraries 
library(moments)
library(car)
library(sandwich)
library(dplyr)
library(haven)
library(AER) 

#Importing data 
Pension_data<-read_dta('HW2data/pension.dta')
summary(Pension_data)
sum(is.na(Pension_data))

#3a) Linear probability model by OLS 
Model_1<-lm(pira~p401k+inc+incsq+age+agesq,Pension_data)
summary(Model_1)
#Since LPM violates homoskedasticity, we also calculate robust standard errors 
coeftest(Model_1,vcov. = vcovHC,(type="HC1"))

#3d) Reduced form 
Model_1b<-lm(p401k~e401k+inc+incsq+age+agesq,Pension_data)
summary(Model_1b)
coeftest(Model_1b,vcov. = vcovHC,(type="HC1"))
#Testing significant partial correlation
Test<-linearHypothesis(Model_1b,c("e401k"),rhs=c(0),test=c("F"),vcov. = vcovHC,type="HC1")
ifelse(Test[2,4]<0.05, "e401k has significant partial correlation with p401k","e401k does not have significant partial correlation with p401k")

# As per F test, we reject the null hypothesis, the instrument is not weak. 

#3e) Structural equation by IV 
Model_1c<-ivreg(pira~p401k+inc+incsq+age+agesq|e401k+inc+incsq+age+agesq, data=Pension_data)
summary(Model_1c,vcov = vcovHC,(type="HC1"), diagnostics = TRUE)




