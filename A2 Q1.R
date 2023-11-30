#Assignment 2: Shivani Gaikwad (42664) 

#Initialise 

rm(list=ls())
options(digits=4, width=75)

#Importing libraries
library(ggplot2)
library(haven)
library(car)
library(dplyr)
library(moments)
library(sandwich)
library(lmtest)
library(MASS)

#1a) 
#Importing data 
CEO_Salary<-data.frame(read_dta('HW2data/ceo_salary.dta'))

summary(CEO_Salary)
sum(is.na(CEO_Salary))

#Running OLS regression 
Model_1<-lm(lsalary~lsales+lmktval+ceoten+ceotensq,CEO_Salary)
summary(Model_1)

#1b) Statistics for regression explanatory variables
Count<-count(CEO_Salary[,c(11:12,6,14)]) 
Mean<-sapply(CEO_Salary[,c(11:12,6,14)],mean)
SD<-sapply(CEO_Salary[,c(11:12,6,14)],sd)
Minimum<-sapply(CEO_Salary[,c(11:12,6,14)],min)
Maximum<-sapply(CEO_Salary[,c(11:12,6,14)],max)

#1c) Re-estimating model with White SE
White_SE<-coeftest(Model_1,vcov=vcovHC(Model_1,type="HC0"))
print(White_SE)

#1d) Standardised residuals 
Stand_residuals<-rstandard(Model_1)
Above_threshold<-ifelse(abs(Stand_residuals)>1.96,"TRUE","FALSE")
number<-sum(Above_threshold=="TRUE")
print(paste0("The number of standardised residuals with an absolute value exceeding 1.96 is: ", number))

#Probability of iid draws from standard normal distribution

 #prob of a standard normal distribution <-2 or >2 
prob <- 2*(1-pnorm(2)) #expected number of observations beyond ±2 in 177 draws 
num_of_obs <- prob * 177 
print(paste0("The number of standardised residuals expected to be above 2 are: ",
             round(num_of_obs,0)))
      
#Original code
Above_threshold_2<-ifelse(abs(Stand_residuals)>2,"TRUE","FALSE")
num<-sum(Above_threshold_2=="TRUE")
Probability<-num/177*100
print(Probability)

#1e) Dummy and interaction terms 
Model_1a<-lm(lsalary~lsales+lmktval+ceoten+ceotensq+college+(college*lsales),CEO_Salary)
summary(Model_1a)

#College-educated represented by 1. CEO salary increases if not college-educated? 
#For college-educated CEOs, the effect of sales on salary is positive. 

#1f) Correction of measurement error 
lsales_adjusted=CEO_Salary[11]-0.10*(CEO_Salary[11])
CEO_Salary<- cbind(CEO_Salary,lsales_adjusted)
Model_1b<-lm(lsalary~unlist(lsales_adjusted)+lmktval+ceoten+ceotensq,CEO_Salary)
summary(Model_1b)
#Coefficient on lsales_adjusted is higher compared to coefficient on lsales. 

#1g) Re-estimating the model 
CEO_Salary_subset<- cbind(CEO_Salary,Stand_residuals)
CEO_Salary_subset_b<-subset(CEO_Salary_subset,abs(Stand_residuals)<=2)
Model_1b<-lm(lsalary~lsales+lmktval+ceoten+ceotensq,CEO_Salary_subset_b)
summary(Model_1b)

