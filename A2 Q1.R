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

#1a) 
#Importing data 
CEO_Salary<-data.frame(read_dta('HW2data/ceo_salary.dta'))
#CEO_Salary$lsalary<-trimws(CEO_Salary$lsalary)

summary(CEO_Salary)
sum(is.na(CEO_Salary))

#Subsetting data with natural logs
x<-data.frame(log(CEO_Salary$salary),log(CEO_Salary$sales),log(CEO_Salary$mktval))
CEO_Salary_subset<-subset(CEO_Salary,subset=CEO_Salary$lsalary-x[1]<0.0000000000001&
                          CEO_Salary$lsales-x[2]<0.0000000000001&
                          CEO_Salary$lmktval-x[3]<0.0000000000001)
#Running OLS regression 
Model_1<-lm(lsalary~lsales+lmktval+ceoten+ceotensq,CEO_Salary_subset)
summary(Model_1)

#1b) 




