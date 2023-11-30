#Assignment 2, Question 2 

#Clearing workspace
rm(list = ls())
options(digits=4, scipen=999)

#Importing libraries 
library(car)
library(haven)
library(moments)
library(dplyr)
library(AER) 
library(plm)
library(lmtest)
library(lfe)
library(stargazer)
library(ggplot2)

#Importing data 
Local_returns<-read_dta('HW2data/local_returns.dta')
#Check panel data 
summary(Local_returns)
sum(is.na(Local_returns))

#2a) Summarising panel data 
dim(Local_returns)
#Plot boxplot to check panel data 
Plot <- ggplot(Local_returns, aes(x=date))
Plot + geom_bar( stat = "count")+theme_minimal()
Local_returns<- pdata.frame(Local_returns, index = c("permno", "date"))
summary(Local_returns[, c(1,6)])
#The panel is unbalanced. 

#by function 
#plotting graphs to check autocorrelation across time 

#2b) Panel data statistics by city 
City_statistics<-Local_returns%>%group_by(city)%>%summarise(mean_returns=mean(ret),
                                           volatility=sd(ret)) 
print(City_statistics)
#Summarising maximum/minimum values and corresponding city number 
c(sapply(City_statistics[2:3],max),sapply(City_statistics[2:3],which.max))
c(sapply(City_statistics[2:3],min),sapply(City_statistics[2:3],which.min))

#The difference between the highest return and lowest return is 0.0106 which is observed between 
# Cities 8 and 11. There is a greater difference of 0.0957 between the highest and lowest volatilities
# which is observed between Cities 17 and 3. 

#2d) Pooled OLS without dummy variables 
Model_2<-lm(ret~city_returns+indret,Local_returns)
summary(Model_2)

#2e) Test for heteroskedasticity of residuals 
BP_test<-bptest(Model_2)
ifelse(BP_test[["p.value"]]<0.05,"Heteroskedasticity exists","Heteroskedasticity does not exist")
  
#2f) Pooled OLS with time fixed effects 
Model_2b<-lm(ret~city_returns+indret+factor(date)-1,data=Local_returns)
summary(Model_2b)


#felm (Model|fixed effects/0 for OLS| IV |Clustering)

#2g) Pooled OLS with both time and firm fixed effects (without clustering)
Model_2c<-felm(ret~city_returns+indret|0|0|0,data=Local_returns)
summary(Model_2c) 

#To compare points f and g 
stargazer(Model_2b,Model_2c,type="text")

#By plm - gives same result 
Model_2cplm<-plm(ret~city_returns+indret,Local_returns, effect = c("twoways"), model = "pooling")
summary(Model_2cplm)
stargazer(Model_2c,Model_2ctest,type="text")


#2h) Pooled OLS with both time and firm fixed effects (double clustering of SEs)
Model_2d<-felm(ret~city_returns+indret|0|0|permno+date,data=Local_returns)
summary(Model_2d)

#Model_2dtest<-felm(ret~city_returns+indret|permno+date|0|permno+date,data=Local_returns)
#summary(Model_2dtest)

Model_2dplm<-coeftest(Model_2cplm, vcov=vcovDC(Model_2cplm, type="sss"))
print(Model_2dplm) #Results are same

#To compare points g and h 
stargazer(Model_2c[["se"]],Model_2d[["cse"]],type="text",
          title = "Standard errors - Unclustered and Clustered")
#To compare points f, g and h 
stargazer(Model_2b,Model_2c,Model_2d,type="text")

#2i) Fixed effects (Within variation) #Individual FE needed
Fixed_effects<-plm(ret~city_returns+indret,Local_returns, effect = c("individual"),model = "within" )
summary(Fixed_effects) 
#(proj R sq = demeaned one, Full model = original R sq).  

#2j) First difference estimate 
First_difference<-plm(ret~city_returns+indret,Local_returns,model = "fd" )
summary(First_difference)

#2k) Comparison between i,j and h - Use stargazer to compare models. 
stargazer(Model_2d,Fixed_effects,First_difference,type="text")

#Code to convert to Latex code: 
#stargazer(Model_2d,Fixed_effects,First_difference,type="latex")

#2l) Dummy and interaction terms  
dummy_SF=ifelse(Local_returns$city==10,1,0)
Local_returns<- cbind(Local_returns,dummy_SF)
Model_2e<-lm(ret~city_returns+indret+I(dummy_SF*city_returns),Local_returns)
summary(Model_2e)

