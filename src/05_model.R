library(readr)
library(dplyr)
library(ggplot2)
library(caTools)
library(stargazer)

# Variables
# bband: ACS broadband subscription point estimate            
# bbandmin: ACS broadband subscription point estimate minus MOE
# bbandmax: ACS broadband subscription point estimate plus MOE        
# conn10min: FCC broadband subscription at 10mbps range minimum (10mps interval)       
# conn10max: FCC broadband subscription at 10mbps range maximum (10mps interval)           
# conn200min: FCC broadband subscription at 200kbps range minimum (200kbps interval)            
# conn200max: FCC broadband subscription at 200kbps range maximum (200kbps interval)           
# connmin: FCC broadband subscription at 10mbps range minimum (generous interval)              
# connmax: FCC broadband subscription at 200kbps range maximum (generous interval)               
# State
# County     
# primRUCA: Primary RUCA code        
# secRUCA: Secondary RUCA code         
# TractPop10: Tract population in 2010       
# LandSqmile10: Land area in square mile, 2010 geometries     
# PopDens10: Population density in 2010      
# urbanicity: RUCA code collapsed (rural, small town, micropolitan, metropolitan)      
# acs_within_fcc: dichotomous indicator of whether the ACS-/+MOE falls entirely within FCC generous interval (10mbps min, 200kbps max)
# population: ACS 2014-18 population estimate      
# area: ACS 2014-18 land area              
# less_hs: population proportion with less than high school education, ACS 2014-18          
# poverty: population proportion in poverty, ACS 2014-18              
# age_65_older: population proportion aged 65+, ACS 2014-18            
# hispanic: population proportion Hispanic, ACS 2014-18                
# black: population proportion Black, ACS 2014-18                
# family: proportion family households, ACS 2014-18                 
# foreign: population proportion foreign-born, ACS 2014-18                
# workfromhome: population proportion working from home, ACS 2014-18            
# longcommute: population proportion with 30+min commute, ACS 2014-18             
# assistance: population proportion receiving social assistance, ACS 2014-18              
# unemploy: population proportion in labor force but unemployed, ACS 2014-18               
# vacant: proportion vacant properties, ACS 2014-18                 
# renters: population proportion renters, ACS 2014-18                 
# yearbuilt: median year property built, ACS 2014-18             
# rentburden: population proportion rent burdened (30%+ rent), ACS 2014-18            
# nointernet: population proportion without internet access, ACS 2014-18             
# st: state abbreviation              
# stname: state name          
# cty: county             
# ctyname: county name         
# trctname: tract name        
# job_all: number of jobs, LODES 2016 workplace file        
# job_age29less: proportion jobs of workers age 29 or younger, LODES 2016 workplace file
# job_age3054: proportion jobs of workers age 30-54, LODES 2016 workplace file
# job_age55ovr: proportion jobs of workers age 55+, LODES 2016 workplace file    
# job_earn1250less: proportion jobs of workers earning 1250 or less monthly, LODES 2016 workplace file
# job_earn12513333: proportion jobs of workers earning 1251-3333 monthly, LODES 2016 workplace file
# job_earn3334ovr: proportion jobs of workers earning 3334+ monthly, LODES 2016 workplace file
# job_white: proportion jobs of white workers, LODES 2016 workplace file   
# job_black: proportion jobs of Black workers, LODES 2016 workplace file        
# job_native: proportion jobs of native American workers, LODES 2016 workplace file               
# job_asian: proportion jobs of Asian workers, LODES 2016 workplace file                
# job_pacific: proportion jobs of Pacific Islander workers, LODES 2016 workplace file              
# job_multirace: proportion jobs of multirace workers, LODES 2016 workplace file            
# job_nonhisp: proportion jobs of non-Hispanic workers, LODES 2016 workplace file             
# job_hisp: proportion jobs of Hispanic workers, LODES 2016 workplace file                
# job_educlesshs: proportion jobs of workers with less than high school education, LODES 2016 workplace file        
# job_educhsequiv: proportion jobs of workers with high school or equivalent, LODES 2016 workplace file  
# job_educsomecol: proportion jobs of workers with some college, LODES 2016 workplace file 
# job_educba: proportion jobs of workers with BA+ education, LODES 2016 workplace file      
# job_male: proportion jobs of male workers, LODES 2016 workplace file    
# job_female: proportion jobs of female workers, LODES 2016 workplace file        
# job_all_private: number of private jobs, LODES 2016 workplace file  
# firmage_01: proportion of private jobs in firms age 0-1 years, LODES 2016 workplace file     
# firmage_23: proportion of private jobs in firms age 2-3 years, LODES 2016 workplace file      
# firmage_45: proportion of private jobs in firms age 4-5 years, LODES 2016 workplace file      
# firmage_610: proportion of private jobs in firms age 6-10 years, LODES 2016 workplace file     
# firmage_11ovr: proportion of private jobs in firms age 11+ years, LODES 2016 workplace file  
# firmsize_019: proportion of private jobs in firms size 0-19 employees, LODES 2016 workplace file    
# firmsize_2049: proportion of private jobs in firms size 20-49 employees, LODES 2016 workplace file   
# firmsize_50249: proportion of private jobs in firms size 50-249 employees, LODES 2016 workplace file   
# firmsize_250499: proportion of private jobs in firms size 250-499 employees, LODES 2016 workplace file  
# firmsize_500ovr: proportion of private jobs in firms size 500+ employees, LODES 2016 workplace file 
# geometry: shape     

# Set seed
set.seed(2410) 

# US tracts with ACS-FCC broadband subscription congruence indicator (ACS 2014-18; FCC 2016), ACS covariates (2014-16), and LODES (2016) covariates.
data <- read_rds("./data/working/data_int_covars_acs_lodes.Rds")
data <- data %>% select(acs_within_fcc, urbanicity, PopDens10,
                        less_hs, poverty, age_65_older, hispanic, black, family, foreign, workfromhome, longcommute, assistance, unemploy, vacant, renters, yearbuilt, rentburden, nointernet,
                        job_age3054, job_age55ovr, job_earn12513333, job_earn3334ovr, job_white, job_black, job_asian, job_hisp, 
                        job_educhsequiv, job_educsomecol, job_educba, job_female, firmage_23,  firmage_45, firmage_610, firmage_11ovr, 
                        firmsize_2049, firmsize_50249, firmsize_250499, firmsize_500ovr)
data <- na.omit(data)


#
# All --------------------------------------------------------------------------------------------------------------------------------
#

# Split data
split <- sample.split(data$acs_within_fcc, SplitRatio = 0.75)
dataTrain = subset(data, split == TRUE)
dataTest = subset(data, split == FALSE)

# Train
mymodel <- glm(acs_within_fcc ~., data = dataTrain, family = binomial(link = "logit"))

# Test
mymodelPreds <- predict(mymodel, dataTest, type = "response")
mymodelPreds <- ifelse(mymodelPreds > 0.5, 1, 0)

table(mymodelPreds, dataTest$acs_within_fcc)

accuracy <- table(mymodelPreds, dataTest$acs_within_fcc)
sum(diag(accuracy))/sum(accuracy)

# Logistic regression on entire dataset
mymodel <- glm(acs_within_fcc ~., data = data, family = binomial(link = "logit"))
summary(mymodel)
stargazer(mymodel, type = "text", apply.coef = exp, single.row = TRUE, no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))


#
# Rural --------------------------------------------------------------------------------------------------------------------------------
#

rural <- data %>% filter(urbanicity == "Rural") %>% select(-urbanicity)

# Split data
split <- sample.split(rural$acs_within_fcc, SplitRatio = 0.75)
ruralTrain = subset(rural, split == TRUE)
ruralTest = subset(rural, split == FALSE)

# Train
ruralModel <- glm(acs_within_fcc ~., data = ruralTrain, family = binomial(link = "logit"))

# Test
ruralModelPreds <- predict(ruralModel, ruralTest, type = "response")
ruralModelPreds <- ifelse(ruralModelPreds > 0.5, 1, 0)

table(ruralModelPreds, ruralTest$acs_within_fcc)

accuracy <- table(ruralModelPreds, ruralTest$acs_within_fcc)
sum(diag(accuracy))/sum(accuracy)

# Logistic regression on entire dataset
ruralModel <- glm(acs_within_fcc ~., data = rural, family = binomial(link = "logit"))
summary(ruralModel)
stargazer(ruralModel, type = "text", apply.coef = exp, single.row = TRUE, no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))


#
# Small town --------------------------------------------------------------------------------------------------------------------------------
#

small <- data %>% filter(urbanicity == "Small town") %>% select(-urbanicity)

# Split data
split <- sample.split(small$acs_within_fcc, SplitRatio = 0.75)
smallTrain = subset(small, split == TRUE)
smallTest = subset(small, split == FALSE)

# Train
smallModel <- glm(acs_within_fcc ~., data = smallTrain, family = binomial(link = "logit"))

# Test
smallModelPreds <- predict(smallModel, smallTest, type = "response")
smallModelPreds <- ifelse(smallModelPreds > 0.5, 1, 0)

table(smallModelPreds, smallTest$acs_within_fcc)

accuracy <- table(smallModelPreds, smallTest$acs_within_fcc)
sum(diag(accuracy))/sum(accuracy)

# Logistic regression on entire dataset
smallModel <- glm(acs_within_fcc ~., data = small, family = binomial(link = "logit"))
summary(smallModel)
stargazer(smallModel, type = "text", apply.coef = exp, single.row = TRUE, no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))


#
# Micropolitan --------------------------------------------------------------------------------------------------------------------------------
#

micro <- data %>% filter(urbanicity == "Micropolitan") %>% select(-urbanicity)

# Split data
split <- sample.split(micro$acs_within_fcc, SplitRatio = 0.75)
microTrain = subset(micro, split == TRUE)
microTest = subset(micro, split == FALSE)

# Train
microModel <- glm(acs_within_fcc ~., data = microTrain, family = binomial(link = "logit"))

# Test
microModelPreds <- predict(microModel, microTest, type = "response")
microModelPreds <- ifelse(microModelPreds > 0.5, 1, 0)

table(microModelPreds, microTest$acs_within_fcc)

accuracy <- table(microModelPreds, microTest$acs_within_fcc)
sum(diag(accuracy))/sum(accuracy)

# Logistic regression on entire dataset
microModel <- glm(acs_within_fcc ~., data = micro, family = binomial(link = "logit"))
summary(microModel)
stargazer(microModel, type = "text", apply.coef = exp, single.row = TRUE, no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))


#
# Metropolitan --------------------------------------------------------------------------------------------------------------------------------
#

metro <- data %>% filter(urbanicity == "Metropolitan") %>% select(-urbanicity)

# Split data
split <- sample.split(metro$acs_within_fcc, SplitRatio = 0.75)
metroTrain = subset(metro, split == TRUE)
metroTest = subset(metro, split == FALSE)

# Train
metroModel <- glm(acs_within_fcc ~., data = metroTrain, family = binomial(link = "logit"))

# Test
metroModelPreds <- predict(metroModel, metroTest, type = "response")
metroModelPreds <- ifelse(metroModelPreds > 0.5, 1, 0)

table(metroModelPreds, metroTest$acs_within_fcc)

accuracy <- table(metroModelPreds, metroTest$acs_within_fcc)
sum(diag(accuracy))/sum(accuracy)

# Logistic regression on entire dataset
metroModel <- glm(acs_within_fcc ~., data = metro, family = binomial(link = "logit"))
summary(metroModel)
stargazer(metroModel, type = "text", apply.coef = exp, single.row = TRUE, no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))


#
# Compare --------------------------------------------------------------------------------------------------------------------------------
#

stargazer(ruralModel, smallModel, microModel, metroModel, type = "text", apply.coef = exp, single.row = TRUE, no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))
