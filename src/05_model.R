library(readr)
library(dplyr)
library(ggplot2)
library(caTools)
library(stargazer)


# Set seed
set.seed(2410) 

# US tracts with ACS-FCC broadband subscription congruence indicator (ACS 2014-18; FCC 2016), ACS covariates (2014-16), and LODES (2016) covariates.
data <- read_rds("./data/working/data_int_covars_acs_lodes.Rds")
data <- data %>% select(acs_within_fcc, urbanicity, population, area,
                        less_hs, poverty, age_65_older, hispanic, black, family, foreign, workfromhome, longcommute, assistance, unemploy, vacant, renters, yearbuilt, rentburden, nointernet,
                        job_all, job_age3054, job_age55ovr, job_earn12513333, job_earn3334ovr, job_white, job_black, job_asian, job_hisp, 
                        job_educhsequiv, job_educsomecol, job_educba, job_female, job_all_private, firmage_23,  firmage_45, firmage_610, firmage_11ovr, 
                        firmsize_2049, firmsize_50249, firmsize_250499, firmsize_500ovr)
data <- na.omit(data)

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


mymodel <- glm(acs_within_fcc ~., data = data, family = binomial(link = "logit"))
summary(mymodel)
stargazer(mymodel, type = "text", apply.coef = exp, single.row = TRUE, no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))