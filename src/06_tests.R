#
# Look at interval widths --------------------------------------------
#

test <- data %>% filter(urbanicity == "Rural") %>%
  select(connmin, connmax, bbandmin, bbandmax) %>%
  mutate(distfcc = connmax - connmin,
         distacs = bbandmax - bbandmin)

hist(test$distfcc)
hist(test$distacs)

mean(data[data$urbanicity == "Rural", ]$connmax, na.rm = TRUE) # 0.71
mean(data[data$urbanicity == "Rural", ]$connmin, na.rm = TRUE) # 0.21

mean(data[data$urbanicity == "Small town", ]$connmax, na.rm = TRUE) # 0.75
mean(data[data$urbanicity == "Small town", ]$connmin, na.rm = TRUE) # 0.28

mean(data[data$urbanicity == "Micropolitan", ]$connmax, na.rm = TRUE) # 0.79
mean(data[data$urbanicity == "Micropolitan", ]$connmin, na.rm = TRUE) # 0.38

mean(data[data$urbanicity == "Metropolitan", ]$connmax, na.rm = TRUE) # 0.88
mean(data[data$urbanicity == "Metropolitan", ]$connmin, na.rm = TRUE) # 0.57


#
# Try some models, vary thresholds and urbanicity --------------------------------------------
#

test <- data %>% filter(urbanicity != "Metropolitan") %>%
                 select(acs_within_fcc, PopDens10, bbandmax,
                        less_hs, poverty, age_65_older, hispanic, black, family, foreign, workfromhome, longcommute, assistance, unemploy, vacant, renters, yearbuilt, rentburden, nointernet,
                        job_age3054, job_age55ovr, job_earn12513333, job_earn3334ovr, job_white, job_black, job_asian, job_hisp, 
                        job_educhsequiv, job_educsomecol, job_educba, job_female, firmage_23,  firmage_45, firmage_610, firmage_11ovr, 
                        firmsize_2049, firmsize_50249, firmsize_250499, firmsize_500ovr)
test <- na.omit(test)

under <- test %>% filter(bbandmax < 0.5) %>% select(-bbandmax)
over <- test %>% filter(bbandmax == 0.7 | bbandmax > 0.7) %>% select(-bbandmax)


#
# Under ---------------------------------------------------------------------------
#

# Split data
split <- sample.split(under$acs_within_fcc, SplitRatio = 0.75)
underTrain = subset(under, split == TRUE)
underTest = subset(under, split == FALSE)

# Train
underModel <- glm(acs_within_fcc ~., data = underTrain, family = binomial(link = "logit"))

# Test
underModelPreds <- predict(underModel, underTest, type = "response")
underModelPreds <- ifelse(underModelPreds > 0.5, 1, 0)

table(underModelPreds, underTest$acs_within_fcc)

accuracy <- table(underModelPreds, underTest$acs_within_fcc)
sum(diag(accuracy))/sum(accuracy)

# Logistic regression on entire dataset
underModel <- glm(acs_within_fcc ~., data = under, family = binomial(link = "logit"))
summary(underModel)
stargazer(underModel, type = "text", apply.coef = exp, single.row = TRUE, no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))


#
# Over ---------------------------------------------------------------------------
#

# Split data
split <- sample.split(over$acs_within_fcc, SplitRatio = 0.75)
overTrain = subset(over, split == TRUE)
overTest = subset(over, split == FALSE)

# Train
overModel <- glm(acs_within_fcc ~., data = overTrain, family = binomial(link = "logit"))

# Test
overModelPreds <- predict(overModel, overTest, type = "response")
overModelPreds <- ifelse(overModelPreds > 0.5, 1, 0)

table(overModelPreds, overTest$acs_within_fcc)

accuracy <- table(overModelPreds, overTest$acs_within_fcc)
sum(diag(accuracy))/sum(accuracy)

# Logistic regression on entire dataset
overModel <- glm(acs_within_fcc ~., data = over, family = binomial(link = "logit"))
summary(overModel)
stargazer(overModel, type = "text", apply.coef = exp, single.row = TRUE, no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))

# Both
stargazer(underModel, overModel, type = "text", apply.coef = exp, single.row = TRUE, no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))
