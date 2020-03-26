library(tidycensus)
library(acs)
library(dplyr)
library(readr)
library(readxl)
library(sf)
library(ggplot2)
library(ggthemes)
library(scales)
library(naniar)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key


#
# ACS: Proportion of households with broadband -------------------------------------------------------------------------------------------------------------
#

# B28002 - PRESENCE AND TYPES OF INTERNET SUBSCRIPTIONS IN HOUSEHOLD, Universe: Households
# 001: Number of households
# 007: Broadband such as cable, fiber optic or DSL

# State FIPS
state_fips <- unique(fips_codes$state)[1:51]

# ACS variables
acsvars <- c("B28002_001", "B28002_007")

# Get tract-level variables from ACS 2014-2018 (5-year)

# https://data.census.gov/cedsci/table?q=internet&g=&table=B28002&tid=ACSDT5Y2018.B28002&lastDisplayedRow=12&vintage=2018
# Total:
#   With an Internet subscription
#     Dial-up with no other type of Internet subscription
#     Broadband of any type
#   Cellular data plan
#     Cellular data plan with no other type of Internet subscription
#   Broadband such as cable, fiber optic or DSL
#     Broadband such as cable, fiber optic or DSL with no other type of Internet subscription
#   Satellite Internet service
#     Satellite Internet service with no other type of Internet subscription
#   Other service with no other type of Internet subscription
# Internet access without a subscription
# No Internet access

acs <- get_acs(geography = "tract", state = state_fips[1], variables = acsvars, year = 2018, survey = "acs5", cache_table = TRUE, output = "wide", geometry = TRUE,
               keep_geo_vars = TRUE)
for(i in 2:length(state_fips)){
  tmp <- get_acs(geography = "tract", state = state_fips[i], variables = acsvars, year = 2018, survey = "acs5", cache_table = TRUE, output = "wide", geometry = TRUE,
                 keep_geo_vars = TRUE)
  acs <- rbind(acs, tmp)
}

# write_rds(acs, "./rivanna_data/working/acs2014-18_bband_raw.Rds")

# Calculate variable min & max (ACS defaults to 90% confidence interval)
# For alternative CIs, see https://www.census.gov/content/dam/Census/programs-surveys/acs/guidance/training-presentations/20180418_MOE.pdf

# NOTE: There are 828 tracts with estimated 0 households (MOE ~10). In these cases, I use +MOE as the number of households. 
acs <- acs %>% mutate(bband = ifelse(B28002_001E > 0, (B28002_007E / B28002_001E), (B28002_007M / B28002_001M)),
                      bbandmin = ifelse(B28002_001E > 0, ((B28002_007E - B28002_001M) / B28002_001E), 0),
                      bbandmax = ifelse(B28002_001E > 0, ((B28002_007E + B28002_001M) / B28002_001E), (B28002_007M /B28002_001M)))
# Bottom-code where bbandmin<0 since coverage cannot be below 0. 271 cases.
acs <- acs %>% mutate(bbandmin = ifelse(bbandmin < 0, 0, bbandmin))
# Top-code where bbandmax>1 since coverage cannot be above 1. 762 cases.
acs <- acs %>% mutate(bbandmax = ifelse(bbandmax > 1, 1, bbandmax))

# write_rds(acs, "./rivanna_data/working/acs2014-18_calc.Rds")

#
# FCC: Number of subscriptions per 1,000 households -------------------------------------------------------------------------------------------------------------
#

# pcat_all: Residential Fixed High-Speed Connections over 200 kbps in at least one direction per per 1,000 Households
# pcat_10x1: Residential Fixed High-Speed Connections at least 10 Mbps downstream and at least 1 Mbps upstream per 1,000 Households 

# Code  Connections per 1,000 HHs
# 0     Zero
# 1     Zero < x <= 200
# 2     200 < x <=400
# 3     400 < x <=600
# 4     600 < x <=800
# 5     800 < x

# Read in
fcc <- read_csv("./rivanna_data/original/tract_map_dec_2016.csv", col_names = TRUE, cols(tractcode = "c"))

# Recode
fcc <- fcc %>% mutate(conn10min = case_when(pcat_10x1 == 0 ~ 0,
                                            pcat_10x1 == 1 ~ 0,
                                            pcat_10x1 == 2 ~ 200/1000,
                                            pcat_10x1 == 3 ~ 400/1000,
                                            pcat_10x1 == 4 ~ 600/1000,
                                            pcat_10x1 == 5 ~ 800/1000),
                      conn10max = case_when(pcat_10x1 == 0 ~ 0,
                                            pcat_10x1 == 1 ~ 200/1000,
                                            pcat_10x1 == 2 ~ 400/1000,
                                            pcat_10x1 == 3 ~ 600/1000,
                                            pcat_10x1 == 4 ~ 800/1000,
                                            pcat_10x1 == 5 ~ 1),
                      conn200min = case_when(pcat_all == 0 ~ 0,
                                             pcat_all == 1 ~ 0,
                                             pcat_all == 2 ~ 200/1000,
                                             pcat_all == 3 ~ 400/1000,
                                             pcat_all == 4 ~ 600/1000,
                                             pcat_all == 5 ~ 800/1000),
                      conn200max = case_when(pcat_all == 0 ~ 0,
                                             pcat_all == 1 ~ 200/1000,
                                             pcat_all == 2 ~ 400/1000,
                                             pcat_all == 3 ~ 600/1000,
                                             pcat_all == 4 ~ 800/1000,
                                             pcat_all == 5 ~ 1),
                      connmin = conn10min,
                      connmax = conn200max)


#
# Join FCC and ACS -------------------------------------------------------------------------------------------------------------
#

# How many FCC tracts are not in ACS and vice versa?
sum(!is.element(fcc$tractcode, acs$GEOID)) # 1053 FCC tracts do not have ACS information.
sum(!is.element(acs$GEOID, fcc$tractcode)) # 342 ACS tracts do not have FCC information.
# 1053 + 342 = 1395 tracts are without both estimates.

# How many tracts total in each dataset?
nrow(acs) # 73056 tracts (of which 342 not in FCC)
nrow(fcc) # 73767 tracts (of which 1053 not in ACS)

# Join (full, see above)
data <- full_join(acs, fcc, by = c("GEOID" = "tractcode"))
anyDuplicated(data$GEOID)

head(data)

# Look at missings
gg_miss_var(acs)
gg_miss_var(fcc)
gg_miss_var(data)

sum(is.na(data$bband))
sum(is.na(data$conn10min))
sum(is.na(data$conn10max))
sum(is.na(data$conn200min))
sum(is.na(data$conn200max))

# Filter to ACS+FCC information available
data <- data %>% filter(!is.na(bband) & !is.na(connmin))


#
# Add RUCA codes -------------------------------------------------------------------------------------------------------------
#

# Documentation: https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/documentation/
# 1	Metropolitan area core: primary flow within an urbanized area (UA)
# 2	Metropolitan area high commuting: primary flow 30% or more to a UA
# 3	Metropolitan area low commuting: primary flow 10% to 30% to a UA
# 4	Micropolitan area core: primary flow within an urban cluster of 10,000 to 49,999 (large UC)
# 5	Micropolitan high commuting: primary flow 30% or more to a large UC
# 6	Micropolitan low commuting: primary flow 10% to 30% to a large UC
# 7	Small town core: primary flow within an urban cluster of 2,500 to 9,999 (small UC)
# 8	Small town high commuting: primary flow 30% or more to a small UC
# 9	Small town low commuting: primary flow 10% to 30% to a small UC
# 10 Rural areas: primary flow to a tract outside a UA or UC
# 99 Not coded: Census tract has zero population and no rural-urban identifier information

# Read in, skip row #1 because it is a note
ruca <- read_excel("./rivanna_data/original/ruca2010revised.xlsx", col_names = TRUE, progress = readxl_progress(), skip = 1)

names(ruca)[1] <- "StateCounty"
names(ruca)[2] <- "State"
names(ruca)[3] <- "County"
names(ruca)[4] <- "Tract"
names(ruca)[5] <- "primRUCA"
names(ruca)[6] <- "secRUCA"
names(ruca)[7] <- "TractPop10"
names(ruca)[8] <- "LandSqmile10"
names(ruca)[9] <- "PopDens10"

# Number of tracts in RUCA
nrow(ruca) # 74002 tracts

# How many data tracts are not in RUCA and vice versa?
sum(!is.element(data$GEOID, ruca$Tract)) # All data tracts are in RUCA.
sum(!is.element(ruca$Tract, data$GEOID)) # 1288 RUCA tracts are not in data (due to previous filtering on bband information)

# Join
data <- left_join(data, ruca, by = c("GEOID" = "Tract"))

# Add urbanicity indicator
data <- data %>% mutate(urbanicity = case_when((primRUCA == 1 | primRUCA == 2 | primRUCA == 3) ~ "Metropolitan",
                                               (primRUCA == 4 | primRUCA == 5 | primRUCA == 6) ~ "Micropolitan",
                                               (primRUCA == 7 | primRUCA == 8 | primRUCA == 9) ~ "Small town",
                                               (primRUCA == 10) ~ "Rural",
                                               (primRUCA == 99 | is.na(primRUCA)) ~ NA_character_))
data$urbanicity <- factor(data$urbanicity, levels = c("Rural", "Small town", "Micropolitan", "Metropolitan"))

# write_rds(data, "./rivanna_data/working/acs_fcc_ruca.Rds")
