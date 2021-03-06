library(tidycensus)
library(acs)
library(rgeos)
library(dplyr)
library(sf)


census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key


#
# Get variable data for geography & transform -------------------------------------------------------------------------------------------------------------
#

# allvars <- load_variables(2018, "acs5", cache = FALSE)

# State FIPS
state_fips <- unique(fips_codes$state)[1:51]

# ACS variables
acsvars <- c("B01003_001",                                                                                                         # total population
             "B15003_001","B15003_002","B15003_003","B15003_004","B15003_005","B15003_006","B15003_007","B15003_008","B15003_009", # educational attainment for 25+, sum(cat2-cat16)/total -- less than HS
             "B15003_010","B15003_011","B15003_012","B15003_013","B15003_014","B15003_015","B15003_016",                           # educational attainment for 25+, sum(cat2-cat16)/total -- less than HS
             "B17020_001","B17020_002",                                                                                            # income under poverty level, cat2/total
             "B01001_001","B01001_020","B01001_021","B01001_022","B01001_023","B01001_024","B01001_025",                           # 65+ population, male65+ plus female65+ / total
             "B01001_044","B01001_045","B01001_046","B01001_047","B01001_048","B01001_049",                                        # 65+ population, male65+ plus female65+ / total
             "B03003_001","B03003_003",                                                                                            # hispanic, hispanic/total
             "B02001_001","B02001_003",                                                                                            # black, black/total
             "B09019_002","B09019_003",                                                                                            # family households, in family households/in households
             "B05002_001","B05002_013",                                                                                            # foreign born, foreign/total
             "B08006_001","B08006_017",                                                                                            # works from home, workers from home / total -- 017 / 001
             "B08303_001","B08303_008","B08303_009","B08303_010","B08303_011","B08303_012","B08303_013",                           # travel time to work, 30min+ / total (008 to 013 / 001)
             "B19058_001","B19058_002",                                                                                            # households with public assistance or food stamps/snap, with support / total (002 / 001)
             "B23025_002","B23025_005",                                                                                            # proportion unemployed of labor force / total (005 / 002)
             "B25002_001","B25002_003",                                                                                            # proportion vacant housing units, vacant / total -- 003 / 001
             "B25003_001","B25003_003",                                                                                            # proportion renters, renters  /total -- 003 / 001
             "B25035_001",                                                                                                         # median year structure built
             "B25071_001",                                                                                                         # median gross rent as percentage of HH income
             "B28011_001", "B28011_008")                                                                                           # no internet access, no access / total (008 / 001)


# Get tract-level variables from ACS 2014-2018 (5-year)
acs_est <- get_acs(geography = "tract", state = state_fips[1], variables = acsvars, year = 2018, survey = "acs5", cache_table = TRUE, output = "wide", geometry = TRUE,
                   keep_geo_vars = TRUE)
for(i in 2:length(state_fips)){
  tmp <- get_acs(geography = "tract", state = state_fips[i], variables = acsvars, year = 2018, survey = "acs5", cache_table = TRUE, output = "wide", geometry = TRUE,
                 keep_geo_vars = TRUE)
  acs_est <- rbind(acs_est, tmp)
}

# write_rds(acs_est, "./data/working/acs2014-18_other_raw.Rds")

# Calculate variables: rates / % population (ALAND and AWATER are in square meters)
acs_estimates <- acs_est %>% transmute(
  GEOID = GEOID,
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  AFFGEOID = AFFGEOID,
  AWATER = AWATER,
  LSAD = LSAD,
  NAME.x = NAME.x,
  NAME.y = NAME.y,
  population = B01003_001E,
  area = ALAND,
  less_hs = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E + B15003_010E +
                  B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E + B15003_016E) / B15003_001E,
  poverty = B17020_002E / B17020_001E,
  age_65_older = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
                    B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E,
  hispanic = B03003_003E / B03003_001E,
  black = B02001_003E / B02001_001E,
  family = B09019_003E / B09019_002E,
  foreign = B05002_013E / B05002_001E,
  workfromhome = B08006_017E / B08006_001E,     
  longcommute = (B08303_008E + B08303_009E + B08303_010E + B08303_011E + B08303_012E + B08303_013E) / B08303_001E,
  assistance = B19058_002E / B19058_001E,
  unemploy = B23025_005E / B23025_002E,  
  vacant = B25002_003E / B25002_001E,
  renters = B25003_003E / B25003_001E,  
  yearbuilt = B25035_001E,
  rentburden = B25071_001E,
  nointernet = B28011_008E / B28011_001E,
  geometry = geometry
)

# write_rds(acs_estimates, "./data/working/acs2014-18_other_calc.Rds")


#
# Join to data -------------------------------------------------------------------------------------------------------------
#

acs <- acs_estimates %>% st_set_geometry(NULL)
data_int <- left_join(data_int, acs, by = c("STATEFP", "COUNTYFP", "AFFGEOID", "GEOID", "NAME.x", "LSAD", "AWATER", "NAME.y"))

# write_rds(data_int, "./data/working/data_int_covars_acs.Rds")
