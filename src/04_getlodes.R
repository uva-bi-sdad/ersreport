library(lodes)
library(dplyr)
library(stringr)


#
# Get LODES data -------------------------------------------------------------------------------------------------------------
#

state_fips <- unique(fips_codes$state)[1:51]

# Get tract-level variables from LODES 2016
# S000 = all workforce segments
# JT00 = all jobs (federal and private)
# https://lehd.ces.census.gov/data/lodes/LODES7/LODESTechDoc7.2.pdf

# Get variables
lodes_est <- read_lodes(state = state_fips[1], type = "workplace", segment = "S000", job_type = "JT00", year = "2016", download_dir = "./data/original/lodes/")
for(i in 2:length(state_fips)){
  tmp <- read_lodes(state = state_fips[i], type = "workplace", segment = "S000", job_type = "JT00", year = "2016", download_dir = "./data/original/lodes/")
  lodes_est <- rbind(lodes_est, tmp)
}

# Get crosswalk
lodes_xwalk <- read_xwalk(state = state_fips[1], download_dir = "./data/original/lodes/")
for(i in 2:length(state_fips)){
  tmp <- read_xwalk(state = state_fips[i], download_dir = "./data/original/lodes/")
  lodes_xwalk <- rbind(lodes_xwalk, tmp)
}

# Write
# write_rds(lodes_est, "./data/working/lodes2016_raw.Rds")
# write_rds(lodes_xwalk, "./data/working/lodes2016_raw_xwalk.Rds")


#
# Aggregate to tract -------------------------------------------------------------------------------------------------------------
#

# Create tract GEOID for join
lodes_est <- lodes_est %>% mutate(GEOID = substr(w_geocode, start = 1, stop = 11))

# Aggregate to tract
lodes_est_tract <- lodes_est %>% arrange(GEOID) %>%
  group_by(GEOID) %>% mutate(job_all = sum(C000, na.rm = TRUE),
                            job_age29less = sum(CA01, na.rm = TRUE)/job_all,
                            job_age3054 = sum(CA02, na.rm = TRUE)/job_all,
                            job_age55ovr = sum(CA03, na.rm = TRUE)/job_all,
                            job_earn1250less = sum(CE01, na.rm = TRUE)/job_all,
                            job_earn12513333 = sum(CE02, na.rm = TRUE)/job_all,
                            job_earn3334ovr = sum(CE03, na.rm = TRUE)/job_all,
                            job_white = sum(CR01, na.rm = TRUE)/job_all,
                            job_black = sum(CR02, na.rm = TRUE)/job_all,
                            job_native = sum(CR03, na.rm = TRUE)/job_all,
                            job_asian = sum(CR04, na.rm = TRUE)/job_all,
                            job_pacific = sum(CR05, na.rm = TRUE)/job_all,
                            job_multirace = sum(CR07, na.rm = TRUE)/job_all,
                            job_nonhisp = sum(CT01, na.rm = TRUE)/job_all,
                            job_hisp = sum(CT02, na.rm = TRUE)/job_all,
                            job_educlesshs = sum(CD01, na.rm = TRUE)/job_all,
                            job_educhsequiv = sum(CD02, na.rm = TRUE)/job_all,
                            job_educsomecol = sum(CD03, na.rm = TRUE)/job_all,
                            job_educba = sum(CD04, na.rm = TRUE)/job_all,
                            job_male = sum(CS01, na.rm = TRUE)/job_all,
                            job_female = sum(CS02, na.rm = TRUE)/job_all) %>%
  ungroup()

# Select relevant columns 
lodes_est_tract <- lodes_est_tract %>% select(w_geocode, GEOID, starts_with("job_"))

# Get one row per tract
lodes_est_tract <- lodes_est_tract %>% group_by(GEOID) %>% slice(1) %>% ungroup()
lodes_est_tract <- lodes_est_tract %>% select(-w_geocode)

# Write
# write_rds(lodes_est_tract, "./data/working/lodes2016_tract_calc.Rds")


#
# Join to interval data -------------------------------------------------------------------------------------------------------------
#

data_int <- left_join(data_int, lodes_est_tract, by = "GEOID")

# Write
# write_rds(data_int, "./data/working/data_int_covars_acs_lodes.Rds")
