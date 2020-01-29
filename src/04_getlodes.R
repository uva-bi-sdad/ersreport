library(lodes)
library(dplyr)
library(stringr)


#
# Get LODES data: All jobs -------------------------------------------------------------------------------------------------------------
#

state_fips <- unique(fips_codes$state)[1:51]

# Get tract-level variables from LODES 2016
# https://lehd.ces.census.gov/data/lodes/LODES7/LODESTechDoc7.2.pdf

# Get variables: All jobs
# S000 = all workforce segments
# JT00 = all jobs (federal and private)
lodes_jobs <- read_lodes(state = state_fips[1], type = "workplace", segment = "S000", job_type = "JT00", year = "2016", download_dir = "./data/original/lodes/")
for(i in 2:length(state_fips)){
  tmp <- read_lodes(state = state_fips[i], type = "workplace", segment = "S000", job_type = "JT00", year = "2016", download_dir = "./data/original/lodes/")
  lodes_jobs <- rbind(lodes_jobs, tmp)
}

# Get variables: All private jobs (for firm information)
# S000 = all workforce segments
# JT02 = private jobs only
lodes_firms <- read_lodes(state = state_fips[1], type = "workplace", segment = "S000", job_type = "JT02", year = "2016", download_dir = "./data/original/lodes/")
for(i in 2:length(state_fips)){
  tmp <- read_lodes(state = state_fips[i], type = "workplace", segment = "S000", job_type = "JT02", year = "2016", download_dir = "./data/original/lodes/")
  lodes_firms <- rbind(lodes_firms, tmp)
}

# Get crosswalk
lodes_xwalk <- read_xwalk(state = state_fips[1], download_dir = "./data/original/lodes/")
for(i in 2:length(state_fips)){
  tmp <- read_xwalk(state = state_fips[i], download_dir = "./data/original/lodes/")
  lodes_xwalk <- rbind(lodes_xwalk, tmp)
}

# Write
# write_rds(lodes_jobs, "./data/working/lodes2016_raw_jobs.Rds")
# write_rds(lodes_firms, "./data/working/lodes2016_raw_firms.Rds")
# write_rds(lodes_xwalk, "./data/working/lodes2016_raw_xwalk.Rds")


#
# Fix crosswalk -------------------------------------------------------------------------------------------------------------
#

lodes_xwalk <- lodes_xwalk %>% select(tabblk2010, st, stusps, stname, cty, ctyname, trct, trctname, bgrp, bgrpname)
str(lodes_xwalk)

lodes_xwalk$tabblk2010 <- as.character(lodes_xwalk$tabblk2010)
lodes_xwalk$st <- as.character(lodes_xwalk$st)
lodes_xwalk$cty <- as.character(lodes_xwalk$cty)
lodes_xwalk$trct <- as.character(lodes_xwalk$trct)
lodes_xwalk$bgrp <- as.character(lodes_xwalk$bgrp)

lodes_xwalk$tabblk2010 <- ifelse(nchar(lodes_xwalk$tabblk2010) == 14, paste0("0", lodes_xwalk$tabblk2010), lodes_xwalk$tabblk2010)
lodes_xwalk$st <- ifelse(nchar(lodes_xwalk$st) == 1, paste0("0", lodes_xwalk$st), lodes_xwalk$st)
lodes_xwalk$cty <- ifelse(nchar(lodes_xwalk$cty) == 4, paste0("0", lodes_xwalk$cty), lodes_xwalk$cty)
lodes_xwalk$trct <- ifelse(nchar(lodes_xwalk$trct) == 10, paste0("0", lodes_xwalk$trct), lodes_xwalk$trct)
lodes_xwalk$bgrp <- ifelse(nchar(lodes_xwalk$bgrp) == 11, paste0("0", lodes_xwalk$bgrp), lodes_xwalk$bgrp)

any(nchar(lodes_xwalk$tabblk2010) < 15)
any(nchar(lodes_xwalk$st) < 2)
any(nchar(lodes_xwalk$cty) < 5)
any(nchar(lodes_xwalk$trct) < 11)
any(nchar(lodes_xwalk$bgrp) < 12)

# Write
# write_rds(lodes_xwalk, "./data/working/lodes2016_calc_xwalk.Rds")


#
# Aggregate to tract: All jobs -------------------------------------------------------------------------------------------------------------
#

lodes_jobs <- left_join(lodes_jobs, lodes_xwalk, by = c("w_geocode" = "tabblk2010"))

# Aggregate to tract
lodes_jobs_tract <- lodes_jobs %>% arrange(trct) %>%
  group_by(trct) %>% mutate(job_all = sum(C000, na.rm = TRUE),
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
lodes_jobs_tract <- lodes_jobs_tract %>% select(w_geocode, st, stusps, stname, cty, ctyname, trct, trctname, starts_with("job_"))

# Get one row per tract
lodes_jobs_tract <- lodes_jobs_tract %>% group_by(trct) %>% slice(1) %>% ungroup()
lodes_jobs_tract <- lodes_jobs_tract %>% select(-w_geocode)

# Write
# write_rds(lodes_jobs_tract, "./data/working/lodes2016_calc_jobs.Rds")


#
# Aggregate to tract: Firms -------------------------------------------------------------------------------------------------------------
#

lodes_firms <- left_join(lodes_firms, lodes_xwalk, by = c("w_geocode" = "tabblk2010"))

# Aggregate to tract
lodes_firms_tract <- lodes_firms %>% arrange(trct) %>%
  group_by(trct) %>% mutate(job_all_private = sum(C000, na.rm = TRUE),
                             firmage_01 = sum(CFA01, na.rm = TRUE)/job_all_private,
                             firmage_23 = sum(CFA02, na.rm = TRUE)/job_all_private,
                             firmage_45 = sum(CFA03, na.rm = TRUE)/job_all_private,
                             firmage_610 = sum(CFA04, na.rm = TRUE)/job_all_private,
                             firmage_11ovr = sum(CFA05, na.rm = TRUE)/job_all_private,
                             firmsize_019 = sum(CFS01, na.rm = TRUE)/job_all_private,
                             firmsize_2049 = sum(CFS02, na.rm = TRUE)/job_all_private,
                             firmsize_50249 = sum(CFS03, na.rm = TRUE)/job_all_private,
                             firmsize_250499 = sum(CFS04, na.rm = TRUE)/job_all_private,
                             firmsize_500ovr = sum(CFS05, na.rm = TRUE)/job_all_private) %>%
  ungroup()

# Select relevant columns 
lodes_firms_tract <- lodes_firms_tract %>% select(w_geocode, st, stusps, stname, cty, ctyname, trct, trctname, starts_with("job_"), starts_with("firm"))

# Get one row per tract
lodes_firms_tract <- lodes_firms_tract %>% group_by(trct) %>% slice(1) %>% ungroup()
lodes_firms_tract <- lodes_firms_tract %>% select(-w_geocode)

# Write
# write_rds(lodes_firms_tract, "./data/working/lodes2016_calc_firms.Rds")


#
# Join to interval data -------------------------------------------------------------------------------------------------------------
#

data_int <- left_join(data_int, lodes_jobs_tract, by = c("GEOID" = "trct"))
data_int <- left_join(data_int, lodes_firms_tract, by = c("GEOID" = "trct", "st", "stusps", "stname", "cty", "ctyname", "trctname"))

# Write
# write_rds(data_int, "./data/working/data_int_covars_acs_lodes.Rds")
