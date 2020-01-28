library(IRanges)


#
# Prepare data -------------------------------------------------------------------------------------
#

# Need two-column dataframes with nothing else
test_acs <- data %>% select(bbandmin, bbandmax) %>% 
  st_set_geometry(NULL)

test_fcc10 <- data %>% select(conn10min, conn10max) %>% 
  st_set_geometry(NULL)

test_fcc200 <- data %>% select(conn200min, conn200max) %>% 
  st_set_geometry(NULL)

test_fcc <- data %>% select(connmin, connmax) %>% 
  st_set_geometry(NULL)

# Can only work with whole numbers (multiply to preserve all decimals)
startacs <- test_acs$bbandmin*10000000
endacs <- test_acs$bbandmax*10000000
test_acs <- IRanges(start = startacs, end = endacs)

startfcc10 <- test_fcc10$conn10min*10000000
endfcc10 <- test_fcc10$conn10max*10000000
test_fcc10 <- IRanges(start = startfcc10, end = endfcc10)

startfcc200 <- test_fcc200$conn200min*10000000
endfcc200 <- test_fcc200$conn200max*10000000
test_fcc200 <- IRanges(start = startfcc200, end = endfcc200)

startfcc <- test_fcc$connmin*10000000
endfcc <- test_fcc$connmax*10000000
test_fcc <- IRanges(start = startfcc, end = endfcc)

# Test ("If type is within, the query interval must be wholly contained within the subject interval. Note that all matches must additionally satisfy the minoverlap constraint described above.")
# countOverlaps(query, subject, type = "within")
countOverlaps(test_acs[1], test_fcc[1], type = "within")
countOverlaps(test_acs[2], test_fcc[2], type = "within")
countOverlaps(test_acs[3], test_fcc[3], type = "within")


#
# Get intervals -------------------------------------------------------------------------------------
#

# ACS is completely within FCC 10
# overlap_acswithinfcc10 <- as.data.frame(countOverlaps(test_acs[1], test_fcc10[1]), type = "within")
# for(i in 2:length(test_acs@start)){
#   tmp <- countOverlaps(test_acs[i], test_fcc10[i], type = "within")
#   overlap_acswithinfcc10 <- rbind(overlap_acswithinfcc10, tmp)
# }
# names(overlap_acswithinfcc10)[1] <- "acs_within_fcc10"
# overlap_acswithinfcc10$acs_within_fcc10 <- as.factor(overlap_acswithinfcc10$acs_within_fcc10)
# 
# # ACS is completely within FCC200
# overlap_acswithinfcc200 <- as.data.frame(countOverlaps(test_acs[1], test_fcc200[1]), type = "within")
# for(i in 2:length(test_acs@start)){
#   tmp <- countOverlaps(test_acs[i], test_fcc200[i], type = "within")
#   overlap_acswithinfcc200 <- rbind(overlap_acswithinfcc200, tmp)
# }
# names(overlap_acswithinfcc200)[1] <- "acs_within_fcc200"
# overlap_acswithinfcc200$acs_within_fcc200 <- as.factor(overlap_acswithinfcc200$acs_within_fcc200)

# ACS is completely within FCC10min-FCC200max
overlap_acswithinfcc <- as.data.frame(countOverlaps(test_acs[1], test_fcc[1]), type = "within")
for(i in 2:length(test_acs@start)){
  tmp <- countOverlaps(test_acs[i], test_fcc[i], type = "within")
  overlap_acswithinfcc <- rbind(overlap_acswithinfcc, tmp)
}
names(overlap_acswithinfcc)[1] <- "acs_within_fcc"
overlap_acswithinfcc$acs_within_fcc <- as.factor(overlap_acswithinfcc$acs_within_fcc)


#
# Put back -------------------------------------------------------------------------------------
#

# Create df with intervals
overlap_geo <- as.data.frame(data$GEOID)
names(overlap_geo)[1] <- "GEOID"
overlap_geo$GEOID <- as.character(data$GEOID)

overlap_df <- cbind(overlap_geo, overlap_acswithinfcc)

# Left join with data (that has geography)
data_int <- left_join(data, overlap_df, by = "GEOID")

# Write
# write_rds(acs, "./data/working/data_int.Rds")