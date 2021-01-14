# Cleaning and merging county 1-year data

suppressPackageStartupMessages({
  library(tidyverse)
  library(visdat)
})

# Load in data
acs_1_county <- read_csv("data/county_acs_1yr_2005_2018_long.csv")
acs_5_county <- read_csv("data/county_acs_5yr_2009_2018_long.csv")
cens_2010_county <- read_csv("data/census/county_census_2010.csv")
ig_county <- read_csv("data/infogroup_counts/county_2006_2019.csv")
ig_county_rows <- nrow(ig_county)

# Clean up ACS
# ------------

# 1 year 
# ------
# fix geoid
length(unique(acs_1_county$countyid))
acs_1_county <- acs_1_county %>%
  mutate(geoid = as.character(geoid)) %>%
  mutate(geoid = ifelse(
    nchar(geoid) == 4, 
    paste0("0", geoid),
    geoid
  )) 

# Check that no counts in the ACS are missing
n_missings <- acs_1_county %>%
  group_by(countyid) %>%
  summarise(n_NAs = sum(is.na(chh_))) %>%
  arrange(desc(n_NAs))
# They are
# Check the distribution by year
n_missing_yearly <- acs_1_county %>%
  group_by(year) %>%
  summarise(n_NAs = sum(is.na(chh_))) %>%
  arrange(desc(n_NAs))
n_missing_yearly

# Filter down to only large counties with full time-series
to_remove <- acs_1_county %>%
  group_by(countyid) %>%
  summarise(n_NAs = sum(is.na(chh_))) %>%
  filter(n_NAs > 0) %>%
  .$countyid
acs_1_county <- acs_1_county %>%
  filter(!(countyid %in% to_remove))
length(unique(acs_1_county$countyid))

# 5 year
# ------
# Check that no counts in the ACS are missing
acs_5_county %>%
  group_by(countyid) %>%
  summarise(n_NAs = sum(is.na(chh_))) %>%
  arrange(desc(n_NAs))
# Some are
# Check the distribution by year
n_missing_yearly <- acs_5_county %>%
  group_by(year) %>%
  summarise(n_NAs = sum(is.na(chh_))) %>%
  arrange(desc(n_NAs))
n_missing_yearly

# Drop Puerto Rico
acs_5_county <- acs_5_county %>%
  filter(stateid != "72")
acs_1_county <- acs_1_county %>%
  filter(stateid != "72")
length(unique(acs_5_county$countyid))

# Clean up Infogroup data
# -----------------------
# Filter down missing id variables
ig_county <- ig_county %>%
  filter(!grepl("[A-Za-z]", countyid)) # drop the cols with state names
ig_county_rows - nrow(ig_county)

# Select just columns with household counts for IG
ig_county <- ig_county %>%
  select(countyid, starts_with("hh_20")) 
vis_miss(ig_county)

# Pivot to county-year units
# --------------------------
ig_county_counts <- ig_county %>%
  pivot_longer(
    -countyid, 
    names_to = c("measure", "year"),
    names_sep = "_",
    values_to = "ig_count"
  ) %>%
  select(-measure) %>%
  mutate(year = as.numeric(year))

# check for missings by year
ig_county_counts$missing <- ifelse(is.na(ig_county_counts$ig_count), TRUE, FALSE)
table(ig_county_counts$year, ig_county_counts$missing)

# check max missings for a county
missings <- ig_county_counts %>%
  group_by(countyid) %>%
  summarise(n_NAs = sum(is.na(ig_count))) %>%
  arrange(desc(n_NAs))
missings
# max is 1, fine, move on

# now wherever possible impute missing ig count 
# as average of current and next year
sum(is.na(ig_county_counts$ig_count))
ig_county_counts <- ig_county_counts %>%
  group_by(countyid) %>%
  mutate(lead = lead(ig_count),
         lag = lag(ig_count)) %>%
  ungroup()

ig_county_counts <- ig_county_counts %>%
  group_by(year, countyid) %>%
  mutate(
    ig_count_imptd = ifelse(
      is.na(ig_count),
      mean(c(lead, lag), na.rm = TRUE),
      ig_count
    )
  ) %>%
  ungroup()

sum(is.na(ig_county_counts$ig_count_imptd))

# One county has zero people. 
# Yakutat City and Borough, Alaska
# Drop it.
ig_county_counts <-ig_county_counts %>% 
  filter(ig_count_imptd != 0)
length(unique(ig_county_counts$countyid))

# merge ig to the decennial and save out
# --------------------------------------
decennial <- ig_county_counts %>% 
  filter(year == 2010) %>%
  inner_join(cens_2010_county)

# check merge 
length(unique(decennial$countyid))
length(unique(ig_county_counts$countyid))
length(unique(cens_2010_county$countyid))

# check which are in dec but not ig
cens_2010_county %>%
  filter(!(countyid %in% ig_county_counts$countyid)) %>%
  .$countyid
# all puerto rico

# fix names so they align with the ACS files
# remove "census_2010 from variable names
names(decennial) <- gsub("census_2010", "", names(decennial))
# manual renames
decennial <- decennial %>%
  rename("chh_density" = "chh_density_")

saveRDS(decennial, "data/prepped_data/counties_decennial.rds")

# merge ig to acs 1 and 5 year
# ----------------------------

# 1 year
large_counties_1 <- inner_join(ig_county_counts, acs_1_county)
length(unique(large_counties_1$countyid))

vis_miss(large_counties_1)

# remove mostly missing columns
large_counties_1 <- large_counties_1[, which(colMeans(!is.na(large_counties_1)) > 0.8)]

saveRDS(large_counties_1, "data/prepped_data/counties_1_acs.rds")

# 5 year
counties_all_5 <- acs_5_county %>%
  inner_join(ig_county_counts) %>%
  arrange(countyid, year)
length(unique(counties_all_5$countyid))

# visualize data missingness
counties_all_5 %>%
  sample_n(10000) %>%
  vis_miss()

# Remove near-empty columns
counties_all_5 <- counties_all_5 %>%
  select(
    -pmoved_withincnty_, 
    -pmoved_difcnty_samestate_, 
    -pmoved_diffstate_,
    -pmoved_fromabroad_,
    -pmoved_,
    -punemployed_,
    -ppov_,
    -ppost2000_
  )

counties_all_5 %>%
  sample_n(10000) %>%
  vis_miss()

saveRDS(counties_all_5, "data/prepped_data/counties_5_acs.rds")

