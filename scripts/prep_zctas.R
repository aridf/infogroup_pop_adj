# Cleaning and merging ZCTA data
suppressPackageStartupMessages({
  library(tidyverse)
  library(visdat)
})

# Load in ZCTA data
cens_2010_zcta <- read_csv("data/census/zcta_census_2010.csv")
acs_5_zcta <- read_csv("data/zcta_acs_2011_2018_long.csv")
ig_zcta <- read_csv("data/infogroup_counts/zcta_2006_2019.csv")
ig_zcta_rows <- nrow(ig_zcta)

# Check that no counts in the ACS are missing
acs_5_zcta %>%
  group_by(zcta_id) %>%
  summarise(n_NAs = sum(is.na(chh_))) %>%
  arrange(desc(n_NAs))
# Shows no missing counts in the ACS data

# Check that no counts in the ACS are zero
acs_5_zcta %>%
  group_by(zcta_id) %>%
  summarise(n_0s = sum(chh_ == 0)) %>%
  arrange(desc(n_0s))
# There are. Remove these
acs_5_zcta <- acs_5_zcta %>%
  filter(chh_ != 0)


# Get number of unique ZCTAs in acs data
length(unique(acs_5_zcta$zcta_id))
  
# Check that no counts in the decennial are missing
cens_2010_zcta %>%
  group_by(zcta_id) %>%
  summarise(n_NAs = sum(is.na(chh_census_2010))) %>%
  arrange(desc(n_NAs))

# Check that no counts in the decennial are zero
cens_2010_zcta %>%
  group_by(zcta_id) %>%
  summarize(n_0s = sum(chh_census_2010 == 0)) %>%
  arrange(desc(n_0s))
# remove zeros
cens_2010_zcta <- cens_2010_zcta %>%
  filter(chh_census_2010 != 0)

# Get number of unique ZCTAs in decennial data
length(unique(cens_2010_zcta$zcta_id))

# Clean up IG data
# ----------------
# Filter missing zctas
ig_zcta <- ig_zcta %>%
  filter(!is.na(zcta))
ig_zcta_rows - nrow(ig_zcta)

# Filter down to ZCTAs that have matches in ACS
ig_zcta <- ig_zcta %>%
  filter(zcta %in% acs_5_zcta$zcta_id)
length(unique(ig_zcta$zcta))

# Filter down to one row per zcta
ig_zcta <- ig_zcta %>%
  select(zcta, starts_with("hh_20")) %>%
  distinct() # Fix ZCTAs were duplicated if they mapped to multiple zips

dupes <- ig_zcta %>%
  group_by(zcta) %>%
  count() %>%
  filter(n > 1) %>%
  .$zcta
  
ig_zcta <- ig_zcta %>%
  filter(!zcta %in% dupes)

# Pivot to zcta-year units
# ------------------------
ig_zcta_counts <- ig_zcta %>%
  pivot_longer(
    -zcta, 
    names_to = c("measure", "year"),
    names_sep = "_",
    values_to = "ig_count"
  ) %>%
  select(-measure) %>%
  rename("zcta_id" = "zcta")

# check for missings by year
ig_zcta_counts$missing <- ifelse(is.na(ig_zcta_counts$ig_count), TRUE, FALSE)
table(ig_zcta_counts$year, ig_zcta_counts$missing)

# There are some IG zctas with no counts. Remove those with 
missings <- ig_zcta_counts %>%
  group_by(zcta_id) %>%
  summarise(n_NAs = sum(is.na(ig_count))) %>%
  arrange(desc(n_NAs))

to_remove <- missings %>%
  filter(n_NAs > 1) %>%
  .$zcta_id

ig_zcta_counts <- ig_zcta_counts %>%
  filter(!(zcta_id %in% to_remove))

# now wherever possible impute missing ig count 
# as average of current and next year
sum(is.na(ig_zcta_counts$ig_count))
ig_zcta_counts <- ig_zcta_counts %>%
  group_by(zcta_id) %>%
  mutate(lead = lead(ig_count),
         lag = lag(ig_count)) %>%
  ungroup()

ig_zcta_counts <- ig_zcta_counts %>%
  group_by(year, zcta_id) %>%
  mutate(
    ig_count_imptd = ifelse(
      is.na(ig_count),
      mean(c(lead, lag), na.rm = TRUE),
      ig_count
    )
  ) %>%
  ungroup()

# check there are no missings
sum(is.na(ig_zcta_counts$ig_count_imptd))

# check there are no zero counts
ig_zcta_counts %>%
  group_by(zcta_id) %>%
  summarise(n_0s = sum(ig_count_imptd == 0)) %>%
  arrange(desc(n_0s))

# remove these
ig_zcta_counts <- ig_zcta_counts %>%
  filter(ig_count_imptd!= 0)

# merge this directly to the census and save out
# ------------------------------------------
decennial <- ig_zcta_counts %>% 
  filter(year == 2010) %>%
  inner_join(cens_2010_zcta)

# check merge 
length(unique(decennial$zcta_id))
length(unique(ig_zcta_counts$zcta_id))
length(unique(cens_2010_zcta$zcta_id))

# check which are missing
cens_2010_zcta %>%
  filter(!zcta_id %in% ig_zcta_counts$zcta_id) %>%
  .$zcta_id %>%
  unique()

# fix names so they align with the ACS files
# remove "census_2010 from variable names
names(decennial) <- gsub("census_2010", "", names(decennial))
# manual renames
decennial <- decennial %>%
  rename("chh_density" = "chh_density_")

saveRDS(decennial, "data/prepped_data/zctas_decennial.rds")

# Merge ACS with IG 
# -----------------
zctas_all_5 <- acs_5_zcta %>%
  mutate(year = as.character(year)) %>%
  right_join(ig_zcta_counts) %>% # right join to keep all years of ACS data
  arrange(zcta_id, year)
length(unique(zctas_all_5$zcta_id))

decennial %>%
  filter(!(zcta_id %in% acs_5_zcta$zcta_id)) %>%
  .$zcta_id %>%
  unique()

# Now we need to drop cases where ACS is missing for years where the 
# ACS is available
years <- unique(acs_5_zcta$year)
zctas_all_5 <- zctas_all_5 %>%
  filter(!((year %in% years) & is.na(chh_)))


zctas_all_5 %>%
  sample_n(10000) %>%
  vis_miss()

# Remove near-empty columns
zctas_all_5 <- zctas_all_5 %>%
  select(-ppov_, -ppost2000_, -ppost2010_, -ruca_mixed)

# Log the counts 
# --------------
# zctas_all_5 <- zctas_all_5 %>%
#  mutate(lcensus = log(chh_),
#         lcensus_gq = log(chh_gq_),
#         lig = log(ig_count_imptd))

# Write out data
# --------------
saveRDS(zctas_all_5, "data/prepped_data/zctas_acs.rds")
