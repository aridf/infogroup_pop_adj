# Cleaning and merging tract data

suppressPackageStartupMessages({
  library(tidyverse)
  library(visdat)
})

# Load in data
acs_5_tract <- read_csv("data/tract_acs_2009_2018_long.csv")
cens_2010_tract <- read_csv("data/census/tract_data_2010.dta.csv")
ig_tract <- read_csv("data/infogroup_counts/tract_2006_2019.csv")
ig_tract_rows <- nrow(ig_tract)

# Check that no counts in the ACS are missing
acs_5_tract %>%
  group_by(tractid) %>%
  summarise(n_NAs = sum(is.na(chh_))) %>%
  arrange(desc(n_NAs))
# There are missing counts in the ACS data
# Check the distribution by year
n_missing_yearly <- acs_5_tract %>%
  group_by(year) %>%
  summarise(n_NAs = sum(is.na(chh_))) %>%
  arrange(desc(n_NAs))
# Looks consistent across time

# Drop 2009 and the tracts with more than 1 missing data
acs_5_tract <- acs_5_tract %>%
  filter(year > 2009)

to_remove <- acs_5_tract%>%
  group_by(tractid) %>%
  summarise(n_NAs = sum(is.na(chh_))) %>%
  filter(n_NAs > 0) %>%
  .$tractid
acs_5_tract <- acs_5_tract %>%
  filter(!(tractid %in% to_remove))

# Check for ACS zero counts
tracts_w_census_zero <- acs_5_tract %>%
  group_by(tractid) %>%
  summarize(n_0 = sum(chh_ == 0)) %>%
  arrange(desc(n_0)) %>%
  filter(n_0 > 0)
# there are 923

# Drop these
to_remove <- tracts_w_census_zero$tractid
acs_5_tract <- acs_5_tract %>%
  filter(!(tractid %in% to_remove))

length(unique(acs_5_tract$tractid))

# Clean up Infogroup data
# -----------------------
# Filter down missing id variables
ig_tract <- ig_tract %>%
  rename("tractid" = "GEOID10") %>%
  filter(!grepl("[A-Za-z]", tractid)) %>% # drop the cols with state names
  filter(!is.na(tractid))
ig_tract_rows - nrow(ig_tract)

# Filter down to tracts that have matches in ACS
ig_tract <- ig_tract %>%
  filter(tractid %in% acs_5_tract$tractid)
length(unique(ig_tract$tractid))

# Filter down to just household counts for IG
ig_tract <- ig_tract %>%
  select(tractid, starts_with("hh_20")) 

# Pivot to tract-year units
# ------------------------
ig_tract_counts <- ig_tract %>%
  pivot_longer(
    -tractid,
    names_to = c("measure", "year"),
    names_sep = "_",
    values_to = "ig_count"
  ) %>%
  select(-measure) %>%
  mutate(year = as.numeric(year))

# check max missings for a county
missings <- ig_tract_counts %>%
  group_by(tractid) %>%
  summarise(n_NAs = sum(is.na(ig_count))) %>%
  arrange(desc(n_NAs))
missings
# max is 1, fine, move on

# now wherever possible impute missing ig count 
# as average of current and next year
sum(is.na(ig_tract_counts$ig_count))
ig_tract_counts <- ig_tract_counts %>%
  group_by(tractid) %>%
  mutate(lead = lead(ig_count),
         lag = lag(ig_count)) %>%
  ungroup()

ig_tract_counts <- ig_tract_counts %>%
  group_by(year, tractid) %>%
  mutate(
    ig_count_imptd = ifelse(
      is.na(ig_count),
      mean(c(lead, lag), na.rm = TRUE),
      ig_count
    )
  ) %>%
  ungroup()

sum(is.na(ig_tract_counts$ig_count_imptd))

# check tracts where impute IG == 0
zeros <- ig_tract_counts %>% 
  group_by(tractid) %>%
  summarize(n_0s = sum(ig_count_imptd == 0)) %>%
  arrange(desc(n_0s)) %>%
  filter(n_0s > 0)

# filter these out
ig_tract_counts <- ig_tract_counts %>%
  filter(ig_count_imptd != 0)

# merge ig to the decennial and save out
# --------------------------------------
decennial <- ig_tract_counts %>% 
  filter(year == 2010) %>%
  inner_join(cens_2010_tract)

# check merge 
length(unique(decennial$tractid))
length(unique(ig_tract_counts$tractid))
length(unique(cens_2010_tract$tractid))

cens_2010_tract %>%
  filter(!(tractid %in% ig_tract_counts$tractid)) %>%
  .$tractid

# fix names so they align with the ACS files
# remove "census_2010 from variable names
names(decennial) <- gsub("census_2010", "", names(decennial))
# manual renames
decennial <- decennial %>%
  rename("chh_density" = "chh_density_")

saveRDS(decennial, "data/prepped_data/tracts_decennial.rds")

# merge ig to acs 5 year
tracts_all_5 <- acs_5_tract %>%
  right_join(ig_tract_counts) %>%
  arrange(tractid, year)
length(unique(tracts_all_5$tractid))

# visualize data missingness
tracts_all_5 %>%
  sample_n(10000) %>%
  vis_miss()

# remove mostly missing columns
# tracts_all_5 <- tracts_all_5[, which(colMeans(!is.na(tracts_all_5)) > 0.8)]

saveRDS(tracts_all_5, "data/prepped_data/tracts_5_acs.rds")
