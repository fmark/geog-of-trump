library(choroplethr)
library(dplyr)

fips <- read.csv("http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", header = F)
names(fips) <- c("state_abbr", "STATEFP", "COUNTYFP", "county_name", "CLASSFP")
fips <- mutate(fips,
               FIPS=as.integer(sprintf("%02d%03d", STATEFP, COUNTYFP))) %>%
  select(-STATEFP, -COUNTYFP, -CLASSFP)


votes <- read.csv("https://raw.githubusercontent.com/tonmcg/County_Level_Election_Results_12-16/master/US_County_Level_Presidential_Results_08-16.csv") %>%
  # Redo the percentages so we understand exactly what the numbers mean
  select(FIPS=fips_code,
         dem_2016, gop_2016, total_2016,
         dem_2008, gop_2008, total_2008) %>%
  mutate(trump_shift = (gop_2016/(gop_2016 + dem_2016)) -
           (gop_2008/(gop_2008 + dem_2008)),
         total_votes = (total_2016 + total_2008) / 2) %>%
  left_join(fips, by="FIPS") %>%
  # Drop Alaska since they don't do county-level results and
  # drop Utah since it was a three-way race there
  filter(!state_abbr %in% c("AK", "UT"))


# Load Case & Deaton white misery death rate
cd_white_misery_12_14 <- read.csv("Mortality from Case and Deaton misery by county for white non-Hispanics aged 25-64, 2012-2014.txt", sep = "\t") %>%
  rename(FIPS = County.Code) %>%
  filter(!is.na(FIPS),
         !Age.Adjusted.Rate %in% c("Suppressed", "Unreliable", "Missing")) %>%
  mutate(Age.Adjusted.Rate = as.numeric(as.character(Age.Adjusted.Rate)),
         Population=as.numeric(as.character(Population)),
         FIPS=as.integer(as.character(FIPS))) %>%
  select(FIPS, Population_12_14=Population, Death_rate_12_14=Age.Adjusted.Rate)

cd_white_misery_06_08 <- read.csv("Mortality from Case and Deaton misery by county for white non-Hispanics aged 25-64, 2006-2008.txt", sep = "\t") %>%
  rename(FIPS = County.Code) %>%
  filter(!is.na(FIPS),
         !Age.Adjusted.Rate %in% c("Suppressed", "Unreliable", "Missing")) %>%
  mutate(Age.Adjusted.Rate = as.numeric(as.character(Age.Adjusted.Rate)),
         Population=as.numeric(as.character(Population)),
         FIPS=as.integer(as.character(FIPS))) %>%
  select(FIPS, Population_06_08=Population, Death_rate_06_08=Age.Adjusted.Rate)

cd_white_misery <- inner_join(cd_white_misery_12_14, cd_white_misery_06_08, by="FIPS") %>%
  mutate(Pop_growth=Population_12_14-Population_06_08,
         Misery_growth=Death_rate_12_14-Death_rate_06_08)

rm(cd_white_misery_06_08, cd_white_misery_12_14)

misery_votes <- left_join(cd_white_misery, votes, by="FIPS")

write.csv(misery_votes, "results/misery_votes.csv", row.names = FALSE)





