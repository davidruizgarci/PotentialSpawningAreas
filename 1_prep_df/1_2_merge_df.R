# -----------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#--------------------------------------------------------------------------------
# 1.2. Merging dataframes and adding spatio-temporal references
#--------------------------------------------------------------------------------
library(dplyr)

# ECEME
ECEME <- read.csv2("temp/pres_absECEME.csv", sep = ";")

# extract date from the code data:
# Add year, month, and day columns in year-month-day format
ECEME <- ECEME %>%
  mutate(
    date = as.Date(paste0("20", substr(code, 1, 2), "-", substr(code, 4, 5), "-", substr(code, 7, 8))))%>%
    select(-Sp)
names(ECEME)

# ICM
ICM <- read.csv2("temp/pres_absCat.csv", sep = ";")

# extract date from the code data:
ICM <- ICM %>%
  mutate(
    date = as.Date(paste0(substr(code, 1, 4), "-", substr(code, 5, 6), "-", substr(code, 7, 8))))
names(ICM)

# Rename and remove columns in ICM
ICM <- ICM %>%
  rename(
    Sp = Species,
    swept_area_km2 = Swept_area_km2
  ) %>%
  select(-Region, -NetHorizontalOpenening, -Kg, -Kg_km2, -Sp)
names(ICM)

# Merge ECEME and ICM dataframes
merged_data <- merge(ECEME, ICM, by = c("code", "Genus", "lat", "lon", "season", "depth", "swept_area_km2", "N", "N_km2", "presence_absence", "date"), all = TRUE)

# Group by code and count occurrences
code_counts <- merged_data %>%
  group_by(code) %>%
  summarise(count = n()) 

# Save dataframe
write.csv2(merged_data, "temp/pres_absData.csv", row.names = FALSE)
