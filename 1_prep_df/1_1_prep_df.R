# -----------------------------------------------------------------------------

# Title:Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#--------------------------------------------------------------------------------
# 1.1. Cleaning and organizing datasets
#--------------------------------------------------------------------------------
library(dplyr)

#Load data
# 1.) ECEME --------------------------------------------------------------------
data <- read.csv2("input/Original/CPUE.csv", sep = ";")
head(data)

# Dipturus eggcases will not be used here you can remove them
data <- data %>%
  filter(Sp != "Dipturus_oxyrinchus")
names(data)

# merge Raja rows
data1 <- data %>%
  group_by(code, Genus) %>%
  summarise(
    Sp = NA,
    lat = first(lat),
    lon = first(lon),
    season = first(season),
    depth = first(depth),
    presence_absence = max(presence_absence),
    swept_area_km2 = first(swept_area_km2),
    N = sum(N),
    N_km2 = sum(N_km2),
    .groups = 'drop'  # Ensure the result is not grouped
  )

# View the updated dataframe
head(data1)

# Check that all rows are twice (Scyliorhinus and Raja)
# Group by code and count occurrences
code_counts <- data1 %>%
  group_by(code) %>%
  summarise(count = n()) #Some are three times (have eggs from 2 Raja spp)

# Create absences in all the tows:
# cefine the unique Genus values
unique_genus <- c("Scyliorhinus", "Raja")
# function to ensure each code has rows for each unique genus
expand_rows <- function(df, unique_genus) {
  # Identify unique codes
  unique_codes <- unique(df$code)
  # Create an empty dataframe to store the results
  result_df <- data.frame()
  
  # Iterate over each unique code
  for (code in unique_codes) {
    # Filter rows for the current code
    current_rows <- df %>% filter(code == !!code)
    
    # Get the existing Genus values for the current code
    existing_genus <- unique(current_rows$Genus)
    
    # Identify the missing Genus values
    missing_genus <- setdiff(unique_genus, existing_genus)
    
    # Create rows for missing Genus values
    for (genus in missing_genus) {
      # Create a new row with the required columns
      new_row <- current_rows[1, ]
      new_row$Sp <- genus
      new_row$Genus <- genus
      new_row$presence_absence <- 0
      new_row$N <- 0
      new_row$N_km2 <- 0

      # Append the new row to the result dataframe
      result_df <- rbind(result_df, new_row)
    }
    
    # Append the existing rows to the result dataframe
    result_df <- rbind(result_df, current_rows)
  }
  
  # Return the resulting dataframe
  return(result_df)
}

# Apply the function to presCat_updated
data2 <- expand_rows(data1, unique_genus)
names(data2)

# Check that all rows are twice (Scyliorhinus and Raja)
# Group by code and count occurrences
code_counts <- data2 %>%
  group_by(code) %>%
  summarise(count = n()) #Some are three times (have eggs from 2 Raja spp)

# Save dataframe
write.csv2(data2, "temp/pres_absECEME.csv", row.names = FALSE)

# 2.) ICM ----------------------------------------------------------------------

# Create a presence absence dataset:
# Eggcase data (only presence):
presCat <- read.csv2("input/Original/DatosCatalunyaECEME_Final.csv", sep = ";")
head(presCat)

# All tows (presence and absence)
pres_absCat <- read.csv2("input/Original/Datos_ICM_PosicionesPescas.csv", sep = ";")
head(pres_absCat)

# Merge data from absence tows - include: code, lat, lon, depth, season
# remove duplicates in pres_absCat based on the code column
pres_absCat_unique <- pres_absCat[!duplicated(pres_absCat$code), ]
# identify the rows in pres_absCat where code is not present in presCat
new_rows <- pres_absCat_unique[!pres_absCat_unique$code %in% presCat$code, ]
# extract the necessary columns
new_rows_to_add <- new_rows[, c("code", "lat", "lon", "season", "depth")]
# add missing columns to new_rows_to_add to match the structure of presCat
# assuming other columns should be NA or 0 (based on your example structure)
new_rows_to_add <- cbind(
  Species = NA,
  Genus = "Scyliorhinus",
  new_rows_to_add,
  Region = NA,
  NetHorizontalOpenening = NA,
  Swept_area_km2 = NA,
  N = 0,
  N_km2 = 0,
  Kg = 0,
  Kg_km2 = 0)

# bind the new rows to presCat
presCat_updated <- rbind(presCat, new_rows_to_add)
head(presCat_updated) #129 unique tows
names(presCat_updated)

# Create absences in all the tows:
# cefine the unique Genus values
unique_genus <- c("Scyliorhinus", "Raja")
# function to ensure each code has rows for each unique genus
expand_rows <- function(df, unique_genus) {
  # Identify unique codes
  unique_codes <- unique(df$code)
  # Create an empty dataframe to store the results
  result_df <- data.frame()
  
  # Iterate over each unique code
  for (code in unique_codes) {
    # Filter rows for the current code
    current_rows <- df %>% filter(code == !!code)
    
    # Get the existing Genus values for the current code
    existing_genus <- unique(current_rows$Genus)
    
    # Identify the missing Genus values
    missing_genus <- setdiff(unique_genus, existing_genus)
    
    # Create rows for missing Genus values
    for (genus in missing_genus) {
      # Create a new row with the required columns
      new_row <- current_rows[1, ]
      new_row$Species <- genus
      new_row$Genus <- genus
      new_row$N <- 0
      new_row$N_km2 <- 0
      new_row$Kg <- 0
      new_row$Kg_km2 <- 0
      
      # Append the new row to the result dataframe
      result_df <- rbind(result_df, new_row)
    }
    
    # Append the existing rows to the result dataframe
    result_df <- rbind(result_df, current_rows)
  }
  
  # Return the resulting dataframe
  return(result_df)
}

# Apply the function to presCat_updated
presCat_expanded <- expand_rows(presCat_updated, unique_genus)
names(presCat_expanded)

# Check that all rows are twice (Scyliorhinus and Raja)
# Group by code and count occurrences
code_counts <- presCat_expanded %>%
  group_by(code) %>%
  summarise(count = n()) #Some are three times (have eggs from 2 Raja spp)

# merge Raja rows
presCat_merged <- presCat_expanded %>%
  group_by(code, Genus) %>%
  summarise(
    Species = first(Species),
    lat = first(lat),
    lon = first(lon),
    Region = first(Region),
    season = first(season),
    depth = first(depth),
    NetHorizontalOpenening = first(NetHorizontalOpenening),
    Swept_area_km2 = first(Swept_area_km2),
    N = sum(N),
    N_km2 = sum(N_km2),
    Kg = sum(Kg),
    Kg_km2 = sum(Kg_km2),
    .groups = 'drop'  # Ensure the result is not grouped
  )

# View the updated dataframe
names(presCat_merged)

# Check that all rows are twice (Scyliorhinus and Raja)
# Group by code and count occurrences
code_counts <- presCat_merged %>%
  group_by(code) %>%
  summarise(count = n()) #Some are three times (have eggs from 2 Raja spp)

# Add a presence_absence column
presCat_merged <- presCat_merged %>%
  mutate(presence_absence = ifelse(N != 0, 1, 0))

# Save dataframe
write.csv2(presCat_merged, "temp/pres_absCat.csv", row.names = FALSE)
