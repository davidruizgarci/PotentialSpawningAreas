# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 3.1. Create data subsets
#-------------------------------------------------------------------------------
library(dplyr)

#Load data
data <- read.csv("temp/data_2D_3D_dist_eke_SD_FINAL.csv", sep = ";") 
names(data)
head(data)

# Calculate how many tows have presence data of any species:
# Summarize the data by 'code'
counts <- data %>%
  group_by(code) %>%
  summarize(
    PA = sum(presence_absence, na.rm = TRUE),
    depth = first(depth))

# Filter rows where PA is not equal to 0
counts_filtered <- counts %>%
  filter(PA != 0)

counts_filtered <- counts_filtered %>%
  mutate(
    PA = as.numeric(gsub(",", ".", PA)),
    depth = as.numeric(gsub(",", ".", depth)))


# Count rows with total_PA < 200 and total_PA > 200
summary_stats <- counts_filtered %>%
  summarize(
    count_less_200 = sum(depth < 200),
    count_more_200 = sum(depth > 200),
    total_rows = n()) %>%
  mutate(
    perc_less_200 = (count_less_200 / total_rows) * 100,
    perc_more_200 = (count_more_200 / total_rows) * 100)


# subet by species:
output_dir <- file.path(temp_data, paste0("data_subsets"))
if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)}

genera <- unique(data$Genus)

# Loop through each genus
for (genus in genera) {
  # Filter data for the current genus
  genus_data <- data %>% 
    filter(Genus == genus)
  
  # Save the dataset to a CSV file
  output_file <- file.path(output_dir, paste0(genus, ".csv"))
  write.csv2(genus_data, file = output_file, row.names = FALSE)
}
