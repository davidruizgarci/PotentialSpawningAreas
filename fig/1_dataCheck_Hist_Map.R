# -----------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 1. Plot data spatial distribution and amount of data per year - data checks
#-------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(sf)
library(readr)
library(patchwork)
library(tidyr)

# 1. All data ------------------------------------------------------------------
# Set data repository and load rasters 
#Mask
mask<- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
print(mask)
mask <- st_transform(mask, crs = 4326)

#Load data
data <- read.csv2("temp/data_2D_3D_dist.csv", sep = ",") 
names(data)
data <- data %>%
  mutate(lon = as.numeric(gsub(",", ".", lon)),
         lat = as.numeric(gsub(",", ".", lat)))

# Make zoomed in map 

# Create a ggplot object
p <- ggplot() +
  # land mask
  geom_sf(data = mask) +
  # add tracks
  geom_point(data = data, aes(x = lon, y = lat, fill = "#FFE4B2"), shape = 21, size = 1.5, alpha = 0.6) + ##FFE4B2
  #Set spatial bounds
  coord_sf(xlim = c(-6, 4), ylim = c(35, 45), expand = TRUE) +
  # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.2) +  
  # theme
  theme_bw() +
  # Directly map colors without scaling
  scale_fill_identity()+
  # Remove grids
  theme(panel.grid = element_blank())

# export plot
outdir <- paste0(output_data, "/fig/Map")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/StudyArea.png")
ggsave(p_png, p, width=23, height=17, units="cm", dpi=300)





# 2. Each species --------------------------------------------------------------
setwd(paste0(temp_data, "/data_subsets"))

# List all CSV files in the directory
file_list <- list.files(pattern = "\\.csv$")

# Read all CSV files into a list of data frames
data_list <- lapply(file_list, read_csv2)
# Convert each tibble in the list to a data frame
data_list <- lapply(data_list, as.data.frame)
summary(data_list)
head(data_list[1])
str(as.data.frame(data_list[1]))

# filter only rows in which the species is present
presence_data_list <- lapply(data_list, function(df) {
  df[df$presence_absence == 1, ]
})

# Create output directory if it doesn't exist
outdir <- paste0(output_data, "/fig/Map/Sp_dataCheck/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# Loop through each dataframe in data_list
for (i in seq_along(presence_data_list)) {
  df <- presence_data_list[[i]]
  
  # Create a ggplot object for each dataframe
  p1 <- ggplot() +
    geom_sf(data = mask) +  # Add land mask
    geom_point(data = df, aes(x = lon, y = lat, fill = "#FFE4B2"), 
               shape = 21, size = 1.5, alpha = 0.6) +  # Add points
    coord_sf(xlim = c(-6, 4), ylim = c(35, 45), expand = TRUE) +  # Set spatial bounds
    theme_bw() +  # Apply theme
    scale_fill_identity() +  # Map colors directly without scaling
    theme(panel.grid = element_blank()) +  # Remove grids
    labs(title = paste0(sub("\\.csv$", "", file_list[i])))
  
  # Save each plot with a unique filename
  file_name <- sub("\\.csv$", "", file_list[i])  # Assuming file_list contains your file names
  p_png <- paste0(outdir, tools::file_path_sans_ext(file_name), ".png")
  
  ggsave(filename  = p_png, p, width = 23, height = 13, units = "cm", dpi = 300)
}



# 3. Each species, presence and all---------------------------------------------
setwd(paste0(temp_data, "/data_subsets"))

# List all CSV files in the directory
file_list <- list.files(pattern = "\\.csv$")

# Read all CSV files into a list of data frames
data_list <- lapply(file_list, read_csv2)
# Convert each tibble in the list to a data frame
data_list <- lapply(data_list, as.data.frame)
summary(data_list)
str(as.data.frame(data_list[1]))

# filter only rows in which the species is present
presence_data_list <- lapply(data_list, function(df) {
  df[df$presence_absence == 1, ]
})
summary(presence_data_list)
str(as.data.frame(presence_data_list[1]))

# Create output directory if it doesn't exist
outdir <- paste0(output_data, "/fig/exploratory/Map/DataAvailability/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# Loop through each dataframe in both presence_data_list and data_list
for (i in seq_along(presence_data_list)) {
  df_presence <- presence_data_list[[i]]
  df_data <- data_list[[i]]
  
  # Create ggplot objects for each dataframe
  p1 <- ggplot() +
    geom_sf(data = mask) +  # Add land mask
    geom_point(data = df_presence, aes(x = lon, y = lat, fill = "#FFE4B2"), 
               shape = 21, size = 1.5, alpha = 0.6) +  # Add points
    coord_sf(xlim = c(-6, 4), ylim = c(35, 45), expand = TRUE) +  # Set spatial bounds
    theme_bw() +  # Apply theme
    scale_fill_identity() +  # Map colors directly without scaling
    theme(panel.grid = element_blank()) +  # Remove grids
    labs(title = paste0(sub("\\.csv$", "", file_list[i])))
  
  p2 <- ggplot() +
    geom_sf(data = mask) +  # Add land mask
    geom_point(data = df_data, aes(x = lon, y = lat, fill = "#FFE4B2"), 
               shape = 21, size = 1.5, alpha = 0.6) +  # Add points
    coord_sf(xlim = c(-6, 4), ylim = c(35, 45), expand = TRUE) +  # Set spatial bounds
    theme_bw() +  # Apply theme
    scale_fill_identity() +  # Map colors directly without scaling
    theme(panel.grid = element_blank()) +  # Remove grids
    labs(title = paste0(sub("\\.csv$", "", file_list[i])))
  
  # Create histogram plot (p3)
  p3 <- ggplot(df_data, aes(x = presence_absence)) +
    geom_histogram(binwidth = 1, fill = "grey", color = "black") +
    scale_x_continuous(breaks = c(0, 1)) +
    labs(title = paste0(sub("\\.csv$", "", file_list[i])),
         x = "Presence/Absence",
         y = "Count") +
    theme_minimal()
  
  # Combine p1, p2, and p3 in a single row using patchwork
  combined_plot <- (p1 / p2) / p3  # Arrange p1, p2, and p3 horizontally
  
  # Save each plot with a unique filename
  file_name <- sub("\\.csv$", "", file_list[i])  # Assuming file_list contains your file names
  p_png <- paste0(outdir, tools::file_path_sans_ext(file_name), ".png")
  
  ggsave(filename = p_png, plot = combined_plot, width = 15, height = 30, units = "cm", dpi = 300)
}

# 2) Data frame on the number of surveys and presences
# Function to count rows by Genus and presence_absence in a list of dataframes
count_presence_absence <- function(data_list) {
  # Initialize an empty dataframe to store counts
  counts <- data.frame(Genus = character(), presence_absence = integer(), Rows = integer(), stringsAsFactors = FALSE)
  
  # Loop through each dataframe in the data_list
  for (df in data_list) {
    # Count rows by Genus and presence_absence
    genus_counts <- df %>%
      group_by(Genus, presence_absence) %>%
      summarise(Rows = n(), .groups = 'drop')
    
    # Combine counts from all dataframes
    counts <- bind_rows(counts, genus_counts)
  }
  
  # Summarise the total rows for each Genus and presence_absence across all dataframes
  counts <- counts %>%
    group_by(Genus, presence_absence) %>%
    summarise(Rows = sum(Rows), .groups = 'drop')
  
  return(counts)
}

# Get row counts for each genus and presence_absence in presence_data_list
presence_absence_counts <- count_presence_absence(data_list)
# Reshape the dataframe to have separate columns for presence_absence counts
final_counts <- presence_absence_counts %>%
  pivot_wider(names_from = presence_absence, values_from = Rows, names_prefix = "presence_absence_") %>%
  # Replace NA values with 0
  replace(is.na(.), 0)

# Rename columns for clarity
final_counts <- final_counts %>%
  rename(Count_presence_absence_0 = presence_absence_0,
         Count_presence_absence_1 = presence_absence_1)

# Print the final counts dataframe
print(final_counts)

output_file <- file.path(temp_data, "pres_abs.csv")
write.csv2(final_counts, file = output_file, row.names = FALSE)


# 4. Frequency of occurence ----------------------------------------------------
head(data)

# Determine if for wach tow there is any egg case:
PA <- data %>%
  group_by(code) %>%
  summarize(
    Genus = first(Genus),         # Keeps the first occurrence for other columns
    lat = first(lat),
    lon = first(lon),
    season = first(season),
    depth = first(depth),
    swept_area_km2 = first(swept_area_km2),
    N = first(N),
    N_km2 = first(N_km2),
    presence_absence = max(presence_absence),  # Sets to 1 if any row has presence, else 0
    date = first(date)
  )

# View the resulting dataframe
head(PA)

# Calculate the percentage of 0s and 1s in presence_absence
PA_summary <- PA %>%
  count(presence_absence) %>%
PA_summary

# Calculate percentage of 0 and 1 in presence_absence for depth < 200
PA_summary_lt200 <- PA %>%
  filter(depth < 200) %>%
  count(presence_absence) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  mutate(depth_range = "< 200 m")

# Calculate percentage of 0 and 1 in presence_absence for depth >= 200
PA_summary_gt200 <- PA %>%
  filter(depth >= 200) %>%
  count(presence_absence) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  mutate(depth_range = ">= 200 m")

# Combine both summaries
PA_summary <- bind_rows(PA_summary_lt200, PA_summary_gt200)

# Display the result
PA_summary
