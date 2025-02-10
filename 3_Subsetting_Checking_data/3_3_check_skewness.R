# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 3.3. Check skewness in predictors and log transform if needed
#-------------------------------------------------------------------------------
library(moments)
library(dplyr)

genus <- "Scyliorhinus" #"Raja" #"Scyliorhinus"

#Load data
file <- paste0(temp_data, "/data_subsets/", genus, ".csv")
data <- read.csv2(file)

names(data)
str(data)

# Select specific columns from the data dataset in which you want to assess skewness

varskew  <- c("fishingEffort", "bottomT_Reanalysis", "seabottom_soReanalysis", "distMounts", "eke", 
              "slope", "depth", "bathy","seabottom_o2Reanalysis", "SD_bottomT", "SD_o2") 

#"distCanyons", "distMounts", "distFans", "seabottom_voReanalysis", "eke", "roughness", 
#"seabottom_o2Reanalysis", "seabottom_nppvReanalysis", "seabottom_phReanalysis", "seabottom_nh4Reanalysis",
#"seabottom_no3Reanalysis", "seabottom_po4Reanalysis", , "seabottom_uoReanalysis",


# Select columns with environmental data
class(varskew)
selvarskew <- data %>% dplyr::select(all_of(varskew))
head(selvarskew)
str(selvarskew)

# Initialize an empty data frame for skewness table
skewness_table <- data.frame(variable = character(), skewness = numeric(), shapiro = numeric())

# Iterate over column names in selvarskew dataset
for (col_name in names(selvarskew)) {
  # Check if column is numeric
  if (is.numeric(selvarskew[[col_name]])) {
    # Original variable
    column <- selvarskew[[col_name]]
    column_noNA <- column[!is.na(column)]
    
    # Calculate skewness and perform Shapiro-Wilk test for the original variable
    skewness_value <- skewness(column_noNA)
    shapiro_test <- shapiro.test(column_noNA)
    p_value <- shapiro_test$p.value
    
    # Add variable name, skewness value, and p-value to the skewness table
    skewness_table <- rbind(
      skewness_table,
      data.frame(
        variable = col_name,
        skewness = skewness_value,
        shapiro = p_value
      )
    )
    
    # Transformed variable using log1p()
    column_transformed <- log1p(column_noNA)
    
    # Calculate skewness and perform Shapiro-Wilk test for the transformed variable
    LN_skewness_value <- skewness(column_transformed)
    LN_shapiro_test <- shapiro.test(column_transformed)
    LN_p_value <- LN_shapiro_test$p.value
    
    # Add variable name, skewness value, and p-value for the transformed variable to the skewness table
    skewness_table <- rbind(
      skewness_table,
      data.frame(
        variable = paste0("LN_", col_name),
        skewness = LN_skewness_value,
        shapiro = LN_p_value
      )
    )
  }
}

# Print the skewness table
print(skewness_table) 
#Improve: all execept: TL, WeightLWR, Cloud.cover, Net_horizontal_opening, Distance_covered_GPS, Average_speed, 

data <- data %>% #recomendacion solo si cambia mucho.
  mutate(LN_slope = log1p(slope),
         LN_fishingEffort = log1p(fishingEffort))
head(data)

output_file <- file.path(temp_data, "data_subsets", paste0(genus,"_dataset_log_pred.csv")) 
write.csv2(data, file = output_file, row.names = FALSE)
