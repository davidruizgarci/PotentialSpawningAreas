# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 3.2. Check missing data in predictors
#-------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)

genus <- "Scyliorhinus" #"Raja" #"Scyliorhinus"

#Load data
file <- paste0(temp_data, "/data_subsets/", genus, ".csv")
data <- read.csv2(file)

names(data)
str(data)

#---------------------------------------------------------------------------------------------------
# Select predictors            
#---------------------------------------------------------------------------------------------------
#Create function for plot how many missing values are there in a particular column (taken from dmarch github: https://github.com/dmarch/agazella):
plot_Missing <- function(data_in, title = NULL){
  # https://www.kaggle.com/notaapple/detailed-exploratory-data-analysis-using-r
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("grey80", "grey10"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}

# set names of the environmental variables
vars <- c("fishingEffort", "distCanyons", "distMounts", "distFans", "bottomT_Reanalysis", 
          "seabottom_o2Reanalysis", "seabottom_nppvReanalysis", "seabottom_phReanalysis", "seabottom_nh4Reanalysis",
          "seabottom_no3Reanalysis", "seabottom_po4Reanalysis", "seabottom_soReanalysis", "seabottom_uoReanalysis",
          "seabottom_voReanalysis", "eke", "subs", "slope", "roughness", "SD_bottomT", "SD_o2")

# Select columns with environmental data
selEnv <- data %>% dplyr::select(all_of(vars))
# Plot missing data for environmental variables and save it:
#Create folder to save it:
output_dir <- file.path(output_data, paste0("prefitting_checks/", genus))
if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)}

#Plot and save:
setwd(output_dir)
jpeg(file = "MissingData.jpeg", 
     width = 23.8, height = 21.65, units = "cm", res = 300)
plot_Missing(selEnv) 
dev.off()
