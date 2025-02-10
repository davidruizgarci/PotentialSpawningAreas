# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 3.4. Check correlation between predictors using pearson
#-----------------------------------------------------------------
library(corrplot)
library(Hmisc)
library(dplyr)

genus <- "Raja" #"Raja" #"Scyliorhinus"

#Load data
file <- paste0(temp_data, "/data_subsets/", genus, "_dataset_log_pred.csv")
data <- read.csv2(file)

names(data)
str(data)

#Set categorical predictors as categories:
data <- data %>% 
  mutate(season = factor(season, c(1:4)))

# Convert the 'time' column to Date format if needed 
data$date <- as.Date(data$date) #, format = "%Y-%m-%d"
data$date_time <- as.POSIXct(data$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Change the name of some variables as you want them to appear in the figure for the paper:
names(data)
colnames(data) <- c("code", "Genus", "lat", "lon", "season", "depth", 
                    "swept_area_km2", "N", "N_km2", "presence_absence", "date", 
                    "date_time", "bathy", "substrate", "slope", "roughness", 
                    "fishingEffort", "distCanyons", "distMounts", "distFans", 
                    "bottom_temp", "bottom_oxygen","bottom_nppv", "bottom_ph", 
                    "bottom_nh4", "bottom_no3", "bottom_po4", "bottom_so", 
                    "bottom_uo", "bottom_vo", "bottom_eke", "SD_bottomT", "SD_o2",
                    "SubAll", "bioSubsFinal", "ln_slope", "ln_fishingEffort")

summary(data)

# Select specific columns from the data dataset in which you want to assess correlation
vars  <- c("depth", "roughness", "bottom_temp", "bottom_no3", 
           "bottom_po4", "bottom_so", "slope", "ln_fishingEffort", "bottom_eke") 

#"fishingEffort", "bottom_nppv", "bottom_ph", "bottom_nppv", "distMounts", 
#"distCanyons", "distFans", "bottom_nh4""SD_bottomT", "SD_o2", "bottom_oxygen",

# calculate correlations using Pearson
correlations <- cor(na.omit(dplyr::select(data, all_of(vars))), method="pearson")


# plot correlations
output_dir <- paste0(output_data, "/prefitting_checks/", genus)
if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)}
pngfile <- paste0(output_dir, "/", genus, "_eda_corplot.png")
png(pngfile, width=3000, height=3000, res=300)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlations, method="color",col=col(200), tl.col = "black", order = "original", diag=FALSE, type="upper", 
         addCoef.col = "black") # Add coefficient of correlation
dev.off()

# Calculate correlations using Spearman and clustering analysis
# plot: #A9D18E for S canicula and #F4B183 for G melastomus
pngfile <- paste0(output_dir, "/", genus, "_eda_cluster.png")
png(pngfile, width=2500, height=2000, res=300)
op <- par(mar=c(0.5,5,0.5,0.5))
v <- as.formula(paste("~", vars, collapse="+"))
plot(varclus(v, similarity=c("spearman"),data=data),cex=1.2) # plot cluster
abline(a=0.30,0,col="steelblue",lty=1,lwd=2)
par(op)
dev.off()

# If two variables are very correlated, there will be convergence. You must eliminate the correlated ones.
# Then you select based on your interest (more ecologic sense, something that you can explain better)
# Spearman is usually used for this. When the values is larger than 0.7 between two variables, you must select only one of this.
# All predictors are uncorrelated (Spearman correlations <0.7), therefore none of them is discarded.
# You may use VIF as well, but this is used on the model, rather than upon the variables itself. Here we want to look at variables it self, because we are using machine learning.
# VIF may be also only for linear relationships and Spearman for non-linear too, but we are not sure, should look for more info. But basically jut use Spearman.

#Make a selection eliminating those that are harder to explain or make less sense:
vars  <- c("depth", "bottom_temp",  "bottom_so", "slope", "ln_fishingEffort", "bottom_eke") 

# calcualate correlations using Spearman and clustering analysis
pngfile <- paste0(output_dir, "/", genus, "_eda_cluster_final.png")
png(pngfile, width=2500, height=2000, res=300)
op <- par(mar=c(0.5,5,0.5,0.5))
v <- as.formula(paste("~", vars, collapse="+"))
plot(varclus(v, similarity=c("spearman"),data=data),cex=.8) # plot cluster
abline(a=0.30,0,col="grey70",lty=1,lwd=2)
par(op)
dev.off()

