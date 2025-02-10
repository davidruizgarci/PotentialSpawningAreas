# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 3.5. Exploratory data analysis
#-------------------------------------------------------------------------------
library(gridExtra)
library(grid)

genus <- "Scyliorhinus" #"Raja" #"Scyliorhinus"

#Load data
file <- paste0(temp_data, "/data_subsets/", genus, ".csv")
data <- read.csv2(file)

names(data)
str(data)

# Create function for density.plot: 
# You will use this to create density plots of environmental data on alive and dead specimens:
density.plot <- function(title = "", xlab = "", legend = "", alpha = 0.35, data, var, group, cols = c("#d7191c", "#2c7bb6")) {
  
  g <- ggplot(data, aes(x = !!sym(var), fill = !!sym(group))) +
    geom_density(alpha = alpha) +
    scale_fill_manual(values = cols) +
    labs(title = title, x = xlab, y = "Density") +
    theme_light() +
    theme(legend.title = element_blank())  # Hide the legend title
  
  return(g)
}

density.box_plot <- function(title = "", xlab = "SST (ºC)", legend = "", data, var, group, cols = c("#d7191c", "#2c7bb6")) {
  
  g <- ggplot(data, aes(x = !!sym(group), y = !!sym(var), fill = !!sym(group))) +
    geom_boxplot(width = 0.5, alpha = 0.7, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = cols) +
    labs(title = title, x = xlab, y = "") +
    theme_light() +
    theme(legend.title = element_blank())  # Hide the legend title
  
  return(g)
}

# This plot helps you to look at which variables may be more important in the model.
# The way you interpret this is: when there is not overlap between dead (absence) and alive (presence)
# then the variable may be important. This indicates that there is a preference towards certain values.
# create plot per variable

# Create the plots
p1 <- density.box_plot(title = "Substrate", xlab = "Substrate", legend = "", data = data, var = "subs", group = "N_km2")
p2 <- density.plot(title = "Depth", xlab = "Depth", legend = "", alpha = 0.35, data = data, var = "depth", group = "N_km2")
p3 <- density.plot(title = "Slope", xlab = "Slope", legend = "", alpha = 0.35, data = data, var = "slope", group = "N_km2")
p4 <- density.plot(title = "Fishing Effort", xlab = "Fishing Effort", legend = "", alpha = 0.35, data = data, var = "fishingEffort", group = "N_km2")
p5 <- density.plot(title = "Dist Canyons", xlab = "Dist Canyons", legend = "", alpha = 0.35, data = data, var = "distCanyons", group = "N_km2")
p6 <- density.plot(title = "Dist Mounts", xlab = "Dist Mounts", legend = "", alpha = 0.35, data = data, var = "distMounts", group = "N_km2")
p7 <- density.plot(title = "Dist Fans", xlab = "Dist Fans", legend = "", alpha = 0.35, data = data, var = "distFans", group = "N_km2")
p8 <- density.plot(title = "Bottom Temp", xlab = "Bottom Temp", legend = "", alpha = 0.35, data = data, var = "bottomT_Reanalysis", group = "N_km2")
p9 <- density.plot(title = "Bottom Oxygen", xlab = "Bottom Oxygen", legend = "", alpha = 0.35, data = data, var = "seabottom_o2Reanalysis", group = "N_km2")
p10 <- density.plot(title = "Bottom NPPV", xlab = "Bottom NPPV", legend = "", alpha = 0.35, data = data, var = "seabottom_nppvReanalysis", group = "N_km2")
p11 <- density.plot(title = "Bottom pH", xlab = "Bottom pH", legend = "", alpha = 0.35, data = data, var = "seabottom_phReanalysis", group = "N_km2")
p12 <- density.plot(title = "Bottom NH4", xlab = "Bottom NH4", legend = "", alpha = 0.35, data = data, var = "seabottom_nh4Reanalysis", group = "N_km2")
p13 <- density.plot(title = "Bottom SO", xlab = "Bottom SO", legend = "", alpha = 0.35, data = data, var = "seabottom_soReanalysis", group = "N_km2")
p14 <- density.plot(title = "Bottom UO", xlab = "Bottom UO", legend = "", alpha = 0.35, data = data, var = "seabottom_uoReanalysis", group = "N_km2")
p15 <- density.plot(title = "Bottom VO", xlab = "Bottom VO", legend = "", alpha = 0.35, data = data, var = "seabottom_voReanalysis", group = "N_km2")
p16 <- density.plot(title = "Bottom EKE", xlab = "Bottom EKE", legend = "", alpha = 0.35, data = data, var = "eke", group = "N_km2")

# Create layout
lay <- rbind(c(1,2),
             c(3,4),
             c(5,6),
             c(7,8),
             c(9,10),
             c(11,12),
             c(13,14),
             c(15,16))

# Plot using grid.arrange
p <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16,
             layout_matrix = lay)


#' Those variables in which the blue and the red curves are very similar
#' may not be good predictors of the response variable.

# Aspect to improve: for categorical variables use box plots instead of these curves as these won't provide you
# much information as they are planned for continuous variables. 

#Save plot
output_dir <- file.path(output_data, paste0("prefitting_checks/", genus))
setwd(output_dir)
jpeg(file = "DensityPlots.jpeg", 
     width = 25, height = 35, units = "cm", res = 300)
grid.draw(p)
dev.off()

