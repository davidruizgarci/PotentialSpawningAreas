# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 4.2.1. Fit Boosted Regression Tree model for density data (N/km2) using laplace distribution
#-------------------------------------------------------------------------------

library(doParallel)
library(ggBRT)
library(lubridate)
library(data.table)
library(egg)
library(fmsb)
library(dplyr)

genus <- "Raja" #"Raja" #"Scyliorhinus"
family <- "LN_POISSON_Final" #bernuilli #LN_laplace_sinO2
type <- "_NKm2" #"_NKm2" "_PA" "only_P
mod_code <- "brt"
dataset <- "ALL2" #ALL, train

#Load data
file <- paste0(temp_data, "/folds_dataset/", genus, "_", dataset, "_folds_dataset.csv")
data <- read.csv2(file)

summary(data)
str(data)
head(data)

#1. Select only density data ---------------------------------------------------
#Chose response variable distribution: 
hist(data$N_km2)
shapiro.test(data$N_km2)

hist(data$N)
shapiro.test(data$N)

#transform response variable:
data$ln_N_km2 <- log1p(data$N_km2)
hist(data$ln_N_km2)
shapiro.test(data$ln_N_km2)
summary(data$ln_N_km2)

#transform response variable:
data$ln_N <- log1p(data$N)
hist(data$ln_N)
shapiro.test(data$ln_N)
summary(data$ln_N)


# Convert the 'time' column to Date format if needed 
data$date <- as.Date(data$date) #, format = "%Y-%m-%d"
data$date_time <- as.POSIXct(data$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Create a season column:
data$season <- case_when(
  (month(data$date) == 12 & day(data$date) >= 21) | (month(data$date) %in% c(1, 2)) | 
    (month(data$date) == 3 & day(data$date) < 21) ~ 1,  # Winter: Dec 21 - Mar 20
  
  (month(data$date) == 3 & day(data$date) >= 21) | (month(data$date) %in% c(4, 5)) | 
    (month(data$date) == 6 & day(data$date) < 21) ~ 2,  # Spring: Mar 21 - Jun 20
  
  (month(data$date) == 6 & day(data$date) >= 21) | (month(data$date) %in% c(7, 8)) | 
    (month(data$date) == 9 & day(data$date) < 21) ~ 3,  # Summer: Jun 21 - Sep 20
  
  (month(data$date) == 9 & day(data$date) >= 21) | (month(data$date) %in% c(10, 11)) | 
    (month(data$date) == 12 & day(data$date) < 21) ~ 4 )  # Autumn: Sep 21 - Dec 20



#2. Organise dataset -----------------------------------------------------------
# Change the name of some variables as you want them to appear in the figure for the paper:
names(data)
colnames(data) <- c("Vessel", "code", "Genus", "lat", "lon", "season", "depth", 
                    "swept_area_km2", "N", "N_km2", "presence_absence", "date", 
                    "date_time", "bathy", "substrate", "slope", "roughness", 
                    "fishingEffort", "distCanyons", "distMounts", "distFans", 
                    "bottom_temp","bottom_oxygen", 
                    "bottom_nppv", "bottom_ph", "bottom_nh4", "bottom_no3", 
                    "bottom_po4", "bottom_so", "bottom_uo", "bottom_vo", 
                    "bottom_eke", "SD_bottomT", "SD_o2", "SubAll", "bioSubsFinal", 
                    "ln_slope", "ln_fishingEffort", "Haul_N", "RN", "id", "fold", 
                    "ln_N_km2", "ln_N") #, "BioSubs", "SA_offset"

#Set categorical predictors as categories:
data <- data %>% 
  mutate(season = factor(season, c(1:4)),
         substrate = factor(substrate),
         fold = factor(data$fold),
         BioSubs = factor(bioSubsFinal)) #Haul_N = factor(data$Haul_N),


summary(data)
str(data)
names(data)


# List the name of the predictor variables
vars  <- c("depth", "slope", "ln_fishingEffort", "substrate", #"distCanyons", "SD_bottomT", #"SD_o2", "bottom_oxygen",
           "bottom_eke", "bottom_so",  "RN") 

# "distMounts","distCanyons", "distFans", "bottom_nppv",
# "bottom_nh4", ,"bottom_eke","bottom_ph", "distMounts","oxygen_sat_percent",
# "season", "substrate", "bottom_po4",  "bottom_oxygen", "SD_bottomT", "SD_o2", 


#3. Prepare model parameters----------------------------------------------------

# Define number of trees
ini.nt = 50
max.nt = 10000
step.nt = 50 #des pas de 50
tree.list <- seq(ini.nt,max.nt,by=step.nt) #list of trees for evaluation

# Define combination of hyper-parameters
comb <- expand.grid(lr=c(0.001, 0.005, 0.01, 0.05), tc=c(1,3,5), bf=c(0.5, 0.6, 0.7)) #combination
comb <- expand.grid(lr=c(0.0005, 0.001, 0.005, 0.01), tc=c(3,5,6), bf=c(0.3, 0.4, 0.5)) #combination



## Prepare clusters
cores <-detectCores()
cores #if you use all of them you, your computer may crash (consumes all the CPU).
cores <- 8  
cl <- makeCluster(cores)
registerDoParallel(cl)



#3. Build presence_absence model -----------------------------------------------

#  Create output data repository
outdir <- paste0(output_data, "/", mod_code, "/", genus, type, "_", family)
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# Check the variables are correct in format:
set.seed(131)
names(data)
str(data)

# Add offset if you are using poisson family, i.e., N + offset
data$SA_offset <- log(data$swept_area_km2)
summary(data$SA_offset)

# Check the distribution of your response variable:
# Summary statistics for ln_N_km2
#summary(data$ln_N_km2)

# Calculate skewness and kurtosis
#library(moments)
#skewness_value <- skewness(data$ln_N_km2)
#kurtosis_value <- kurtosis(data$ln_N_km2)

# Print skewness and kurtosis
#cat("Skewness:", skewness_value, "\n") 
#> 0.5 or < -0.5 and kurtosis_value is high, choose "laplace" for robustness.
#is near 0 and the histogram is bell-shaped with few outliers, go with "gaussian".

#cat("Kurtosis:", kurtosis_value, "\n")
# Histogram with density plot
#hist(data$ln_N_km2, breaks = 30, main = "Histogram of ln_N_km2", col = "lightblue", xlab = "ln_N_km2")
#lines(density(data$ln_N_km2), col = "red", lwd = 2)

# Boxplot for outlier detection
#boxplot(data$ln_N_km2, main = "Boxplot of ln_N_km2", col = "lightgreen")
# Calculate IQR and bounds for outlier detection
#quantiles <- quantile(data$ln_N_km2, probs = c(0.25, 0.75))
#IQR_value <- IQR(data$ln_N_km2)
#lower_bound <- quantiles[1] - 1.5 * IQR_value
#upper_bound <- quantiles[2] + 1.5 * IQR_value

# Identify outliers
#outliers <- data$ln_N_km2[data$ln_N_km2 < lower_bound | data$ln_N_km2 > upper_bound]

# Print the number of outliers
#cat("Number of outliers:", length(outliers), "\n")


all_list <- foreach(i=1:nrow(comb), .packages=c("dismo", "gbm", "dplyr")) %dopar% {
  
  # Fit model
  # Uses a block cross-validation
  # faster learning rate means larger values
  mod <- dismo::gbm.step(data = data,             # data.frame with data
                    gbm.x = vars,          # predictor variables
                    #gbm.y = "ln_N_km2",            # response variable
                    #family = "laplace",  # the nature of error structure
                    gbm.y = "N",            # response variable
                    family = "poisson",  # the nature of error structure
                    offset = data$SA_offset,
                    tree.complexity = comb$tc[i],   # tree complexity
                    learning.rate = comb$lr[i],  # learning rate
                    bag.fraction = comb$bf[i],    # bag fraction
                    fold.vector = data$fold,
                    n.folds = length(unique(data$fold)),
                    n.trees = ini.nt, 
                    step.size = step.nt, 
                    max.trees = max.nt)   

  if(!is.null(mod)) {
    # Keep CV parameters
    mod_out <- data.frame(
      tree.complexity = mod$interaction.depth,
      learning.rate = mod$shrinkage,
      bag.fraction = mod$bag.fraction,
      n.trees = mod$n.trees,
      AUC = mod$self.statistics$discrimination,
      cv.AUC = mod$cv.statistics$discrimination.mean,
      deviance = mod$self.statistics$mean.resid,
      cv.deviance = mod$cv.statistics$deviance.mean,
      PER = (1-mod$self.statistics$mean.resid/mod$self.statistics$mean.null)*100,
      cv.PER = (1-mod$cv.statistics$deviance.mean/mod$self.statistics$mean.null)*100) 
    
    # keep deviance values for all trees
    cv_deviance <- mod$cv.values
    cv_deviance <- c(cv_deviance, rep(NA, length(tree.list) - length(cv_deviance)))  #fill with NA
    
    # selected variables
    pred_order <- summary(mod)$var
    rn_position <- which(pred_order == "RN")
    pred_list <- as.character(pred_order[1:(rn_position-1)])
    
    
    list(mod_out = mod_out, cv_deviance = cv_deviance, pred_list = pred_list)
  } else {
    # Return NULL or an empty list if mod is NULL
    list(mod_out = NULL, cv_deviance = NULL, pred_list = NULL)
  }
}




## combine model outputs
mod_list <- foreach(i=1:nrow(comb)) %dopar% all_list[[i]]$mod_out
mod_list[!lengths(mod_list)] <-  list(data.frame(tree.complexity = NA))
mod_out <- rbindlist(mod_list, fill = TRUE)
mod_out <- bind_cols(comb,
                     dplyr::select(mod_out, -c(tree.complexity, learning.rate, bag.fraction))) %>%
  dplyr::mutate(id = 1:n())

## combine deviance outputs
deviance_list <- list()
for(i in 1:nrow(mod_out)){
  # extract deviance data
  dev <- all_list[[i]]$cv_deviance
  
  # check that there is no null data
  if(is.null(dev)) dev <- rep(NA,length(tree.list))
  
  # make data.frame with number of trees
  df <- data.frame(id=mod_out$id[i],lr = mod_out$lr[i], tc = mod_out$tc[i], bf = mod_out$bf[i], ntrees = tree.list, cv_deviance = dev)
  deviance_list[[i]] <- df
}
cv_deviance <- rbindlist(deviance_list)

## get selected variables
predict_list <- foreach(i=1:nrow(comb)) %dopar% all_list[[i]]$pred_list

## stop clusters
stopCluster(cl)

## plot profiles
p <- ggplot(data = cv_deviance) +
  geom_line(data = dplyr::rename(cv_deviance, comb = id), aes(x = ntrees, y = cv_deviance, group = comb), color = "grey80") +
  geom_line(aes(x = ntrees, y = cv_deviance, group = id), color = "firebrick3") +
  scale_x_continuous(limits = c(0, max(cv_deviance$ntrees[!is.na(cv_deviance$cv_deviance)]))) +
  facet_wrap(id ~.,) +
  theme_article()

p

#  Create output repository
outfile <- paste0(outdir, "/", genus, "_", mod_code, "_", "optim_params", type, "_", family, ".png")
ggsave(outfile, p, width=25, height=14, units="cm", dpi=300)

## export outputs
outfile <- paste0(outdir, "/brt_optim_params", type, "_", family,".csv")
write.csv2(mod_out, outfile, row.names = FALSE)

outfile <- paste0(outdir, "/brt_cv_deviance", type, "_", family,".csv")
write.csv(cv_deviance, outfile, row.names = FALSE)

outfile <- paste0(outdir, "/brt_predlist", type, "_", family,".rds")
saveRDS(predict_list, outfile)





# 4. Fit full density model-------------------------------------------
# genus <- "Scyliorhinus" #"Raja" #"Scyliorhinus"
# family <- "LN_gaussian"
# type <- "_NKm2" #"_NKm2" "_PA"
# mod_code <- "brt"

#Open the created dataset containing the models information and select the best model.
outdir <- paste0(output_data, "/", mod_code, "/", genus, type, "_", family)

mod_out <- read.csv2(paste0(outdir, "/brt_optim_params", type, "_", family,".csv"))
predict_list <- readRDS(paste0(outdir, "/brt_predlist", type, "_", family,".rds"))

View(mod_out)
View(predict_list)
plot(p)

#Save as excel to build the table for the paper:
#library(openxlsx)
#outfile <- paste0(outdir, "/brt_optim_params.xlsx")
#write.xlsx(mod_out, outfile, rowNames = FALSE)

#' selection is based on parameters, criteria and checking curves (see appendix 1 for Elith et al. 2008 recommendations)
#' But as summary:
#' 1) The model with the lowest cv_deviance which n.trees is >1000
#' 2) Then, if there are two or more very similar: the one with the largest nt, and lowest lr and tc.

select_model_id <- 29 #scyliorhinus 27 (7) # raja 30 (29)


#List the name of the predictor variables
#vars  <- c("depth", "slope", "ln_fishingEffort", "bottom_temp",
#           "substrate", "bottom_eke", "bottom_so",  "RN") 

#str(data)

tc <- mod_out$tc[select_model_id]
lr <- mod_out$lr[select_model_id]
bf <- mod_out$bf[select_model_id]
ntrees <- mod_out$n.trees[select_model_id]
pred_list <- vars[vars %in% predict_list[[select_model_id]]]

# off set:
mod_full <- dismo::gbm.step(data = data,             # data.frame with data
                       gbm.x = pred_list,          # predictor variables
                       gbm.y = "N",            # response variable
                       family = "poisson",  # the nature of error structure
                       offset = data$SA_offset,
                       tree.complexity = tc,   # tree complexity
                       learning.rate = lr,  # learning rate
                       bag.fraction = bf,    # bag fraction
                       fold.vector = data$fold,
                       n.folds = length(unique(data$fold)),
                       n.trees = ini.nt, 
                       step.size = step.nt, 
                       max.trees = max.nt)   
mod_full$n.trees

#mod_full <- gbm::gbm(N ~ slope + depth + ln_fishingEffort + offset(SA_offset), 
#                     data = data, 
#                     distribution = "poisson", 
#                     n.trees = ntrees, 
#                    interaction.depth = tc, 
#                     shrinkage = lr, 
#                     bag.fraction = bf)
#summary(mod_full)


#No offset:
# fir BRT with selected parameters
mod_full <- dismo::gbm.fixed(data = data,             # data.frame with data
                             gbm.x = pred_list,          # predictor variables
                             gbm.y = "ln_N_km2",             # response variable
                             family = "laplace",  # the nature of errror structure
                             tree.complexity = tc,   # tree complexity
                             learning.rate = lr,  # learning rate
                             bag.fraction = bf,    # bag fraction
                             n.trees = ntrees) 
summary(mod_full)

# Save model
saveRDS(mod_full, file = paste0(outdir, "/", genus, "_Nkm2.rds"))  # save model
mod_full <- readRDS(paste0(outdir, "/", genus, "_Nkm2.rds"))






# 5. Make variable influence plots ---------------------------------------------  

# 5.1. Make radarPlot
# Define function for radar plot explaning the variable contribution from models:
radarPlot <- function(var_imp, var_order, colors_border=rgb(0.2,0.5,0.5,0.9), colors_in=rgb(0.2,0.5,0.5,0.4)){
  
  # set parameters for plot
  min_val <- 0
  max_val <- ceiling(max(var_imp))#100
  nseg <- 4
  
  # prepare data.frame
  var_imp <- dplyr::select(data.frame(t(var_imp)), var_order)
  data <- rbind(rep(max_val, length(var_imp)) , rep(min_val, length(var_imp)), var_imp)
  data <- data.frame(data)
  row.names(data) <- c("max", "min", "MaxEnt")
  
  radarchart(data  , axistype=1 , 
             #custom polygon
             pcol=colors_border , pfcol=colors_in , plwd=3 , plty=1,
             # number of segments
             seg=nseg,
             #custom the grid
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=round(seq(min_val,max_val,max_val/nseg),1), cglwd=0.8,
             #custom labels
             vlcex=1)
}

# Plot variable contribution using radar plot
var_imp <- summary(mod_full)$rel.inf
names(var_imp) <- summary(mod_full)$var
asc <- names(var_imp)
pngfile <- paste0(outdir, "/", genus, "_", mod_code, "_var_radar", type, "_", family, ".png")
png(pngfile, width=1500, height=1000, res=150)
radarPlot(var_imp, var_order=asc)
dev.off()

# 5.2. Make bar plot
# Plot variable contribution using bar plot
pngfile <- paste0(outdir, "/", genus, "_", mod_code, "_var_influence", type, "_", family, ".png")
png(pngfile, width=1000, height=1000, res=150)
ggBRT::ggInfluence(mod_full, show.signif = F, col.bar = "skyblue3")
dev.off()

# 5.3. Make response curve plot
# Plot response curves
pngfile <- paste0(outdir, "/", genus, "_", mod_code, "_response", type, "_", family, ".png")
png(pngfile, width=1000, height=2000, res=200)
names(mod_full$gbm.call)[1] <- "dataframe"
ggBRT::ggPD(mod_full, n.plots =13, smooth = F, rug = F, ncol=2, col.line = "skyblue3")
dev.off()






# 6. Boosted Regression Tree - Interactions-------------------------------------

#* Check whether there are important interactions among variables.
#* for that purpose you assess the magnitude of 2nd order interaction effects in gbm models fitted with interaction depths greater than 1.
names(mod_full$gbm.call)[1] <- "dataframe"

find.int <- dismo::gbm.interactions(mod_full)
find.int$interactions

#See the list of potential interactions:
find.int$rank.list

outdir_interaction <- paste0(outdir, "/interactions", type, "_", family)
if (!dir.exists(outdir_interaction)) dir.create(outdir_interaction, recursive = TRUE)

#Assess the level of interaction between the potential interactions:
#*A higher value (close to 1) suggests a stronger and potentially significant interaction between 
#*the two variables. A lower value (close to 0) would indicate a less significant interaction.

#*# Set the angle for the 3D plot
theta <- 140  # Adjust the azimuthal angle as desired
phi <- 20    # Adjust the polar angle as desired

#Plot:
pngfile <- paste0(outdir_interaction, "/", genus, "_", mod_code, "_interaction_1_Nkm2.png")
png(pngfile, width=1500, height=1500, res=200)
dismo::gbm.perspec(mod_full, 1, 3, theta = theta, phi = phi, smooth = 0.5)
dev.off()

#*# Set the angle for the 3D plot
theta <- 120  # Adjust the azimuthal angle as desired
phi <- 40    # Adjust the polar angle as desired

#Check how the correlation between the 2 variables occur in 3D (x=var1, z=var2, y=respuesta)
pngfile <- paste0(outdir_interaction, "/", genus, "_", mod_code, "_interaction_2_Nkm2.png")
png(pngfile, width=1500, height=1500, res=200)
dismo::gbm.perspec(mod_full, 5, 1, theta = theta, phi = phi, smooth = 0.5)
dev.off()

#dismo::gbm.perspec(mod_full, 7, 2)
#dismo::gbm.perspec(mod_full, 4, 2)






# 7. Boosted Regression Tree - Predict (Bootstrap approach) --------------------
# For each  model, we fit the model 50 times (n.boot <- 50).
# For each of the 50 iterations, we used the parameter values chosen for the final model, but we sampled
# half the data (with replacement) to fit the model. (Hindell et al. 2020):
# idata <- stratified(data, c("Alive_Dead", "fold"), 0.5, replace = TRUE) 
# The variable you put here are: your response variable "Alive_dead", and the fold variable "N_Haul" in my case.

# Set output directory
# Each bootstrap model is stored here
outdir_bootstrap <- paste0(outdir, "/bootstrap/", genus, type, "_", family)
if (!dir.exists(outdir_bootstrap)) dir.create(outdir_bootstrap, recursive = TRUE)

# Define number of bootstrap models
n.boot <- 100  # number of model fits

## Prepare clusters
cores <- 8
cl <- makeCluster(cores)
registerDoParallel(cl)

foreach(i=1:n.boot, .packages=c("dismo", "gbm", "dplyr", "splitstackshape", "stringr"), .combine = "c") %dopar% {
  
  #library("splitstackshape")
  #library("stringr")
  # sampled half the data (with replacement) to fit the model (Hindell et al. 2020)
  idata <- stratified(data, c("presence_absence"), 0.8, replace = TRUE)
  
  # fit BRT
  mod_boot <- dismo::gbm.fixed(data = idata,              # data.frame with data
                              gbm.x = pred_list,         # predictor variables
                              gbm.y = "ln_N_km2",        # response variable
                              family = "laplace",        # error distribution
                              tree.complexity = tc,      # tree complexity
                              learning.rate = lr,        # learning rate
                              bag.fraction = bf,         # bag fraction
                              n.trees = ntrees)              # cross-validation folds
  
  # store model
  outfile <- paste0(outdir_bootstrap, "/", str_pad(i, 2, pad = "0"), "_", genus, "_", mod_code, "_boot_Nkm2.rds")
  saveRDS(mod_boot, file = outfile)  # save model
  
  # Return something here if needed
  return(mod_boot)
}

## stop clusters
stopCluster(cl)




# 8. Check model traits --------------------------------------------------------
# 8.1. OVERDIPSERION
# Calculate predicted values
pred_values <- predict(mod_full, newdata = data, type = "response")

# Ensure there are no zero or negative values in predictions to avoid log issues
pred_values[pred_values <= 0] <- 1e-10  # Small positive value to replace zero

# Compute residuals
observed_values <- data$N
residuals <- observed_values - pred_values

# Calculate deviance residuals for Poisson
deviance_residuals <- 2 * (observed_values * log(observed_values / pred_values) - (observed_values - pred_values))
deviance_residuals[is.nan(deviance_residuals)] <- 0  # Handle NaNs from log(0)

# Total deviance (sum of all deviance residuals)
deviance <- sum(deviance_residuals, na.rm = TRUE)

# Calculate residual degrees of freedom (approximation)
df_residual <- nrow(data) - length(mod_full$var.names)

# Deviance per degree of freedom (to check overdispersion)
deviance_per_df <- deviance / df_residual

# Print result
print(paste("Deviance per degree of freedom:", round(deviance_per_df, 2)))
if (deviance_per_df > 1) {
  print("The model suggests overdispersion.")
} else {
  print("The model does not suggest overdispersion.")
}




# 8.2. RESIDUAL CHECK 
# Generate predicted values using the model
predicted_values <- predict(mod_full, newdata = data, n.trees = mod_full$n.trees, type = "response")

# LAPLACE:
# Calculate residuals (observed - predicted)
residuals <- data$ln_N_km2 - predicted_values

# POISSON:
# Calculate residuals
# For Poisson model, calculate residuals (observed - predicted)
residuals_poisson <- data$N - predicted_values

# Create a residual plot
ggplot(data.frame(predicted = predicted_values, residuals = residuals), aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Predicted Values", x = "Predicted Values", y = "Residuals") +
  theme_minimal() #+
  #ylim(NA, 100)  # Set the upper limit of the x-axis to 25

#Residuals vs. Predicted Values Plot:
#A well-behaved model will have residuals that appear randomly scattered around zero. If you see systematic patterns, this could indicate model misspecification or the presence of heteroscedasticity.

# Histogram of residuals
ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()


# Optional: Add a density plot overlay
ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Residuals", x = "Residuals", y = "Density") +
  theme_minimal()
  #xlim(NA, 6)  # Set the upper limit of the x-axis to 25

#Distribution of Residuals:
#Ideally, residuals should be symmetric and centered around zero. A significant skew or a distribution with extreme outliers may suggest issues with model fit or data preprocessing needs.

# Generate a Q-Q plot for the residuals
qqnorm(residuals)
qqline(residuals, col = "red")  # Add a reference line for better visualization

# Q-Q plot of deviance residuals against the Poisson distribution
qqplot(residuals_poisson, qpois, lambda = mean(residuals_poisson), 
       main = "Q-Q Plot of Poisson Residuals", 
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
abline(a = 0, b = 1, col = "red")  # Reference line

# Generate a Q-Q plot against a Laplace distribution
library(VGAM) 
#residuals <- residuals[residuals != max(residuals)]
qqplot(qlaplace(ppoints(length(residuals))), 
       residuals,
       main = "Q-Q Plot of Residuals Against Laplace Distribution",
       xlab = "Theoretical Quantiles (Laplace)",
       ylab = "Sample Quantiles (Residuals)")
abline(0, 1, col = "red")  # Add a 45-degree reference line
#If the points fall along the 45-degree line, your residuals align well with a Laplace distribution.
#Deviations from the line indicate that the residuals might not perfectly follow a Laplace distribution, suggesting potential model or data issues.




# 9. TSS and RMSE calculation----------------------------------------------------
# ONLY IF YOU USE A TRAINING AND TESTING DATASET- NOT DONE IN OUR CASE
#Load test data
file <- paste0(temp_data, "/train_test/", genus, "_training_testing/", genus, "_test_dataset.csv")
test_data <- read.csv2(file)
summary(test_data)

#Chose response variable distribution: 
hist(test_data$N_km2)
shapiro.test(test_data$N_km2)

#transform response variable:
test_data$ln_N_km2 <- log1p(test_data$N_km2)
hist(test_data$ln_N_km2)
shapiro.test(test_data$ln_N_km2)
summary(test_data$ln_N_km2)
#data$e_N_km2 <- exp(data$ln_N_km2) - 1

# Change the name of some variables as you want them to appear in the figure for the paper:
names(test_data)
colnames(test_data) <- c("code", "Genus", "lat", "lon", "season", "depth", 
                    "swept_area_km2", "N", "N_km2", "presence_absence", "date", 
                    "date_time", "bathy", "substrate", "slope", "roughness", 
                    "fishingEffort", "distCanyons", "distMounts", "distFans", 
                    "bottom_temp","bottom_oxygen", 
                    "bottom_nppv", "bottom_ph", "bottom_nh4", "bottom_no3", 
                    "bottom_po4", "bottom_so", "bottom_uo", "bottom_vo", 
                    "bottom_eke", "SD_bottomT", "SD_o2",
                    "ln_slope", "ln_fishingEffort", "Vessel", "RN", "id", "ln_N_km2")


summary(test_data)
str(test_data)

test_predictions <- predict(mod_full, newdata = test_data, n.trees = mod_full$n.trees)

# 9.1. R-squared----------------------------------------------------------------
library(caret)
r_squared <- R2(test_predictions, test_data$ln_N_km2)
print(paste("R-squared: ", r_squared)) 

# 9.2. Calculate RMSE-----------------------------------------------------------
rmse <- sqrt(mean((test_data$ln_N_km2 - test_predictions)^2))
print(rmse) 

# 9.3. True Skill Statistic (TSS (only for PA) ----------------------------------------------------------------
# Convert probabilities to binary outcomes (assuming a threshold of 0.5)
predicted_class <- ifelse(test_predictions > 0.5, 1, 0)
#  Extract the actual values (assuming the target variable is 'presence_absence')
y_true <- test_data$presence_absence

# Create confusion matrix
conf_matrix <- table(predicted_class, y_true)

# Extract TP, FN, FP, TN
TP <- conf_matrix[2, 2]  # True Positive
FN <- conf_matrix[1, 2]  # False Negative
FP <- conf_matrix[2, 1]  # False Positive
TN <- conf_matrix[1, 1]  # True Negative

# Calculate TSS
TSS <- (TP / (TP + FN)) - (FP / (FP + TN))

# Output the TSS value
TSS



# 10. Check Spatial autocorrelation----------------------------------------------
# To calculate Moran's I for spatial autocorrelation after fitting a model 
library(DHARMa)
resid <- resid(mod_full)
testSpatialAutocorrelation(resid, x = data$lon, y = data$lon, plot = FALSE) 

# Create variogram of residuals
library(gstat)
library(sp)

# Create spatial object from data (ensure 'data' has lat/lon as coordinates)
coordinates(data) <- ~lon+lat
vario <- variogram(resid ~ 1, data)

# Plot the variogram
p <- plot(vario)
p

# export plot
outdir_semi <- paste0(outdir, "/semivariograma", type, "_", family)
if (!dir.exists(outdir_semi)) dir.create(outdir_semi, recursive = TRUE)
pngfile <- paste0(outdir, "/", genus, "_semivariograma.png")
png(pngfile, width=1500, height=1500, res=300)
print(p)
dev.off()


