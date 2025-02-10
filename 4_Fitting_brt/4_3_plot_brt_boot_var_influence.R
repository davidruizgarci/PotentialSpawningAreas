# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------#---------------------------------------------------------------------------------------------------
# 4.3. Plot BRT variable relative incluence using bootstrap
#---------------------------------------------------------------------------------------------------
library(gbm)
library(data.table)
library(dplyr)
library(ggplot2)
library(egg)


# 1. Presence Raja--------------------------------------------------------------
genus <- "Raja" 
family <- "bernuilli_Final2"
type <- "_PA"
mod_code <- "brt"


# Set data repository

indir <- paste(output_data, mod_code, paste0(genus, type, "_", family), sep="/")

outdir <- paste(indir, "predict_boost", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# import model full model
mod <- readRDS(paste0(indir, "/", genus, "_Nkm2.rds"))

# list of bootstrap models
outdir_bootstrap <- paste0(indir, "/bootstrap/", genus, type, "_", family)
boots_files <- list.files(outdir_bootstrap, full.names = T)

# batch import of bootstrap models
models <- lapply(boots_files, readRDS)

# create empty list to store data
data_list <- list()

for(i in 1:length(models)){
  # get model
  #i=1
    mi <- models[[i]]
  #print(summary(mi))

  # get relative importance for variables
  df <- data.frame(boot = i, var = summary(mi)$var, rel.inf = summary(mi)$rel.inf)
  
  # append data
  data_list[[i]] <- df
}

# combine all data
data <- rbindlist(data_list)

# calculate median and CI per variable
data <- data %>%
  dplyr::group_by(var) %>%
  dplyr::summarize(median = median(rel.inf),
                   cil = quantile(rel.inf, prob = 0.025),
                   ciu = quantile(rel.inf, prob = 0.975)) %>%
  arrange(median)

# plot
# reorder factors for plot in descending order
data$var <- factor(data$var, levels = data$var)

# plot:
p_skates <- ggplot(data=data, mapping=aes(x=var, y=median, ymin=cil, ymax=ciu)) + 
  geom_pointrange(col="steelblue") +
  coord_flip() +
  ylab("Relative influence (%)") + xlab("") +
  theme_article(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(linewidth =.1, color="grey50"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0))
  )

p_skates


# export plot
#p_png <- paste0(outdir, "/", genus, "_", mod_code, "_", family, "_var_influence_boot_c.png")
#ggsave(p_png, p_skates, width=14, height=12, units="cm", dpi=300)






# 2. Presence Scyliorhinus------------------------------------------------------
genus <- "Scyliorhinus" 
family <- "bernuilli_Final2" 
type <- "_PA" 
mod_code <- "brt"



# 1. Set data repository

indir <- paste(output_data, mod_code, paste0(genus, type, "_", family), sep="/")

outdir <- paste(indir, "predict_boost", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# import model full model
mod <- readRDS(paste0(indir, "/", genus, "_Nkm2.rds"))

# list of bootstrap models
outdir_bootstrap <- paste0(indir, "/bootstrap/", genus, type, "_", family)
boots_files <- list.files(outdir_bootstrap, full.names = T)

# batch import of bootstrap models
models <- lapply(boots_files, readRDS)

# create empty list to store data
data_list <- list()

for(i in 1:length(models)){
  # get model
  #i=1
  mi <- models[[i]]
  #print(summary(mi))
  
  # get relative importance for variables
  df <- data.frame(boot = i, var = summary(mi)$var, rel.inf = summary(mi)$rel.inf)
  
  # append data
  data_list[[i]] <- df
}

# combine all data
data2 <- rbindlist(data_list)

# calculate median and CI per variable
data2 <- data2 %>%
  dplyr::group_by(var) %>%
  dplyr::summarize(median = median(rel.inf),
                   cil = quantile(rel.inf, prob = 0.025),
                   ciu = quantile(rel.inf, prob = 0.975)) %>%
  arrange(median)

# plot
# reorder factors for plot in descending order
data2$var <- factor(data2$var, levels = data2$var)

# plot:
p_catsharks <- ggplot(data=data2, mapping=aes(x=var, y=median, ymin=cil, ymax=ciu)) + 
  geom_pointrange(col="steelblue") +
  coord_flip() +
  ylab("Relative influence (%)") + xlab("") +
  theme_article(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(linewidth =.1, color="grey50"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0))
  )

p_catsharks


# 3. Combine both ------------------------------------------------------------------
# Assuming 'data' is for skates and 'data2' is for catsharks
data_combined <- rbind(
  data %>% mutate(species = "Skates"),
  data2 %>% mutate(species = "Catsharks")
)

# Create the combined plot
p_combined <- ggplot(data=data_combined, 
                     mapping=aes(x=var, y=median, ymin=cil, ymax=ciu, color=species)) + 
  geom_pointrange(position = position_dodge(width=0.5)) + # dodge to avoid overlap
  coord_flip() +
  scale_color_manual(values = c("Skates" = "steelblue", "Catsharks" = "orange")) + # Green for skates, blue for catsharks
  ylab("Relative influence (%)") + xlab("") +
  theme_article(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(linewidth = .1, color="grey50"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
    legend.position = "right" # Optional: Add legend for species
  )

# Print the combined plot
p_combined


# export plot
p_png <- paste0(outdir, "/combined_", mod_code, "_", family, "_var_influence_boot_c.png")
ggsave(p_png, p_combined, width=17, height=13, units="cm", dpi=300)




# 4. Density Raja--------------------------------------------------------------
genus <- "Raja" 
family <- "LN_gaussian_Final2" 
type <- "_NKm2" 
mod_code <- "brt"



# 1. Set data repository

indir <- paste(output_data, mod_code, paste0(genus, type, "_", family), sep="/")

outdir <- paste(indir, "predict_boost", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# import model full model
mod <- readRDS(paste0(indir, "/", genus, "_Nkm2.rds"))

# list of bootstrap models
outdir_bootstrap <- paste0(indir, "/bootstrap/", genus, type, "_", family)
boots_files <- list.files(outdir_bootstrap, full.names = T)

# batch import of bootstrap models
models <- lapply(boots_files, readRDS)

# create empty list to store data
data_list <- list()

for(i in 1:length(models)){
  # get model
  #i=1
  mi <- models[[i]]
  #print(summary(mi))
  
  # get relative importance for variables
  df <- data.frame(boot = i, var = summary(mi)$var, rel.inf = summary(mi)$rel.inf)
  
  # append data
  data_list[[i]] <- df
}

# combine all data
data3 <- rbindlist(data_list)

# calculate median and CI per variable
data3 <- data3 %>%
  dplyr::group_by(var) %>%
  dplyr::summarize(median = median(rel.inf),
                   cil = quantile(rel.inf, prob = 0.025),
                   ciu = quantile(rel.inf, prob = 0.975)) %>%
  arrange(median)

# plot
# reorder factors for plot in descending order
data3$var <- factor(data3$var, levels = data$var)

# plot:
p_skates_den <- ggplot(data=data3, mapping=aes(x=var, y=median, ymin=cil, ymax=ciu)) + 
  geom_pointrange(col="steelblue") +
  coord_flip() +
  ylab("Relative influence (%)") + xlab("") +
  theme_article(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(linewidth =.1, color="grey50"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0))
  )

p_skates_den


# export plot
#p_png <- paste0(outdir, "/", genus, "_", mod_code, "_", family, "_var_influence_boot_c.png")
#ggsave(p_png, p_skates_den, width=14, height=12, units="cm", dpi=300)




# 5. Repeat for Scyliorhinus----------------------------------------------------

# Density Scyliorhinus
genus <- "Scyliorhinus" 
family <- "gaussian_Final2" 
type <- "_PA" 
mod_code <- "brt"


# 1. Set data repository

indir <- paste(output_data, mod_code, paste0(genus, type, "_", family), sep="/")

outdir <- paste(indir, "predict_boost", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# import model full model
mod <- readRDS(paste0(indir, "/", genus, "_Nkm2.rds"))

# list of bootstrap models
outdir_bootstrap <- paste0(indir, "/bootstrap/", genus, type, "_", family)
boots_files <- list.files(outdir_bootstrap, full.names = T)

# batch import of bootstrap models
models <- lapply(boots_files, readRDS)

# create empty list to store data
data_list <- list()

for(i in 1:length(models)){
  # get model
  #i=1
  mi <- models[[i]]
  #print(summary(mi))
  
  # get relative importance for variables
  df <- data.frame(boot = i, var = summary(mi)$var, rel.inf = summary(mi)$rel.inf)
  
  # append data
  data_list[[i]] <- df
}

# combine all data
data4 <- rbindlist(data_list)

# calculate median and CI per variable
data4 <- data4 %>%
  dplyr::group_by(var) %>%
  dplyr::summarize(median = median(rel.inf),
                   cil = quantile(rel.inf, prob = 0.025),
                   ciu = quantile(rel.inf, prob = 0.975)) %>%
  arrange(median)

# plot
# reorder factors for plot in descending order
data4$var <- factor(data4$var, levels = data4$var)

# plot:
p_catsharks_den <- ggplot(data=data4, mapping=aes(x=var, y=median, ymin=cil, ymax=ciu)) + 
  geom_pointrange(col="steelblue") +
  coord_flip() +
  ylab("Relative influence (%)") + xlab("") +
  theme_article(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(linewidth =.1, color="grey50"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0))
  )

p_catsharks_den


# 6. Combine both ------------------------------------------------------------------
# Assuming 'data' is for skates and 'data2' is for catsharks
data_combined <- rbind(
  data %>% mutate(species = "Skates_PA"),
  data2 %>% mutate(species = "Catsharks_PA"),
  data3 %>% mutate(species = "Skates_den"),
  data4 %>% mutate(species = "Catsharks_den")
)

# Reorder the 'var' column in data_combined
data_combined$var <- factor(data_combined$var, levels = rev(c(
  "ln_fishingEffort", "depth", "slope", "bottom_so", "bottom_temp"
)))

# Create the combined plot
p_combined <- ggplot(data=data_combined, 
                     mapping=aes(x=var, y=median, ymin=cil, ymax=ciu, color=species)) + 
  geom_pointrange(position = position_dodge(width=0.5)) + # dodge to avoid overlap
  coord_flip() +
  scale_color_manual(values = 
                       c("Skates_PA" = "#4cb8cf", "Catsharks_PA" = "#F9B233",
                         "Skates_den" = "#3492a3", "Catsharks_den" = "#C98F29")) + #"#3fbf8c", "#f28d7c"
  ylab("Relative influence (%)") + xlab("") +
  theme_article(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(linewidth = .1, color="grey50"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
    legend.position = "right" # Optional: Add legend for species
  )

# Print the combined plot
p_combined


# export plot
p_png <- paste0(outdir, "/combined_", mod_code, "_", family, "_var_influence_boot_col.png")
ggsave(p_png, p_combined, width=17, height=20, units="cm", dpi=300)

