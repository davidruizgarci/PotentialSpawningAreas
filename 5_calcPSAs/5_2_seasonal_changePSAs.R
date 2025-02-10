# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 5.2. Calculate changes in PSAs across seasons
#-------------------------------------------------------------------------------

# Raja
genus <- "Raja" 
family <- "LN_gaussian_Final2" 
type <- "_NKm2" 
mod_code <- "brt"
dataset <- "ALL" 

# Scyliorhinus
genus <- "Scyliorhinus" 
family <- "bernuilli_Final2" 
type <- "_PA" 
mod_code <- "brt"
dataset <- "ALL" 


# 1. Load data-----------------------------------------------------------------
# Define seasons
seasons <- c("spring", "summer", "fall", "winter")

# Loop through each season
for (season in seasons) {
  # Construct the file path
  path <- paste0("output/fig/Map/", genus, type, "_", family, "/", season, ".csv")
  
  # Read the CSV and dynamically assign it to a dataframe named <season>_df
  assign(paste0(season, "_df"), read.csv2(path, sep = ";"))
}

# Now you can access them directly
head(spring_df)  # Dataframe for spring
head(summer_df)  # Dataframe for summer
head(fall_df)  # Dataframe for autumn
head(winter_df)  # Dataframe for winter


# 2. Statistically compare groups ----------------------------------------------
# Shapiro-Wilk normality test
shapiro_spring <- shapiro.test(spring_df$habitat)
shapiro_summer <- shapiro.test(summer_df$habitat)
shapiro_fall <- shapiro.test(fall_df$habitat)
shapiro_winter <- shapiro.test(winter_df$habitat)

# Print results
print(shapiro_spring)
print(shapiro_summer)
print(shapiro_fall)
print(shapiro_winter)

# Combine data into a single dataframe
seasonal_data <- rbind(
  data.frame(season = "Spring", habitat = spring_df$habitat),
  data.frame(season = "Summer", habitat = summer_df$habitat),
  data.frame(season = "Fall", habitat = fall_df$habitat),
  data.frame(season = "Winter", habitat = winter_df$habitat)
)
head(seasonal_data)

# Perform Kruskal-Wallis test
kruskal_test <- kruskal.test(habitat ~ season, data = seasonal_data)
print(kruskal_test)

# Pairwise Wilcoxon test with Bonferroni adjustment for multiple comparisons
pairwise_test <- pairwise.wilcox.test(seasonal_data$habitat, seasonal_data$season, p.adjust.method = "bonferroni")
print(pairwise_test)

# Boxplot for visual comparison
library(ggplot2)
ggplot(seasonal_data, aes(x = season, y = habitat, fill = season)) +
  geom_boxplot(outlier.color = "red", outlier.size = 1.5) +
  labs(title = "Habitat Distribution by Season", x = "Season", y = "Habitat") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# 3. Violin plot showing predicted CPUE for each season-------------------------
library(smplot2)
library(sdamr)
library(smplot2)
#available colors:
#"blue", "crimson", "green", "purple", "orange", "skyblue", "pink", "limegreen", "lightpurple", "brown", "red", "lightbrown", "asparagus", "viridian", "darkred", "lightblue","wine", "yellow", "lightgreen"

# log transformation
#seasonal_data <- seasonal_data %>%
#  mutate(habitat = log1p(habitat))

# Calculate the 90th percentile for each season
percentiles <- seasonal_data %>%
  group_by(season) %>%
  summarize(
    percentile_90 = quantile(habitat, 0.9, na.rm = TRUE)
  )


p1 <- ggplot(data = seasonal_data, aes(x = season, y = habitat)) +
  # Violin plot with transparency
  geom_violin(aes(fill = season), trim = FALSE, color = "transparent", alpha = 0.5) + 
  # Boxplot with solid color (same as violin color but no transparency)
  geom_boxplot(aes(fill = season), width = 0.1, outlier.shape = NA) + 
  #upper 90th percentile
  #geom_segment(data = percentiles, 
  #             aes(x = season, xend = season, y = 0, yend = percentile_90),
  #             color = "red", size = 1.5, linetype = "dashed") +  # Customize the line
  # Scale for fill colors
  scale_fill_manual(values = c(
    "Spring" = "lightgreen", 
    "Summer" = "orange", 
    "Fall" = "yellow", 
    "Winter" = "skyblue"
  )) +
  # Label for y-axis
  ylab("Habitat Value") +
  # Adjust y-axis position
  scale_y_continuous(position = "right") +
  # Theme adjustments
  theme(
    axis.title.x = element_blank(),
    # Uncomment if you need rotated y-axis labels
    # axis.text.y = element_text(angle = 90),
    axis.line.y = element_blank(),
    #axis.line.x = element_blank(),
    #axis.ticks.y = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

# Print the plot
print(p1)

# export plot
outdir <- paste0(output_data, "/fig/raincloud")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", genus, "_habitat.png")
ggsave(p_png, p1, width=17, height=10, units="cm", dpi=300)


