# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#-------------------------------------------------------------------------------
# 3.6. Create training and testing data sets:
#---------------------------------------------------------------------------------------------------
library(dplyr)


# It may not be interesting creating a training and testing if data is limited.
# Especially when you will check the model using cross-validation and bootstrap. 
# You can create and then decide whether you will use it or not. 

# IN OUR CASE WE DIDNT USE IT DUE TO DATA CONSTRAINTS.

# 1. Add Vessel data -----------------------------------------------------------
#Load data
genus <- "Scyliorhinus" 
file <- paste0(temp_data, "/data_subsets/", genus, "_dataset_log_pred.csv")
data1 <- read.csv2(file)
names(data1)
str(data1)

#Load data
genus <- "Raja" 
file <- paste0(temp_data, "/data_subsets/", genus, "_dataset_log_pred.csv")
data2 <- read.csv2(file)
names(data2)
str(data2)

data2 <- data2[, c("code", "Vessel")]
data1 <- merge(data1, data2[, c("code", "Vessel")], by = "code", all.x = TRUE)

#Save data set:
genus <- "Scyliorhinus" 
output_file <- paste0(temp_data, "/data_subsets/", genus, "_dataset_log_pred.csv")
write.csv2(data1, file = output_file, row.names = FALSE)




# 2. Add random number ---------------------------------------------------------
# Before splitting it, create a Random Number which must be present in both training and testing datasets
genus <- "Raja" #Scyliorhinus #Raja
file <- paste0(temp_data, "/data_subsets/", genus, "_dataset_log_pred.csv")
data <- read.csv2(file)
names(data)
str(data)

# Generate Random Number (from 1 to 100) and an ID column for each row (i.e. specimen)
# RN will serve as an indicator for variables that have influence greater or less than 
# random (Scales et al., 2017; Soykan, Eguchi, Kohin, & Dewar, 2014);
# only variables with influence greater than the random number were included in the final models.

data$RN <- sample.int(100, size=nrow(data), replace=T, prob=NULL)
data <- data %>%
  mutate(id = seq_along(RN))
head(data)

# Split the data set into train and test sets. You can do it either:
# (1) dividing the data randomly (usually 70%; 30%, if data is limited, you may just use cross validation instead):
# Set the seed for reproducibility
#set.seed(123)
#train_frac <- 0.8
#train_indices <- sample(1:nrow(data), size = train_frac * nrow(data))
#train_data <- data[train_indices, ]
#test_data <- data[-train_indices, ]
#head(train_data)
#names(test_data)

# (2) or, as in this case, separating certain categories within your group ("random effect") factor. 
# In this case this factor is "Vessel"
# Count the occurrences of each value in the factor column
value_counts <- table(data$Vessel)
# Print the value counts
print(value_counts)
# Create a bar plot
barplot(value_counts, main="Value Counts Bar Chart", xlab="Value", ylab="Count")
#You can check that it is correct:
total_count_sum <- sum(value_counts)

# Set the seed for reproducibility
set.seed(123)
# Calculate the total number of unique Vessels values
unique_hauls <- unique(data$Vessel)
num_unique_hauls <- length(unique_hauls)

# Calculate the percentage of presence before splitting
pres_data <- data[data$presence_absence == 1, ]
ndata_pres<-nrow(pres_data)
ndata_pres

abs_data <- data[data$presence_absence == 0, ]
ndata_abs <-nrow(abs_data)
ndata_abs

ntot_data<-ndata_abs+ndata_pres
ntot_data
percetange_pres<-ndata_pres*100/ntot_data
percetange_pres
percetange_abs<-ndata_abs*100/ntot_data
percetange_abs


set.seed(226)  # For reproducibility

# 1. Get all unique vessels from the entire dataset
unique_vessels <- unique(data$Vessel)

# 2. Calculate the number of vessels to allocate to training (80%) and testing (20%)
train_frac <- 0.8
num_train_vessels <- round(train_frac * length(unique_vessels))

# 3. Randomly select vessels for the training set, the rest will be for the testing set
train_vessels <- sample(unique_vessels, size = num_train_vessels)
test_vessels  <- setdiff(unique_vessels, train_vessels)

# 4. Filter the data based on the selected vessels for training and testing sets
train_data <- data[data$Vessel %in% train_vessels, ]
test_data  <- data[data$Vessel %in% test_vessels, ]

# 5. Shuffle rows within the training and testing dataframes (optional, for randomization)
train_data <- train_data[sample(nrow(train_data)), ]
test_data  <- test_data[sample(nrow(test_data)), ]

# 6. Print the unique Vessel values in each dataset to confirm no overlap
cat("Unique vessels in train_data:", unique(train_data$Vessel), "\n")
cat("Unique vessels in test_data:", unique(test_data$Vessel), "\n")

# 7. Check the presence/absence ratio in the train and test sets
train_percentage_pres <- sum(train_data$presence_absence == 1) / nrow(train_data) * 100
test_percentage_pres  <- sum(test_data$presence_absence == 1) / nrow(test_data) * 100

cat("Percentage of presence in train_data:", train_percentage_pres, "\n")
cat("Percentage of presence in test_data:", test_percentage_pres, "\n")

#Check it all if needed:
View(train_data)
View(test_data)

#Create folder to save them:
output_dir <- file.path(temp_data, paste0("train_test/", genus, "_training_testing"))
if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)}

#Save them:
output_file <- file.path(output_dir, paste0(genus,"_train_dataset.csv"))
write.csv2(train_data, file = output_file, row.names = FALSE)

output_file <- file.path(output_dir, paste0(genus,"_test_dataset.csv"))
write.csv2(test_data, file = output_file, row.names = FALSE)
