# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management

# ------------------------------------------------------------------------------
# 3.8. Summarise egg-case content in the ECEME biological subsample
# ------------------------------------------------------------------------------
library(readxl)
source("fun/workflow_settings.R")

# 1. Set input and output files ------------------------------------------------
# Rscript 3_Subsetting_Checking_data/3_8_summarise_ECEME_content.R workbook.xlsx output.csv
args <- workflow_arguments(c("eceme_workbook", "eceme_summary"))
# Skip the workbook header row and read only the biological subsample sheet.
data <- read_excel(args[1], sheet = "Elasmobranquios", skip = 1, .name_repair = "minimal")
# Retain rows with numerical capsule identifiers, excluding notes and blank rows.
data <- data[!is.na(suppressWarnings(as.numeric(data[[1]]))), ]
if (anyDuplicated(data[[1]])) stop("Duplicate capsule identifier")

# 2. Classify recorded content -------------------------------------------------
# All development states, including uncertain states, indicate non-empty cases.
# Rotten content also counts as non-empty; this does not imply a viable embryo.
# Standardise recorded development labels before classifying their content.
state <- tolower(trimws(as.character(data[["Estado de desarrollo"]])))
# Labels containing the empty-case word are classified as empty.
empty <- grepl("vac", state, fixed = TRUE)
rotten <- grepl("podrido", state)
# Uncertain, numbered and rotten states are non-empty under the author definition.
embryo <- rotten | grepl("estado|etstado|indeterminable", state) |
  grepl("^[0-9]+([.,][0-9]+)?$", state)
embryo <- embryo & !empty
# Keep genuinely missing or unrecognised states visible rather than guessing.
unknown <- is.na(state) | !(empty | embryo)
species <- as.character(data[["Especie"]])

# 3. Summarise using every capsule in the group as the denominator --------------
groups <- c(sort(unique(species)), "Raja spp.", "All species")
summary <- do.call(rbind, lapply(groups, function(group) {
  keep <- if (group == "All species") rep(TRUE, nrow(data)) else if (group == "Raja spp.") grepl("^Raja ", species) else species == group
  n <- sum(keep)
  data.frame(
    taxon = group, n = n, empty = sum(empty[keep]),
    embryo = sum(embryo[keep]), rotten = sum(rotten[keep]), unknown = sum(unknown[keep]),
    empty_pct = 100 * sum(empty[keep]) / n, embryo_pct = 100 * sum(embryo[keep]) / n
  )
}))
stopifnot(all(summary$empty + summary$embryo + summary$unknown == summary$n))
dir.create(dirname(args[2]), recursive = TRUE, showWarnings = FALSE)
write.csv(summary, args[2], row.names = FALSE)
print(summary)
