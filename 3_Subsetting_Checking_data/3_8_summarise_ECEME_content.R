# ------------------------------------------------------------------------------
# 3.8. Summarise egg-case content in the ECEME biological subsample
# ------------------------------------------------------------------------------
library(readxl)

# 1. Set input and output files ------------------------------------------------
# Rscript 3_Subsetting_Checking_data/3_8_summarise_ECEME_content.R workbook.xlsx output.csv
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop('Provide the workbook path and the summary CSV path')
data <- read_excel(args[1], sheet = 'Elasmobranquios', skip = 1, .name_repair = 'minimal')
data <- data[!is.na(suppressWarnings(as.numeric(data[[1]]))), ]
if (anyDuplicated(data[[1]])) stop('Duplicate capsule identifier')

# 2. Classify recorded content -------------------------------------------------
# All development states, including uncertain states, indicate non-empty cases.
# Rotten content also counts as non-empty; this does not imply a viable embryo.
state <- tolower(trimws(as.character(data[['Estado de desarrollo']])))
empty <- grepl('vac', state, fixed = TRUE)
rotten <- grepl('podrido', state)
embryo <- rotten | grepl('estado|etstado|indeterminable', state) |
  grepl('^[0-9]+([.,][0-9]+)?$', state)
embryo <- embryo & !empty
unknown <- is.na(state) | !(empty | embryo)
species <- as.character(data[['Especie']])

# 3. Summarise using every capsule in the group as the denominator --------------
groups <- c(sort(unique(species)), 'Raja spp.', 'All species')
summary <- do.call(rbind, lapply(groups, function(group) {
  keep <- if (group == 'All species') rep(TRUE, nrow(data)) else
    if (group == 'Raja spp.') grepl('^Raja ', species) else species == group
  n <- sum(keep)
  data.frame(taxon = group, n = n, empty = sum(empty[keep]),
    embryo = sum(embryo[keep]), rotten = sum(rotten[keep]), unknown = sum(unknown[keep]),
    empty_pct = 100 * sum(empty[keep]) / n, embryo_pct = 100 * sum(embryo[keep]) / n)
}))
stopifnot(all(summary$empty + summary$embryo + summary$unknown == summary$n))
dir.create(dirname(args[2]), recursive = TRUE, showWarnings = FALSE)
write.csv(summary, args[2], row.names = FALSE)
print(summary)
