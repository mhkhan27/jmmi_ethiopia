# set wd to this script's locations
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load configuration script (libraries, functions, directories, tool, etc.)  <-- update at each round
source("./config.R")

##########################################################################################################
# Step 1: load dataset_checked and combine follow up responses from all partners
##########################################################################################################

# load dataset_checked
raw.step1 <- read_excel(paste0(directory.checking, "dataset_checked.xlsx"), col_types = "text")
raw.step1 <- to.double(raw.step1, columns=get.numeric.columns())

# load and combine responses
response.filenames <- list.files(directory.responses, pattern="*.xlsx", recursive=TRUE, full.names=TRUE)
responses <- do.call(plyr::rbind.fill, 
                     lapply(response.filenames, function(x) read_excel(x, sheet=1, col_types="text")))
write.xlsx(responses, paste0(directory.final, assessment.month, "_follow_up_responses.xlsx"))

##########################################################################################################
# Step 2: generate cleaning log
##########################################################################################################

# load file with combined responses
responses <- read_excel(paste0(directory.final, assessment.month, "_follow_up_responses.xlsx"), col_types="text")

# split responses for outliers and logical checks
responses.outlier <- filter(responses, check.id=="Outlier")
responses.logical <- filter(responses, check.id=="Logical") %>% 
  select(uuid, variable, old.value, new.value)

# generate cleaning log for outlier responses
cleaning.log <- do.call(rbind, responses.outlier %>% 
  group_by(uuid, item) %>% 
  group_map(~ get.cleaning.log(.x, .y)))
cleaning.log$old.value <- apply(cleaning.log, 1, function(x) get.value(raw.step1, x["uuid"], x["variable"]))

# combine with responses for logical checks and add missing columns
cleaning.log <- rbind(cleaning.log, responses.logical)
cleaning.log <- cleaning.log %>% mutate(modified=case_when(
  old.value==new.value ~ F,
  old.value!=new.value ~ T,
  is.na(old.value) & !is.na(new.value) ~ T,
  !is.na(old.value) & is.na(new.value) ~ T,
  TRUE ~ F)) %>% 
  filter(modified) %>% select(uuid, variable, old.value, new.value)

##########################################################################################################
# Step 3: apply changes and re-calculate price_per_unit
##########################################################################################################

raw.step3 <- apply.changes(raw.step1, cleaning.log)
raw.step3 <- add.price.per.unit(raw.step3)
write.xlsx(raw.step3, paste0(directory.final, assessment.month, "_cleaned_dataset.xlsx"))
