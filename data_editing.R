# set wd to this script's locations
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load configuration script (libraries, functions, directories, tool, etc.)  <-- update at each round
source("./config.R")

##########################################################################################################
# Step 1: load dataset_checked and combine follow up responses from all partners
##########################################################################################################

# load dataset_checked
raw.step1 <- read_excel(paste0("output/checking/", assessment.month, "_dataset_checked.xlsx"), col_types="text")
raw.step1 <- to.double(raw.step1, columns=get.numeric.columns())

# load and combine responses
response.filenames <- list.files("output/fu_responses/", pattern="*.xlsx", recursive=TRUE, full.names=TRUE)
responses <- do.call(plyr::rbind.fill, 
                     lapply(response.filenames, function(x) read_excel(x, sheet=1, col_types="text")))
write.xlsx(responses, paste0("output/editing/", assessment.month, "_follow_up_responses.xlsx"))

# check that number of responses = number of requests
request.filenames <- list.files("output/fu_requests/", pattern="*.xlsx", recursive=TRUE, full.names=TRUE)
requests <- do.call(plyr::rbind.fill, 
                    lapply(request.filenames, function(x) read_excel(x, sheet=1, col_types="text")))
if (nrow(responses) != nrow(requests)) stop("Number of responses does not match number of requests")

##########################################################################################################
# Step 2: generate cleaning log
##########################################################################################################

# split responses for outliers and logical checks
responses.outlier.price <- filter(responses, check.id=="Outlier.price")
responses.outlier.generic <- filter(responses, check.id=="Outlier.generic") %>% 
  select(uuid, variable, old.value, new.value, check.id)

# generate cleaning log for outlier responses
cleaning.log.prices <- do.call(rbind, responses.outlier.price %>% 
  group_by(uuid, item) %>% 
  group_map(~get.cleaning.log(.x, .y)))
cleaning.log.prices$old.value <- apply(cleaning.log.prices, 1, 
                                function(x) get.value(raw.step1, "uuid", x["uuid"], x["variable"]))
cleaning.log.prices$check.id <- "Outlier.price"

# combine with responses for logical checks and add missing columns
cl.editing <- rbind(cleaning.log.prices, responses.outlier.generic)
cl.editing <- cl.editing %>% mutate(modified=case_when(
  old.value==new.value ~ F,
  old.value!=new.value ~ T,
  is.na(old.value) & !is.na(new.value) ~ T,
  !is.na(old.value) & is.na(new.value) ~ T,
  TRUE ~ F)) %>% 
  filter(modified) %>% select(uuid, variable, old.value, new.value, check.id)

##########################################################################################################
# Step 3: apply changes, re-calculate price_per_unit and save dataset_cleaned
##########################################################################################################

# 1) apply changes
raw.step2 <- apply.changes(raw.step1, cl.editing)
# 2) re-calculate price_per_unit
raw.step2 <- add.price.per.unit(raw.step2)
# 3) remove personal data
cols.personal.data <- c("deviceid", "partner", "partner_other", "enumerator_id", "shop", "vendor", "phone", 
                        "general_comments", "enumerator_comments",
                        "gps", "_gps_latitude", "_gps_longitude", "_gps_altitude", "_gps_precision")
raw.step2 <- raw.step2 %>% select(-all_of(cols.personal.data))
# 4) save dataset
write.xlsx(raw.step2, paste0("output/editing/", assessment.month, "_dataset_cleaned.xlsx"))

##########################################################################################################
# Step 4: combine cleaning logs and save
##########################################################################################################

# 1) read cleaning log from checking script
cl.checking <- read_excel(paste0("output/checking/", assessment.month, "_cleaning_log_checking.xlsx"))
# 2) combine cleaning logs from checking and editing
cl.combined <- rbind(cl.checking, cl.editing)
# 3) save combined cleaning log
write.xlsx(cl.combined, paste0("output/editing/", assessment.month, "_cleaning_log.xlsx"))

##########################################################################################################
# Step 5: save summary of number of prices
##########################################################################################################

summary.num.prices <- get.num.prices(raw.step2)
write.xlsx(summary.num.prices, paste0("output/editing/", assessment.month, "_summary_number_prices.xlsx"))

