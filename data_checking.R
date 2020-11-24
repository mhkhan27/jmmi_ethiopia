library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(httr)
library(randomcoloR)
library(anytime)

# set wd to this script's locations
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#source("./utilities.R")
#source("./send_email.R")
#source("./cleaning_log.R")
source("./utils.R")


#---------------------------------------------------------------------------------------------------------
# PLEASE CHECK THE FOLLOWING PARAMETERS AT EACH ROUND AND UPDATE IF NEEDED
#---------------------------------------------------------------------------------------------------------
# specify month of the assessment (it is used in the name of the output files)
assessment.month <- "2020-11"
# specify URLs and input filenames
filename.tool <- "resources/ETH_JMMI_Kobo_v2.xlsx"
filename.email.list <- "resources/2020-07-15_email_list.xlsx"
filename.sendinblue.api.key <- "./api_key.txt"
filename.raw.dataset <- "data/20201123_data_submission.xlsx"

#---------------------------------------------------------------------------------------------------------
# PLEASE DO NOT CHANGE THE FOLLOWING PARAMETERS
#---------------------------------------------------------------------------------------------------------
# directories
directory.requests <- "output/partner_requests/"
directory.responses <- "output/partner_responses/"
directory.outputs <- "output/"
# output filenames
filename.out.fu.requests <- paste(directory.requests, assessment.month, "_follow_up_requests.xlsx", sep="")

#---------------------------------------------------------------------------------------------------------
# LOAD TOOL
#---------------------------------------------------------------------------------------------------------

tool.survey <- read_excel(filename.tool, sheet="survey")
tool.choices <- read_excel(filename.tool, sheet="choices") %>% filter(!is.na(list_name))


#---------------------------------------------------------------------------------------------------------
# Step 1: Merge datasets from all partners into one dataset
#---------------------------------------------------------------------------------------------------------

# load RAW dataset
raw <- read_excel(filename.raw.dataset, sheet=1, col_types = "text") %>% rename(uuid="_uuid")


#---------------------------------------------------------------------------------------------------------
# Step 2: General checks --> triggers an error
#---------------------------------------------------------------------------------------------------------

# check for duplicates
duplicates <- raw %>% group_by(uuid) %>% summarise(n=n()) %>% filter(n>1)
if (dim(duplicates)[1] > 0) stop("Duplicates detected in the RAW file.")

# check for survey duration
raw.check <- raw %>% 
  mutate(minutes=as.numeric(anytime(raw$end)-anytime(raw$start))) %>% 
  select(uuid, method, start, end, minutes)
raw.check.flagged <- filter(raw.check, minutes < 30)
if (nrow(raw.check.flagged) > 0) stop("Surveys with duration < 30 minutes were detected")

# check GPS
# TODO

#---------------------------------------------------------------------------------------------------------
# Step 3: Convert price to price_per_unit
#---------------------------------------------------------------------------------------------------------

# get list of all food and hygiene items (excluding water) and their standard units
all.items <- (tool.choices %>% filter(list_name=="all_items", name!="water"))$name
standard.units <- do.call(rbind, lapply(all.items, function(x) {
  variable <- paste0(x, "_standard_unit")
  label <- tool.survey[!is.na(tool.survey$name) & tool.survey$name==variable, "label::English"]
  unit <- str_remove(str_split(as.character(label), "in units of ")[[1]][2], "\\?")
  return(data.frame(item=x, standard.unit=str_sub(unit, 3, str_length(unit)), quantity=1))}))
# -> check that standard.units are correctly extracted from the tool

# get list of non-standard units reported in other
cols <- as.character(lapply(all.items, function(x) paste0(x, "_nonstandard_unit")))
non.standard.units <- unique(na.omit(as.character(t(raw[cols]))))

# convert relevant columns to numeric
cols <- c(as.character(lapply(all.items, function(x) paste0(x, "_nonstandard_unit_g"))),
          as.character(lapply(all.items, function(x) paste0(x, "_nonstandard_unit_ml"))),
          as.character(lapply(all.items, function(x) paste0(x, "_price"))),
          as.character(lapply(all.items, function(x) paste0(x, "_stock_days"))),
          as.character(lapply(all.items, function(x) paste0(x, "_resupply_days"))),
          "truck_capacity", 
          "water_price_base", "water_price_5km", "water_price_10km")
raw.unit <- to.double(raw, columns=cols)

# calculate price per unit (i.e. conversion for prices not collected in standard units)
calculate_price_per_unit <- function(standard_unit, non_standard_unit, unit_g, unit_ml, price){
  case_when(
    standard_unit == "yes" ~ price,
    non_standard_unit == "gram" ~ price / unit_g * 1000,
    non_standard_unit == "millilitre" ~ price / unit_ml * 1000,
    TRUE ~ NA_real_)
}
for (item in all.items){
  raw.unit[[paste0(item, "_price_per_unit")]] <- 
    calculate_price_per_unit(raw.unit[[paste0(item, "_standard_unit")]],
                             raw.unit[[paste0(item, "_nonstandard_unit")]],
                             raw.unit[[paste0(item, "_nonstandard_unit_g")]],
                             raw.unit[[paste0(item, "_nonstandard_unit_ml")]],
                             raw.unit[[paste0(item, "_price")]])
}
# calculate price per unit for water
raw.unit$water_price_per_unit <- raw.unit$water_price_base/raw.unit$truck_capacity
raw.unit$water_price_per_unit_5km <- raw.unit$water_price_5km/raw.unit$truck_capacity
raw.unit$water_price_per_unit_10km <- raw.unit$water_price_10km/raw.unit$truck_capacity


#---------------------------------------------------------------------------------------------------------
# Step 4: Outliers detection
#---------------------------------------------------------------------------------------------------------

# get list of columns to be checked for outliers
cols.outliers <- c(as.character(lapply(all.items, function(x) paste0(x, "_price_per_unit"))),
                   "water_price_per_unit", 
                   "water_price_per_unit_5km", "water_price_per_unit_10km")
# detect outliers
outliers.sub1 <- raw.unit %>% 
  select("uuid", all_of(cols.outliers)) %>% 
  detect.outliers(., method="sd-linear", n.sd=3)
outliers.sub2 <- raw.unit %>% 
  select("uuid", all_of(cols.outliers)) %>% 
  detect.outliers(., method="sd-log", n.sd=3)
outliers.sub3 <- raw.unit %>% 
  select("uuid", all_of(cols.outliers)) %>% 
  detect.outliers(., method="iqr")
outliers <- rbind(outliers.sub1, outliers.sub2, outliers.sub3)
outliers <- outliers %>% mutate(mid=paste0(uuid, variable))
outliers <- outliers[!duplicated(outliers$mid),] %>% select(-mid)

# add outliers to cleaning log
# TODO: check once new data arrives
cleaning.log.outliers <- data.frame()
for (r in 1:nrow(outliers)){
  uuid <- as.character(outliers[r, "uuid"])
  variable <- as.character(outliers[r, "variable"])
  item <- str_split(variable, "_price_per_unit")[[1]][1]
  # get price variable
  if (variable=="water_price_per_unit") price.variable <- "water_price_base"
  else if (variable=="water_price_per_unit_5km") price.variable <- "water_price_5km"
  else if (variable=="water_price_per_unit_10km") price.variable <- "water_price_10km"
  else price.variable <- paste0(item, "_price")
  # get reported unit and quantity
  if (item=="water"){
    unit <- "litre (truck capacity)"
    quantity <- get.value(raw.unit, uuid, "truck_capacity")
  } else{
    if (get.value(raw.unit, uuid, paste0(item, "_standard_unit"))=="yes"){
      unit <- as.character(standard.units[standard.units$item==item, "standard.unit"])
      quantity <- as.numeric(standard.units[standard.units$item==item, "quantity"])
    } else{
      if (get.value(raw.unit, uuid, paste0(item, "_nonstandard_unit"))=="gram"){
        unit <- "gram"
        quantity <- get.value(raw.unit, uuid, paste0(item, "_nonstandard_unit_g"))
      } else if (get.value(raw.unit, uuid, paste0(item, "_nonstandard_unit"))=="millilitre"){
        unit <- "millilitre"
        quantity <- get.value(raw.unit, uuid, paste0(item, "_nonstandard_unit_ml"))
      } else {
        unit <- get.value(raw.unit, uuid, paste0(item, "_nonstandard_unit"))
        quantity <- 1
      }
    }
  }
  cleaning.log.outliers <- rbind(cleaning.log.outliers,
                                 data.frame(uuid=uuid, item=item,
                                            variable="quantity", old.value=quantity),
                                 data.frame(uuid=uuid, item=item,
                                            variable="unit", old.value=unit),
                                 data.frame(uuid=uuid, item=item,
                                            variable="price",
                                            old.value=get.value(raw.unit, uuid, price.variable)))
}
cleaning.log.outliers$issue <- "Price seems to be too low or to high. Please check/confirm the quantity, unit, and price."
cleaning.log.outliers$check.id <- "Outlier"

# create boxplot to visually inspect outlier detection performance
df <- raw.unit[, c("uuid", "adm1_region", "adm2_zone", "adm3_woreda", all_of(cols.outliers))] %>% 
  pivot_longer(cols=all_of(cols.outliers), names_to="variable", values_to="value") %>% 
  filter(!is.na(value))
df$item <- as.character(lapply(df$variable, function(x) str_split(x, "_price")[[1]][1]))
cl <- cleaning.log.outliers %>% 
  filter(variable=="price") %>% select(uuid, item) %>% mutate(detected=T)
df <- left_join(df, cl, by=c("uuid", "item")) %>% 
  mutate(detected=ifelse(is.na(detected), F, detected),
         adm0="national")
f.alpha <- function(x) return(ifelse(x, 1, 0))
f.colour <- function(x) return(ifelse(x, "#FF0000", "#00FF00"))
g <- ggplot(df) +
  geom_boxplot(aes(x=adm0, y=(as.numeric(value)))) + ylab("Values") +
  geom_point(aes(x=adm0, y=(as.numeric(value))), 
             alpha=f.alpha(df$detected), colour=f.colour(df$detected)) +
  facet_wrap(~variable, scales="free_y", nrow = 6, ncol = 3)
ggsave(paste0("output/test_outlier_analysis.pdf"), g, 
       width = 40, height = 40, units = "cm", device="pdf")


#---------------------------------------------------------------------------------------------------------
# Step 5: Logical checks
#---------------------------------------------------------------------------------------------------------

# In the early section about item availability, vendors are first asked about the general 
# availability of every monitored item in the market (A), and are then asked which of 
# these items they are currently selling (B). If they say they’re selling a particular 
# item (in B) that they previously marked “completely unavailable in this marketplace” 
# (in A), this is a clear contradiction that needs to be corrected.

check.availability <- function(r){
  if (!is.na(r["food_sold"])){
    food.sold <- str_split(r["food_sold"], " ")[[1]]
    food.sold[food.sold=="goat_meat"] <- "goatmeat"
    food.sold[food.sold=="vegetables_leafy_darkgreen"] <- "vegetables"
    food.sold[food.sold=="cooking_oil"] <- "cookingoil"
    cols.food <- as.character(lapply(food.sold, function(x) paste0("availability_food_", x)))
  } else cols.food <- c()
  if (!is.na(r["hygiene_sold"])){
    hygiene.sold <- str_split(r["hygiene_sold"], " ")[[1]]
    hygiene.sold[hygiene.sold=="bath_soap"] <- "bathsoap"
    cols.hygiene <- as.character(lapply(hygiene.sold, function(x) paste0("availability_hygiene_", x)))
  } else cols.hygiene <- c()
  cols <- c(cols.food, cols.hygiene)
  if (length(cols) > 0){
    df <- pivot_longer(data.frame(as.list(r[c("uuid", all_of(cols))])), 
                       cols=all_of(cols), names_to="variable", values_to="old.value") %>% 
      filter(old.value=="unavailable")
    return(df)
  } else return(data.frame())
}
res <- apply(raw.unit, 1, check.availability)
cleaning.log.logical <- do.call(rbind, res[as.logical(lapply(res, function(x) nrow(x)>0))]) %>% 
  mutate(item=NA, check.id="Logical",
         issue="This item was reported to be completely unavailable in the marketplace, but it was reported to be sold this week. Please correct the availability (fully_available or limited).")


#---------------------------------------------------------------------------------------------------------
# Step 6: Produce file with follow-up requests to be sent to partners
#---------------------------------------------------------------------------------------------------------

cleaning.log <- rbind(cleaning.log.outliers, cleaning.log.logical)
cleaning.log$new.value <- NA
cleaning.log$explanation <- NA
cleaning.log <- left_join(cleaning.log, 
                          select(raw.unit, uuid, date, partner, enumerator_id, 
                                 adm1_region, adm2_zone, adm3_woreda),
                          by="uuid")
cleaning.log.cols <- c("uuid", "date", "partner", "enumerator_id", 
                       "adm1_region", "adm2_zone", "adm3_woreda", "check.id", "issue",
                       "item", "variable", "old.value", "new.value", "explanation")
cleaning.log <- select(cleaning.log, all_of(cleaning.log.cols))

save.follow.up.requests(cleaning.log)


#---------------------------------------------------------------------------------------------------------
# Module 3: split cleaning log and send emails
#---------------------------------------------------------------------------------------------------------

# split cleaning log in sepearate files, one for each partner
split.follow.up(filename = filename.out.cl.follow_ups, output.directory = directory.requests)

# -> files are saved into directory.requests
# -> check manually that everything is ok

# get list of files and extract organisation from their names
request.filenames <- list.files(directory.requests, pattern="*", recursive=TRUE, full.names=TRUE)
e <- data.frame(filename=request.filenames, stringsAsFactors=FALSE) %>% 
  mutate(organisation=apply(., 1, get.org))

# load email list
email.list <- read.xlsx(filename.email.list, sheet="mapping_R")

# [TEST] check for duplicates and that all organisations from file names are in email list
if (length(unique(e$organisation)) != dim(e)[1]) stop("Request files with duplicated organisations")
if (any(!(e$organisation %in% email.list$organisation))) {
  print(e$organisation[!e$organisation %in% email.list$organisation])
  stop("Unknown organisation in the request files")
}

# join with email list
e.full <- left_join(e, email.list, by="organisation")

# load api.key
api.key <- as.character(read.csv(filename.sendinblue.api.key, header = FALSE, stringsAsFactors = FALSE))

#---------------------------------------------------------------------------------------------------------
# WARNING:
# - now you need to update the textContent of the email in utilities.R (line 98)
#---------------------------------------------------------------------------------------------------------

# test emails (no emails will be sent here)
g <- e.full %>% group_by(org.key) %>% group_map(~ send.email.partner(.x,.y, api.key, test=TRUE))

#---------------------------------------------------------------------------------------------------------
# WARNING:
# - the following command will send the emails to the partners.
# - uncomment and run only after testing with the command above.
#---------------------------------------------------------------------------------------------------------
# g <- e.full %>% group_by(org.key) %>% group_map(~ send.email.partner(.x,.y, api.key, test=FALSE))


#---------------------------------------------------------------------------------------------------------
# Module 4: combine responses and apply changes
#---------------------------------------------------------------------------------------------------------

# load cleaning_log (initial file before feedbacks)
cleaning.log <- read.csv(filename.out.cl.follow_ups) %>% mutate(ruuid=paste(uuid, variable))
colnames(cleaning.log) <- tolower(colnames(cleaning.log))
col.names <- names(cleaning.log)[1:(dim(cleaning.log)[2]-1)]

# load and combine response files
response.filenames <- list.files(directory.responses, pattern="*", recursive=TRUE, full.names=TRUE)
responses <- lapply(response.filenames, function(f){load.response.file(f, col.names)}) %>% 
  bind_rows() %>% filter(!is.na(index)) %>% 
  mutate(variable=sapply(strsplit(variable, "\r\n"), "[", 1),  # remove arabic text in variable name
         ruuid=paste(uuid, variable))
write.xlsx(select(responses, -ruuid), filename.out.combined.response)

# [TEST] check that all records in the combined response file have a match in the cleaning log
no.match <- filter(responses, !ruuid %in% cleaning.log$ruuid)
error.message <- paste("There are response records with no match in the cleaning log.")
if (dim(no.match)[1] > 0) stop(error.message)
# print(select(filter(responses, !ruuid %in% cleaning.log$ruuid), c(uuid, variable)))

# [TEST] check for duplicates in the combined response file
duplicates <- responses %>% group_by(uuid, variable) %>% summarise(n=n()) %>% filter(n>1)
error.message <- paste("There are duplicates in the combined response file")
if (dim(duplicates)[1] > 0) stop(error.message)
# print(duplicates)
# responses <- distinct(responses, ._uuid, variable, .keep_all=TRUE)

# join cleaning.log and responses to add new.value and explanation
joined <- left_join(select(cleaning.log, -c(new.value, explanation, variable.explanation)),
                    select(responses, c(uuid, variable, index, new.value, explanation)), 
                    by=c("uuid", "variable"))

# rename, add, and remove a few columns
edits <- joined %>% 
  mutate(response.received=!is.na(index.y),
         is.number.value=!is.na(as.numeric(value)),
         is.number.new.value=!is.na(as.numeric(new.value)), 
         modified=ifelse((is.number.new.value & (value!=new.value)) | 
                           (!is.na(new.value) & new.value=="NA") |
                           (!is.number.value & (value!=new.value)), TRUE, FALSE)) %>% 
  select(-c(index.y, ruuid, is.number.value, is.number.new.value))

# [TEST] check how many records in the cleaning log had received no feedback
num.no.feedback <- sum(!edits$response.received)
error.message <- paste("There was no feedback for", num.no.feedback, "records")
if (num.no.feedback > 0) stop(error.message)

# if there are records in the cleaning log without a response, set the new.value to NA
edits.complete <- edits %>% 
  mutate(new.value=ifelse(response.received, new.value, "NA"),
         modified=ifelse(response.received, modified, TRUE))

# save cleaning.log
write.csv(edits, filename.out.cl, row.names=F)

# keep only records for which a new.value was specified
edits.filtered <- edits.complete %>% 
  filter(modified) %>% 
  mutate(new.value = as.numeric(replace(new.value, new.value == "NA", NA)))

# load RAW dataset
raw.edited <- read.csv(filename.out.raw.edited, stringsAsFactors = FALSE)

# use clog package to apply changes to RAW dataset and get CLEAN dataset
my_cleaninglog <- cleaninglog(ids = edits.filtered$uuid,
                              variables = edits.filtered$variable,
                              new_values = edits.filtered$new.value,
                              name = edits.filtered$q_orgname,
                              change = edits.filtered$modified,
                              data_id_column_name = "X_uuid")

cleaned_data <- clog_clean(raw.edited, my_cleaninglog)
write.csv(cleaned_data, filename.out.clean, row.names=F)
