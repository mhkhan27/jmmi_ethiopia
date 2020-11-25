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
filename.raw.dataset <- "data/20201125_data_submission.xlsx"

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
# Step 1.1: Load tool and dataset
#---------------------------------------------------------------------------------------------------------

# load tool
tool.survey <- read_excel(filename.tool, sheet="survey")
tool.choices <- read_excel(filename.tool, sheet="choices") %>% filter(!is.na(list_name))

# load RAW dataset
raw <- read_excel(filename.raw.dataset, sheet=1, col_types = "text") %>% rename(uuid="_uuid")


#---------------------------------------------------------------------------------------------------------
# Step 1.2: General checks --> triggers an error
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
# Step 1.3: fix nonstandard_unit wrongly reported as "other" <-- MANUALLY!!
#---------------------------------------------------------------------------------------------------------

# generate dataframe with all other responses for the nonstandart_unit
cl.other <- data.frame()
cols <- colnames(raw)[grepl("_nonstandard_unit_other", colnames(raw))]
other <- raw[c("uuid", cols)] %>% 
  pivot_longer(cols=all_of(cols), names_to="variable", values_to="old.value") %>% 
  filter(!is.na(old.value))
# --> open other data.frame and manually recode existing units
cl.other <- rbind(cl.other, 
                  data.frame(uuid="c85a6190-052c-437b-aa06-7e46488804c0",
                             variable="bath_soap_standard_unit", old.value="no", new.value="yes"),
                  data.frame(uuid="c85a6190-052c-437b-aa06-7e46488804c0",
                             variable="bath_soap_nonstandard_unit", old.value="other", new.value=NA),
                  data.frame(uuid="c85a6190-052c-437b-aa06-7e46488804c0",
                             variable="bath_soap_nonstandard_unit_other", old.value="Pieces", new.value=NA))
# apply changes
raw.step1 <- apply.changes(raw, cl.other)

#---------------------------------------------------------------------------------------------------------
# Step 1.4: add columns with conversion from price to price_per_unit
#---------------------------------------------------------------------------------------------------------

# get list of all food and hygiene items (excluding water) and their standard units
all.items <- (tool.choices %>% filter(list_name=="all_items", name!="water"))$name
standard.units <- get.list.std.units()
# -> check that standard.units are correctly extracted from the tool

# get list of nonstandard_unit reported in the dataset
# -> check if calculate_price_per_unit needs to be updated to include new units
non.standard.units <- get.list.nstd.units()

# convert relevant columns to numeric
cols <- c(as.character(lapply(all.items, function(x) paste0(x, "_nonstandard_unit_g"))),
          as.character(lapply(all.items, function(x) paste0(x, "_nonstandard_unit_ml"))),
          as.character(lapply(all.items, function(x) paste0(x, "_price"))),
          as.character(lapply(all.items, function(x) paste0(x, "_stock_days"))),
          as.character(lapply(all.items, function(x) paste0(x, "_resupply_days"))),
          "truck_capacity", 
          "water_price_base", "water_price_5km", "water_price_10km")
raw.step1 <- to.double(raw.step1, columns=cols)

# calculate price per unit (i.e. conversion for prices not collected in standard units)
raw.step1 <- add.price.per.unit(raw.step1)

#---------------------------------------------------------------------------------------------------------
# Step 1.5: Outliers detection (prices)
#---------------------------------------------------------------------------------------------------------

# get list of columns to be checked for outliers
cols.outliers1 <- c(as.character(lapply(all.items, function(x) paste0(x, "_price_per_unit"))),
                   "water_price_per_unit", 
                   "water_price_per_unit_5km", "water_price_per_unit_10km")
# detect outliers
outliers.sub1 <- raw.step1 %>% 
  select("uuid", all_of(cols.outliers1)) %>% 
  detect.outliers(., method="sd-linear", n.sd=3)
outliers.sub2 <- raw.step1 %>% 
  select("uuid", all_of(cols.outliers1)) %>% 
  detect.outliers(., method="sd-log", n.sd=3)
outliers.sub3 <- raw.step1 %>% 
  select("uuid", all_of(cols.outliers1)) %>% 
  detect.outliers(., method="iqr")
outliers <- rbind(outliers.sub1, outliers.sub2, outliers.sub3)
outliers <- outliers %>% mutate(mid=paste0(uuid, variable))
outliers <- outliers[!duplicated(outliers$mid),] %>% select(-mid)

# add outliers to cleaning log
# TODO: check once new data arrives
cleaning.log.outliers <- create.outliers.cleaning.log(outliers)

# create boxplot to visually inspect outlier detection performance
generate.price.outliers.boxplot()


#---------------------------------------------------------------------------------------------------------
# Step 1.6: Outliers detection (other numeric variables)
#---------------------------------------------------------------------------------------------------------

cols.outliers2 <- c(as.character(lapply(all.items, function(x) paste0(x, "_stock_days"))),
                    as.character(lapply(all.items, function(x) paste0(x, "_resupply_days"))))

outliers.sub1 <- raw.step1 %>% 
  select("uuid", all_of(cols.outliers2)) %>% 
  detect.outliers(., method="sd-linear", n.sd=3)
outliers.sub2 <- raw.step1 %>% 
  select("uuid", all_of(cols.outliers2)) %>% 
  detect.outliers(., method="sd-log", n.sd=3)
outliers.sub3 <- raw.step1 %>% 
  select("uuid", all_of(cols.outliers2)) %>% 
  detect.outliers(., method="iqr")
outliers <- rbind(outliers.sub1, outliers.sub2, outliers.sub3)
outliers <- outliers %>% mutate(mid=paste0(uuid, variable))
cleaning.log.outliers.generic <- outliers[!duplicated(outliers$mid),] %>% select(-mid) %>% 
  mutate(item=NA, check.id="Outlier",
         issue="Value seems to be too low or to high. Please check/confirm.")

# create boxplot to visually inspect outlier detection performance
generate.generic.outliers.boxplot()

#---------------------------------------------------------------------------------------------------------
# Step 1.7: Logical checks
#---------------------------------------------------------------------------------------------------------

# In the early section about item availability, vendors are first asked about the general 
# availability of every monitored item in the market (A), and are then asked which of 
# these items they are currently selling (B). If they say they’re selling a particular 
# item (in B) that they previously marked “completely unavailable in this marketplace” 
# (in A), this is a clear contradiction that needs to be corrected.

res <- apply(raw.step1, 1, check.availability)
cleaning.log.logical <- do.call(rbind, res[as.logical(lapply(res, function(x) nrow(x)>0))]) %>% 
  mutate(check.id="Logical",
         issue="This item was reported to be completely unavailable in the marketplace, but it was 
         reported to be sold this week. Please correct the availability (fully_available or limited).")


#---------------------------------------------------------------------------------------------------------
# Step 1.8: Produce file with follow-up requests to be sent to partners
#---------------------------------------------------------------------------------------------------------

cleaning.log <- rbind(cleaning.log.outliers, cleaning.log.outliers2, cleaning.log.logical)
cleaning.log$new.value <- NA
cleaning.log$explanation <- NA
cleaning.log <- left_join(cleaning.log, 
                          select(raw.step1, uuid, date, partner, enumerator_id, 
                                 adm1_region, adm2_zone, adm3_woreda),
                          by="uuid")
cleaning.log.cols <- c("uuid", "date", "partner", "enumerator_id", 
                       "adm1_region", "adm2_zone", "adm3_woreda", "check.id", "issue",
                       "item", "variable", "old.value", "new.value", "explanation")
cleaning.log <- select(cleaning.log, all_of(cleaning.log.cols))

save.follow.up.requests(cleaning.log)


#---------------------------------------------------------------------------------------------------------
# Step 2: split follow up responses and send emails to partners
#---------------------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------------------
# Step 3.1: combine follow up responses from all partners
#---------------------------------------------------------------------------------------------------------

directory.responses

response.filenames <- list.files(directory.responses, pattern="*", recursive=TRUE, full.names=TRUE)
responses <- lapply(response.filenames, function(f){load.response.file(f, col.names)}) %>% 
  bind_rows() %>% filter(!is.na(index)) %>% 
  mutate(variable=sapply(strsplit(variable, "\r\n"), "[", 1),  # remove arabic text in variable name
         ruuid=paste(uuid, variable))
write.xlsx(select(responses, -ruuid), filename.out.combined.response)


#---------------------------------------------------------------------------------------------------------
# Step 3.2: generate cleaning log
#---------------------------------------------------------------------------------------------------------

responses <- read_excel("output/partner_responses/2020-11_follow_up_responses.xlsx", col_types = "text")

responses.outlier <- filter(responses, check.id=="Outlier")
responses.logical <- filter(responses, check.id=="Logical") %>% 
  select(uuid, variable, old.value, new.value)

cleaning.log <- do.call(rbind, responses.outlier %>% 
  group_by(uuid, item) %>% 
  group_map(~ get.cleaning.log(.x, .y)))
cleaning.log$old.value <- apply(cleaning.log, 1, function(x) get.value(raw.step1, x["uuid"], x["variable"]))
cleaning.log <- rbind(cleaning.log, responses.logical)
cleaning.log <- cleaning.log %>% mutate(modified=case_when(
  old.value==new.value ~ F,
  old.value!=new.value ~ T,
  is.na(old.value) & !is.na(new.value) ~ T,
  !is.na(old.value) & is.na(new.value) ~ T,
  TRUE ~ F)) %>% 
  filter(modified) %>% select(uuid, variable, old.value, new.value)


#---------------------------------------------------------------------------------------------------------
# Step 3.3: apply changes and re-calculate price_per_unit
#---------------------------------------------------------------------------------------------------------

raw.step3 <- apply.changes(raw.step1, cleaning.log)
raw.step3 <- add.price.per.unit(raw.step3)




