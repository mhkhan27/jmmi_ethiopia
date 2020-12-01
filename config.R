if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, writexl, openxlsx, httr, randomcoloR, anytime, sf, leaflet, mapview)

source("./utils.R")

##########################################################################################################
# PLEASE CHECK THE FOLLOWING PARAMETERS AT EACH ROUND AND UPDATE IF NEEDED
##########################################################################################################
# specify month of the assessment (it is used in the name of the output files)
assessment.month <- "2020-11"
# specify URLs and input filenames
filename.tool <- "resources/ETH_JMMI_Kobo_v2.xlsx"
filename.email.list <- "resources/2020-07-15_email_list.xlsx"
filename.sendinblue.api.key <- "./api_key.txt"
filename.raw.dataset <- "data/20201130_data_submission.xlsx"

##########################################################################################################
# PLEASE DO NOT CHANGE THE FOLLOWING PARAMETERS
##########################################################################################################
# directories
directory.requests <- "output/partner_requests/"
directory.responses <- "output/partner_responses/"
directory.final <- "output/final/"
directory.checking <- "output/checking/"
directory.outputs <- "output/"

##########################################################################################################
# LOAD TOOL AND GENERATE VARIABLES BASED ON TOOL
##########################################################################################################

# load tool
tool.survey <- read_excel(filename.tool, sheet="survey") %>% filter(!is.na(name))
tool.choices <- read_excel(filename.tool, sheet="choices") %>% filter(!is.na(list_name))

# get list of all food and hygiene items (excluding water) and their standard units
all.items <- get.all.items()
standard.units <- get.list.std.units()
# -> check that standard.units are correctly extracted from the tool

##########################################################################################################
# DEFINE LISTS OF COLUMNS
##########################################################################################################

# get list of base prices for all items
base.prices <- c(as.character(lapply(all.items, function(x) paste0(x, "_price"))),
                 "water_price_base", "water_price_5km", "water_price_10km")

# get list of columns to be checked for outliers
cols.outliers1 <- c(as.character(lapply(all.items, function(x) paste0(x, "_price_per_unit"))),
                    "water_price_per_unit", 
                    "water_price_per_unit_5km", "water_price_per_unit_10km")

# get list of columns to be checked for generic outliers
cols.outliers.gen <- c(as.character(lapply(all.items, function(x) paste0(x, "_stock_days"))),
                       as.character(lapply(all.items, function(x) paste0(x, "_resupply_days"))))

