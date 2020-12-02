# set wd to this script's locations
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load configuration script (libraries, functions, directories, tool, etc.)  <-- update at each round
source("./config.R")

##########################################################################################################
# Step 1: load dataset
##########################################################################################################

data <- read_excel(paste0(directory.final, assessment.month, "_cleaned_dataset.xlsx"), guess_max = 20000)

##########################################################################################################
# Step 2: aggregate at woreda level <-- basis for the entire analysis
##########################################################################################################

cols <- colnames(data)[!(colnames(data) %in% c("adm3_woreda", "partner", "phone")) &
                         !str_starts(colnames(data), "type_vendor") &
                         !str_ends(colnames(data), "_standard_unit") &
                         !str_ends(colnames(data), "_nonstandard_unit") &
                         !str_ends(colnames(data), "_nonstandard_unit_g") &
                         !str_ends(colnames(data), "_nonstandard_unit_ml") &
                         !str_ends(colnames(data), "_price") &
                         !str_ends(colnames(data), "truck_capacity") &
                         !str_ends(colnames(data), "water_price_base") &
                         !str_ends(colnames(data), "water_price_5km") &
                         !str_ends(colnames(data), "water_price_10km")]

data.woreda <- lapply(cols, function(x){
  
  # get question type (select_one, select_multiple, integer, etc.)
  if (str_detect(x, "/")) q.type <- "select_multiple"
  else if (x %in% tool.survey$name) q.type <- str_split(as.character(tool.survey[tool.survey$name==x, "type"]), " ")[[1]][1]
  else q.type <- "not found"
  if (!str_detect(x, "/") & q.type=="select_multiple") q.type <- "select_multiple_concat"
  
  # group_by woreda
  df <- data[c("adm3_woreda", x)] %>% filter(!is.na(!!sym(x))) %>% group_by(adm3_woreda)
  
  # aggregate based on question type and name
  if (q.type %in% c("select_one", "select_multiple", "integer", "decimal") | str_detect(x, "price_per_unit")){
    if (nrow(df) == 0) return(data.frame(adm3_woreda=unique(data$adm3_woreda)) %>% mutate(!!x := NA))
    if (q.type %in% c("select_one", "select_multiple")){
      df <- df %>% summarise(!!x := case_when(
        str_starts(x, "availability") ~ get.availability(!!sym(x)),
        str_starts(x, "food_sold") ~ get.at.least.one(!!sym(x)),
        str_starts(x, "hygiene_sold") ~ get.at.least.one(!!sym(x)),
        TRUE ~ get.mode(!!sym(x))))
    } else{
      df <- df %>% summarise(!!x := median(!!sym(x)))
    }
    return(df)
  }
  return(NULL)
})
data.woreda <- data.woreda[!sapply(data.woreda, is.null)] %>% reduce(full_join, by="adm3_woreda")
write.xlsx(data.woreda, "data.woreda.xlsx")

##########################################################################################################
# Step 3: determine list of indicators
##########################################################################################################

analysis.national <- run.analysis(data.woreda, "adm0_national")
analysis.region <- run.analysis(data.woreda, "adm1_region")
analysis.zone <- run.analysis(data.woreda, "adm2_zone")
analysis <- rbind(analysis.national, analysis.region, analysis.zone)
write.xlsx(analysis, "analysis.xlsx")
