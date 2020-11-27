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
                         !str_starts(colnames(data), "type_vendor")]
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
    } else if (q.type %in% c("integer", "decimal") | str_detect(x, "price_per_unit")){
      df <- df %>% summarise(!!x := median(!!sym(x)))
    }
    return(df)
  }
  return(NULL)
})
data.woreda <- data.woreda[!sapply(data.woreda, is.null)] %>% reduce(full_join, by="adm3_woreda")
write.xlsx(b, "data.woreda.xlsx")

##########################################################################################################
# Step 3: determine list of indicators
##########################################################################################################



