# set wd to this script's locations
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load configuration script (libraries, functions, directories, tool, etc.)  <-- update at each round
source("./config.R")

##########################################################################################################
# Step 1: load dataset
##########################################################################################################

data <- read_excel(paste0("output/editing/", assessment.month, "_dataset_cleaned.xlsx"), guess_max = 20000)

##########################################################################################################
# Step 2: aggregate at woreda level <-- basis for the entire analysis
##########################################################################################################

cols <- colnames(data)[!(colnames(data) %in% c("adm3_woreda")) &
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

# add number of prices for each item
cols <- colnames(data)[str_detect(colnames(data), "price_per_unit")]
num.prices <- data %>% group_by(adm3_woreda) %>% select(all_of(cols)) %>% summarise_all(~sum(!is.na(.)))
colnames(num.prices) <- lapply(colnames(num.prices), function(x){
  return(ifelse(x=="adm3_woreda", x, paste0("number_prices_", str_split(x, "_price_per_unit")[[1]][1])))})
data.woreda <- left_join(data.woreda, num.prices, by="adm3_woreda")
data.woreda <- data.woreda %>% 
  relocate(colnames(data.woreda)[str_starts(colnames(data.woreda), "number_prices_")], .before=cols[1])

##########################################################################################################
# Step 3: generate analysis output for InDesign
##########################################################################################################

# run analysis for each admin level
analysis.woreda <- run.analysis(data.woreda, "adm3_woreda")
analysis.zone <- run.analysis(data.woreda, "adm2_zone")
analysis.region <- run.analysis(data.woreda, "adm1_region")
analysis.national <- run.analysis(data.woreda, "adm0_nation")

# combine analysis of the 4 admin levels and add admin names
analysis <- rbind(analysis.national, analysis.region, analysis.zone, analysis.woreda) %>%
  mutate(admin.level=case_when(
    str_length(admin)==2 ~ "Nation",
    str_length(admin)==4 ~ "Region",
    str_length(admin)==6 ~ "Zone",
    str_length(admin)==8 ~ "Woreda",
    TRUE ~ NA_character_)) %>%
  left_join(select(tool.choices, name, `label::English`), by=c("admin"="name")) %>% 
  rename(admin.pcode=admin, admin.name="label::English") %>% 
  relocate(admin.level, .before="admin.pcode") %>% 
  relocate(admin.name, .after="admin.pcode")

# add JMMI basket cost
analysis <- add.basket.cost(analysis, "full")
analysis <- add.basket.cost(analysis, "food")

# add prices in USD
df <- analysis[get.columns.prices.baskets(analysis)] / USD.to.BIRR
colnames(df) <- as.character(lapply(colnames(df), function(x) paste0(x, ".USD")))
analysis <- cbind(analysis, df)

# add time trends for prices per unit and basket cost
# analysis <- calculate.time.trends(analysis, num.months=1)

# add summary columns based on non-aggregated dataset
data.partners <- read_excel(filename.raw.dataset, sheet=1, col_types="text") %>% 
  rename(uuid="_uuid") %>% select(uuid, partner) %>% distinct()
res <- do.call(rbind, lapply(c("adm0_nation", "adm1_region", "adm2_zone", "adm3_woreda"),
                      function(x) calculate.summary.columns(data, data.partners, x)))
analysis <- left_join(analysis, res, by="admin.pcode")

# save analysis
write.xlsx(analysis, paste0("output/analysis/", assessment.month, "_analysis_InDesign.xlsx"))

##########################################################################################################
# Step 4: generate boxplots at national level
##########################################################################################################

labels <- read_excel("resources/item_labels.xlsx")
cols <- colnames(data.woreda)[str_detect(colnames(data.woreda), "price_per_unit")]
data <- data.woreda[cols] %>% 
  pivot_longer(cols=all_of(cols), names_to="item", values_to="price_per_unit") %>% 
  filter(!is.na(price_per_unit)) %>% left_join(labels, by="item") %>% 
  mutate(item=paste0(label, "\n(", unit, ")")) %>% select(-c("label", "unit")) %>% 
  mutate(category=case_when(
    str_detect(item, "Beef|Goat|Mutton") ~ "meat_items",
    str_detect(item, "Water") ~ "water_items",
    TRUE ~ "other_items"))

# one boxplot with all items
analysis.boxplot(data, "all_items")

# one boxplot for each category (meat_items, water_items, other_items)
t <- data %>% group_by(category) %>% group_map(~analysis.boxplot(.x,.y))

##########################################################################################################
# Step 5: generate CSV file for the coverage map
##########################################################################################################

df <- analysis %>% filter(admin.level=="Woreda") %>% 
  select("admin.pcode") %>% rename(assessed_woreda=admin.pcode)
write_csv(df, paste0("output/analysis/", assessment.month, "_assessed_woreda_GIS.csv"))

