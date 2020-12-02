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
# Step 3: generate analysis output for InDesign
##########################################################################################################

analysis.national <- run.analysis(data.woreda, "adm0_national")
analysis.region <- run.analysis(data.woreda, "adm1_region")
analysis.zone <- run.analysis(data.woreda, "adm2_zone")
analysis <- rbind(analysis.national, analysis.region, analysis.zone)
write.xlsx(analysis, "analysis.xlsx")

##########################################################################################################
# Step 4: generate boxplot at national level
##########################################################################################################

labels <- read_excel("resources/item_labels.xlsx")
cols <- colnames(data.woreda)[str_detect(colnames(data.woreda), "price_per_unit")]
data <- data.woreda[cols] %>% 
  pivot_longer(cols=all_of(cols), names_to="item", values_to="price_per_unit") %>% 
  filter(!is.na(price_per_unit)) %>% left_join(labels, by="item") %>% 
  mutate(item=paste0(label, "\n(", unit, ")")) %>% select(-c("label", "unit"))

boxplot_statistics <- function(x) {
  r <- quantile(x, probs = c(0.00, 0.25, 0.5, 0.75, 1))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(r)
}

get.number.label <- function(item, value){
  rounding <- ifelse(str_starts(item, "Water"), 2, 0)
  df <- data.frame(value=value, rounding=rounding)
  return(apply(df, 1, function(x) return(format(round(x["value"], digits=x["rounding"]), nsmall=0))))
}

medians <- plyr::ddply(data, "item", summarise, med = median(price_per_unit, na.rm=T))
mins <- plyr::ddply(data, "item", summarise, min = min(price_per_unit, na.rm=T))
maxs <- plyr::ddply(data, "item", summarise, max = max(price_per_unit, na.rm=T))

ggplot(data,aes(item, price_per_unit, width=0.3)) +
  stat_summary(fun.data = boxplot_statistics, geom="boxplot", fill = "#D1D3D4") +
  theme_bw() + 
  geom_text(data=mins, 
            aes(x=item, y=min, label=get.number.label(item, min)),
            size=2.5, vjust=1.5) +
  geom_text(data=medians, 
            aes(x=item, y=med, label=get.number.label(item, med)),
            size=2.5, hjust = -1) +
  geom_text(data=maxs, 
            aes(x=item, y=max, label=get.number.label(item, max)),
            size=2.5, vjust =-0.5) +
  theme(axis.text.x = element_text(angle = 0, size = 7, hjust = 0.5 ),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggsave("boxplot.pdf", width=27, height=12, units="cm", device="pdf")
  

