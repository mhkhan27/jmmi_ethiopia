# set wd to this script's locations
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load configuration script (libraries, functions, directories, tool, etc.)  <-- update at each round
source("./config.R")

##########################################################################################################
# Step 1: load dataset
##########################################################################################################

raw <- read_excel(filename.raw.dataset, sheet=1, col_types = "text") %>% rename(uuid="_uuid")

##########################################################################################################
# Step 2: check for duplicates and short survey duration --> triggers an error
##########################################################################################################

# check for duplicates
duplicates <- raw %>% group_by(uuid) %>% summarise(n=n()) %>% filter(n>1)
if (dim(duplicates)[1] > 0) stop("Duplicates detected in the RAW file.")

# check for survey duration
raw.check <- raw %>% 
  mutate(minutes=as.numeric(anytime(raw$end)-anytime(raw$start), units="mins")) %>% 
  select(uuid, partner, method, date_of_dc, consent_yn, type_vendor, start, end, minutes)
raw.checks.flagged <- raw.check %>% 
  filter(consent_yn=="yes", minutes < 10)
if (nrow(raw.checks.flagged) > 0) stop("Surveys with duration < 10 minutes were detected")

##########################################################################################################
# Step 3: GPS check
##########################################################################################################

# 1) load shapes layers
woredas <-  st_transform(st_read("resources/ETH_COD_SHP/eth_admbnda_adm3_csa_bofed_20201008.shp"), crs = 4326)
zones <-  st_transform(st_read("resources/ETH_COD_SHP/eth_admbnda_adm2_csa_bofed_20201008.shp"), crs = 4326)
regions <-  st_transform(st_read("resources/ETH_COD_SHP/eth_admbnda_adm1_csa_bofed_20201008.shp"), crs = 4326)

# 2) convert dataset to sf data.frame and do spatial join with woreda layer
survey.points <- filter(raw, !is.na(`_gps_longitude`)) %>% 
  st_as_sf(x = ., coords = c("_gps_longitude", "_gps_latitude"), crs=4326) %>% 
  st_join(woredas["ADM3_PCODE"]) %>% st_join(zones["ADM2_PCODE"]) %>% st_join(regions["ADM1_PCODE"])

# 3) generate and save map
m <- leaflet() %>% 
  addPolygons(data = regions, color = "#0000FF", opacity = 0.3, fillOpacity = 0.1, weight = 2,
              label = regions$ADM3_PCODE) %>% 
  addPolygons(data = woredas, color = "#0000FF", opacity = 0.2, fillOpacity = 0, weight = 1,
              label = woredas$ADM3_PCODE) %>% 
  addCircleMarkers(data = survey.points, radius=5, color = "#FF00FF", stroke=F, fillOpacity = 0.5, 
                   label = paste0(survey.points$partner, "_", survey.points$adm3_woreda)) %>% 
  addTiles()
mapshot(m, file=paste0("output/checking/", assessment.month, "_map_samples.pdf"))  # <-- takes 1 minute

# 4) check that reported woreda is within the woreda polygon
raw.check <- st_drop_geometry(survey.points) %>% 
  mutate(woreda.reported=adm3_woreda, woreda.gps=ADM3_PCODE, check=(woreda.reported!=woreda.gps)) %>% 
  filter(check) %>% select(uuid, woreda.reported, woreda.gps)

# 5) generate cleaning log with required changes to fix wrongly reported admins
cl.adm3 <- raw.check %>% mutate(variable="adm3_woreda") %>% 
  rename(old.value=woreda.reported, new.value=woreda.gps)
cl.adm2 <- cl.adm3 %>% 
  mutate(variable="adm2_zone", old.value=str_sub(old.value, 1, 6), new.value=str_sub(new.value, 1, 6))
cl.adm1 <- cl.adm3 %>% 
  mutate(variable="adm1_region", old.value=str_sub(old.value, 1, 4), new.value=str_sub(new.value, 1, 4))
cl.gps <- rbind(cl.adm1, cl.adm2, cl.adm3)

# 6) apply changes
# raw.step1 <- apply.changes(raw, cl.gps)  # <-- uncomment to apply GPS checks
raw.step1 <- raw  # <-- uncomment to ignore GPS checks

##########################################################################################################
# Step 4: recode/remove nonstandard_unit reported as "other" <-- MANUALLY
##########################################################################################################

# generate dataframe with all other responses for the nonstandart_unit
cl.other <- data.frame()
cols <- colnames(raw.step1)[grepl("_nonstandard_unit_other", colnames(raw.step1))]
other <- raw.step1[c("uuid", cols)] %>% 
  pivot_longer(cols=all_of(cols), names_to="variable", values_to="old.value") %>% 
  filter(!is.na(old.value))

# --> open 'other' data.frame and manually recode/remove the responses in 'cl.other' below
cl.other <- rbind(
  get.entry.other.changes(uuid="c85a6190-052c-437b-aa06-7e46488804c0", item="bath_soap", 
                          standard_unit="no", nonstandard_unit="piece", 
                          nonstandard_unit_g=NA, nonstandard_unit_ml=NA, nonstandard_unit_other=NA),
  get.entry.other.changes(uuid="cc319a16-54d3-4eea-b758-aadd2b740306", item="bath_soap", 
                          standard_unit="no", nonstandard_unit="gram", 
                          nonstandard_unit_g=200, nonstandard_unit_ml=NA, nonstandard_unit_other=NA),
  get.entry.other.changes(uuid="cc319a16-54d3-4eea-b758-aadd2b740306", item="bleach", 
                          standard_unit="no", nonstandard_unit="gram", 
                          nonstandard_unit_g=20, nonstandard_unit_ml=NA, nonstandard_unit_other=NA),
  remove.food.item(raw.step1, uuid="cf347092-8ccb-42ef-8a93-ff25aaac6c13", item="beef"),
  get.entry.other.changes(uuid="2facc454-6ad7-4741-b163-a6965ee6f48c", item="bath_soap", 
                          standard_unit="no", nonstandard_unit="gram", 
                          nonstandard_unit_g=200, nonstandard_unit_ml=NA, nonstandard_unit_other=NA),
  get.entry.other.changes(uuid="a0b6aca2-fb2a-4336-9b4e-44d05a8d7d37", item="bath_soap", 
                          standard_unit="no", nonstandard_unit="gram", 
                          nonstandard_unit_g=200, nonstandard_unit_ml=NA, nonstandard_unit_other=NA))
# add old.valus
cl.other$old.value <- apply(cl.other, 1, function(x) get.value(raw.step1, "uuid", x["uuid"], x["variable"]))
cl.other <- select(cl.other, uuid, variable, old.value, new.value)
# keep only required changes
cl.other <- cl.other %>% 
  filter((is.na(old.value) & !is.na(new.value)) |
           (!is.na(old.value) & is.na(new.value)) |
           (!is.na(old.value) & !is.na(new.value) & old.value!=new.value)) %>% 
  mutate(check.id="Recode.other")
# apply changes
raw.step1 <- apply.changes(raw.step1, cl.other)

##########################################################################################################
# Step 5: logical check 1 (item availability vs item sold)
##########################################################################################################

# CHECK DESCRIPTION: In the early section about item availability, vendors are first asked about the 
# general availability of every monitored item in the market (A), and are then asked which of these 
# items they are currently selling (B). If they say they’re selling a particular item (in B) that they 
# previously marked “completely unavailable in this marketplace” (in A), this is a clear contradiction 
# that needs to be corrected.

# FIX: replace with mode of the woreda

# find issues and determine fixes
res <- apply(raw.step1, 1, check.availability)
cl.logical.check1 <- do.call(rbind, res[as.logical(lapply(res, function(x) nrow(x)>0))]) %>% 
  left_join(select(raw.step1, uuid, adm3_woreda), by="uuid")
cl.logical.check1$new.value <- apply(cl.logical.check1, 1, function(x){
  d <- as.vector(filter(raw.step1, adm3_woreda==x["adm3_woreda"])[[x["variable"]]])
  d <- d[!is.na(d) & (d %in% c("fully_available", "limited"))]
  if (length(d)==0) return("limited")
  else return(get.mode(d))
})
cl.logical.check1 <- cl.logical.check1 %>% 
  select(-c("item", "adm3_woreda")) %>% 
  mutate(check.id="Logical.check")

# apply fixes
raw.step1 <- apply.changes(raw.step1, cl.logical.check1)

##########################################################################################################
# Step 6: logical check 2 ("water" wrongly reported in type_vendor)
##########################################################################################################

# DESCRIPTION: if water is wrongly reported in type_vendor, the "water" sections should be set to NA.

# DETECTION: both truck_capacity and water_price_base are 0

# find uuid with issues
uuid.nok <- filter(raw.step1, as.numeric(truck_capacity)==0 & as.numeric(water_price_base)==0)[["uuid"]]

# generate cleaning log with the required changes
cl.logical.check2 <- do.call(rbind, lapply(uuid.nok, function(x) remove.water.responses(x)))
cl.logical.check2$old.value <- apply(cl.logical.check2, 1, 
                                     function(x) get.value(raw.step1, "uuid", x["uuid"], x["variable"]))
cl.logical.check2 <- select(cl.logical.check2, uuid, variable, old.value, new.value)
cl.logical.check2 <- cl.logical.check2 %>% 
  filter((is.na(old.value) & !is.na(new.value)) | (!is.na(old.value) & is.na(new.value)) |
           (!is.na(old.value) & !is.na(new.value) & old.value!=new.value)) %>% 
  mutate(check.id="Logical.check")

# apply changes
raw.step1 <- apply.changes(raw.step1, cl.logical.check2)

##########################################################################################################
# Step 7: logical check 3 (check that no reported base prices are 0) --> triggers an error
##########################################################################################################

raw.check <- raw.step1[c("uuid", all_of(base.prices))] %>% 
  pivot_longer(cols=all_of(base.prices), names_to="variable", values_to="value") %>% 
  filter(!is.na(value) & as.numeric(value)==0)

if (nrow(raw.check) > 0) stop("Prices 0 are detected")

##########################################################################################################
# Step 8: add columns with conversion from price to price_per_unit (basis for price outlier detection)
##########################################################################################################

# get list of nonstandard_unit reported in the dataset
non.standard.units <- get.list.nstd.units()
# -> check if there are new nonstandard units that need to be added in 'calculate_price_per_unit' function

# convert relevant columns to numeric
raw.step1 <- to.double(raw.step1, columns=get.numeric.columns())

# calculate price per unit (i.e. conversion for prices not collected in standard units)
# 1) run with test=T so that unrecognised units will result in -1
raw.step1 <- add.price.per.unit(raw.step1, test=T)
# 2) get list of unrecognised units --> fix function calculate_price_per_unit if needed
raw.check <- raw.step1[c("uuid", all_of(cols.outliers1))] %>% 
  pivot_longer(cols=all_of(cols.outliers1), names_to="variable", values_to="value") %>% 
  filter(!is.na(value) & as.numeric(value)==-1)
raw.check$item <- apply(raw.check, 1, function(x) str_split(x["variable"], "_")[[1]][1])
raw.check$nonstandard_unit <- apply(raw.check, 1, function(x) 
  get.value(raw.step1, "uuid", x["uuid"], paste0(x["item"], "_nonstandard_unit")))
raw.check$nonstandard_unit_other <- apply(raw.check, 1, function(x) 
  get.value(raw.step1, "uuid", x["uuid"], paste0(x["item"], "_nonstandard_unit_other")))
raw.check <- arrange(raw.check, item, nonstandard_unit, nonstandard_unit_other)
if (nrow(raw.check) > 0) stop(paste0(nrow(raw.check)," units were not converted. Edit calculate_price_per_unit function if needed."))
# 3) run with test=F to get correct prices per unit
raw.step1 <- add.price.per.unit(raw.step1, test=F)
# 4) test price per unit calculations --> open res and inspect results
test.calculation <- lapply(c(all.items, "water"), function(x) test.price.per.unit(raw.step1, x))

##########################################################################################################
# Step 9: Outliers detection (prices)
##########################################################################################################

# detect outliers
outliers.sub1 <- raw.step1 %>% 
  select("uuid", all_of(cols.outliers1)) %>% 
  detect.outliers(., method="sd-linear", n.sd=3)
outliers.sub2 <- raw.step1 %>% 
  select("uuid", all_of(cols.outliers1)) %>% 
  detect.outliers(., method="sd-log", n.sd=3)
outliers.sub3 <- raw.step1 %>% 
  select("uuid", all_of(cols.outliers1)) %>% 
  detect.outliers(., method="iqr-log")
outliers <- rbind(outliers.sub1, outliers.sub2, outliers.sub3)
outliers <- outliers %>% mutate(mid=paste0(uuid, variable))
outliers <- outliers[!duplicated(outliers$mid),] %>% select(-mid)

# add outliers to cleaning log
cleaning.log.outliers <- create.outliers.cleaning.log(outliers)

# create boxplot to visually inspect outlier detection performance
generate.price.outliers.boxplot()
# --> inspect the boxplot to visually spot missed outliers or unlikely ranges

##########################################################################################################
# Step 10: Outliers detection (other numeric variables)
##########################################################################################################

# detect outliers
outliers.sub1 <- raw.step1 %>% 
  select("uuid", all_of(cols.outliers.gen)) %>% 
  detect.outliers(., method="sd-linear", n.sd=3)
outliers.sub2 <- raw.step1 %>% 
  select("uuid", all_of(cols.outliers.gen)) %>% 
  detect.outliers(., method="iqr-linear")
outliers <- rbind(outliers.sub1, outliers.sub2)
outliers <- outliers %>% mutate(mid=paste0(uuid, variable))
cleaning.log.outliers.generic <- outliers[!duplicated(outliers$mid),] %>% select(-mid) %>% 
  mutate(item=NA, check.id="Outlier.generic",
         issue=ifelse(outlier.type=="high", 
                      "Value seems to be too high. Please check/confirm.",
                      "Value seems to be too low Please check/confirm.")) %>% 
  select(-outlier.type)

# create boxplot to visually inspect outlier detection performance
generate.generic.outliers.boxplot()
# --> inspect the boxplot to visually spot missed outliers or unlikely ranges

##########################################################################################################
# Step 11: Produce dataframe with follow-up requests to be sent to partners
##########################################################################################################

cleaning.log <- rbind(cleaning.log.outliers, cleaning.log.outliers.generic)
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

##########################################################################################################
# Step 12: save dataset_checked and changes made, and split follow up requests
##########################################################################################################

# save dataset_checked
write.xlsx(raw.step1, paste0("output/checking/", assessment.month, "_dataset_checked.xlsx"))

# save summary of number of prices per admin
summary.num.prices <- get.num.prices(raw.step1)
write.xlsx(summary.num.prices, paste0("output/checking/", assessment.month, "_summary_number_prices.xlsx"))

# save changes made to dataset_raw to produce dataset_checked
# cl <- rbind(cl.gps, cl.other, cl.logical.check1, cl.logical.check2)  # uncomment if gps checks are used
cl <- rbind(cl.other, cl.logical.check1, cl.logical.check2)  # uncomment if gps checks are not used
write.xlsx(cl, paste0("output/checking/", assessment.month, "_cleaning_log_checking.xlsx"))

# split follow up requests
for (p in unique(cleaning.log$partner)){
  save.follow.up.requests(cl=filter(cleaning.log, partner==p), partner=p)
}
