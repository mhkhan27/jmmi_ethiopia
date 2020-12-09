##########################################################################################################
# GENERIC FUNCTIONS
##########################################################################################################
# get list of all items in the tool rather than water
get.all.items <- function(){
  return((tool.choices %>% filter(list_name=="all_items", name!="water"))$name)
}
# get list of all items in the tool with their standard units
get.list.std.units <- function(){
  standard.units <- do.call(rbind, lapply(all.items, function(x) {
    variable <- paste0(x, "_standard_unit")
    label <- tool.survey[!is.na(tool.survey$name) & tool.survey$name==variable, "label::English"]
    unit <- str_remove(str_split(as.character(label), "in units of ")[[1]][2], "\\?")
    return(data.frame(item=x, standard.unit=str_sub(unit, 3, str_length(unit)), quantity=1))}))
  return(standard.units)
}
# get list of nonstandard units in the dataset
get.list.nstd.units <- function(){
  cols <- colnames(raw)[str_ends(colnames(raw), "_nonstandard_unit")]
  non.standard.units <- raw[c("uuid", cols)] %>% 
    pivot_longer(cols=all_of(cols), names_to="variable", values_to="old.value") %>% 
    filter(!is.na(old.value)) %>% 
    left_join(select(raw, uuid, adm1_region), by="uuid")
  non.standard.units$adm1_region.name <- apply(
    non.standard.units, 1, 
    function(x) as.character(tool.choices[tool.choices$name==as.character(x["adm1_region"]), "label::English"]))
  return(non.standard.units)
}
# get value from generic data.frame
get.value <- function(df, idx.col, idx, col){
  col.type <- class(df[[col]])
  if (col.type=="numeric") res <- as.numeric(df[df[[idx.col]]==idx, col])
  else res <- as.character(df[df[[idx.col]]==idx, col])
  if (length(res)==1) return(res)
  else stop("Multiple matches")
}
# convert the given columns of a data.frame to double
to.double <- function(df, columns){
  for (col in columns) df[[col]] <- as.double(df[[col]])
  return(df)
}
# get list of numeric columns
get.numeric.columns <- function(){
  cols <- c(as.character(lapply(all.items, function(x) paste0(x, "_nonstandard_unit_g"))),
            as.character(lapply(all.items, function(x) paste0(x, "_nonstandard_unit_ml"))),
            as.character(lapply(all.items, function(x) paste0(x, "_price"))),
            as.character(lapply(all.items, function(x) paste0(x, "_stock_days"))),
            as.character(lapply(all.items, function(x) paste0(x, "_resupply_days"))),
            "truck_capacity", 
            "water_price_base", "water_price_5km", "water_price_10km")
  return(cols)
}
# apply cleaning log changes to a data.frame
apply.changes <- function(df, cleaning.log){
  for (r in 1:nrow(cleaning.log)){
    uuid <- as.character(cleaning.log[r, "uuid"])
    variable <- as.character(cleaning.log[r, "variable"])
    if (class(df[[variable]])=="numeric") new.value <- as.numeric(cleaning.log[r, "new.value"])
    else new.value <- as.character(cleaning.log[r, "new.value"])
    df[df$uuid==uuid, variable] <- new.value
  }
  return(df)
}
# calculate mode
get.mode <- function(x){
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
# generate data.frame with number of prices for each item at all admin level
get.num.prices <- function(data){
  cols <- colnames(data)[str_detect(colnames(data), "price_per_unit")]
  df0 <- data %>% select(all_of(cols)) %>% summarise_all(~sum(!is.na(.))) %>% mutate(admin="ET")
  df1 <- data %>% group_by(adm1_region) %>% select(all_of(cols)) %>% summarise_all(~sum(!is.na(.))) %>% 
    rename(admin=adm1_region)
  df2 <- data %>% group_by(adm2_zone) %>% select(all_of(cols)) %>% summarise_all(~sum(!is.na(.))) %>% 
    rename(admin=adm2_zone)
  df3 <- data %>% group_by(adm3_woreda) %>% select(all_of(cols)) %>% summarise_all(~sum(!is.na(.))) %>% 
    rename(admin=adm3_woreda)
  return(rbind(df0, df1, df2, df3) %>% relocate(admin, .before=1))
}

##########################################################################################################
# FUNCTIONS FOR DATA CHECKING
##########################################################################################################
# function used in logical check 1 to return the list of items unavailable for a given survey
check.availability <- function(r){
  if (!is.na(r["food_sold"]) & r["food_sold"]!="none"){
    food.sold <- str_split(r["food_sold"], " ")[[1]]
    food.sold[food.sold=="goat_meat"] <- "goatmeat"
    food.sold[food.sold=="vegetables_leafy_darkgreen"] <- "vegetables"
    food.sold[food.sold=="cooking_oil"] <- "cookingoil"
    cols.food <- as.character(lapply(food.sold, function(x) paste0("availability_food_", x)))
  } else cols.food <- c()
  if (!is.na(r["hygiene_sold"]) & r["hygiene_sold"]!="none"){
    hygiene.sold <- str_split(r["hygiene_sold"], " ")[[1]]
    hygiene.sold[hygiene.sold=="bath_soap"] <- "bathsoap"
    cols.hygiene <- as.character(lapply(hygiene.sold, function(x) paste0("availability_hygiene_", x)))
  } else cols.hygiene <- c()
  cols <- c(cols.food, cols.hygiene)
  if (length(cols) > 0){
    df <- pivot_longer(data.frame(as.list(r[c("uuid", all_of(cols))])), 
                       cols=all_of(cols), names_to="variable", values_to="old.value") %>% 
      filter(old.value=="unavailable")
    df$item <- as.character(lapply(df$variable, function(x) str_split(x, "_")[[1]][3]))
    return(df)
  } else return(data.frame())
}
# generates a cleaning log to remove the water responses (used in logical check 2)
remove.water.responses <- function(uuid){
  cols <- colnames(raw.step1)[str_detect(colnames(raw.step1), "water") & 
                                !(colnames(raw.step1) %in% c("type_vendor/water", 
                                                             "availability_hygiene_water"))]
  cols <- c("truck_capacity", cols)
  cl <- rbind(
    data.frame(uuid=uuid, variable="type_vendor", 
               new.value=trimws(str_remove(get.value(raw.step1, "uuid", uuid, "type_vendor"), "water"))),
    data.frame(uuid=uuid, variable="type_vendor/water", new.value="0"),
    do.call(rbind, lapply(cols, function(x) data.frame(uuid=uuid, variable=x, new.value=NA))))
  return(cl)
}
# generate a cleaning log to recode the other responses
get.entry.other.changes <- function(uuid, item, standard_unit, nonstandard_unit, 
                                    nonstandard_unit_g, nonstandard_unit_ml, nonstandard_unit_other){
  cl <- rbind(data.frame(uuid=uuid, variable=paste0(item, "_standard_unit"), new.value=standard_unit),
              data.frame(uuid=uuid, variable=paste0(item, "_nonstandard_unit"), new.value=nonstandard_unit),
              data.frame(uuid=uuid, variable=paste0(item, "_nonstandard_unit_g"), new.value=nonstandard_unit_g),
              data.frame(uuid=uuid, variable=paste0(item, "_nonstandard_unit_ml"), new.value=nonstandard_unit_ml),
              data.frame(uuid=uuid, variable=paste0(item, "_nonstandard_unit_other"), new.value=nonstandard_unit_other))
  return(cl)
}
# generate a cleaning log to remove the other responses for food items
remove.food.item <- function(df, uuid, item){
  cols <- colnames(df)[str_starts(colnames(df), item)]
  cl <- rbind(
    data.frame(uuid=uuid, variable=paste0("food_sold/", item), new.value="0"),
    do.call(rbind, lapply(cols, function(x) return(data.frame(uuid=uuid, variable=x, new.value=NA)))))
  return(cl)
}
# save the follow-up requests for a give partner
save.follow.up.requests <- function(cl, partner){
  # save follow-up requests
  wb <- createWorkbook()
  addWorksheet(wb, "Follow-up")
  writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)
  col.id <- which(colnames(cl)=="issue")
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows=1:(dim(cl)[1]+1), cols=col.id)
  setColWidths(wb, "Follow-up", cols=col.id, widths=35)
  setColWidths(wb, "Follow-up", cols=c(2, 4, 5, 6, 7, 8, 12, 13), widths=13)
  setColWidths(wb, "Follow-up", cols=c(10, 11, 14), widths=22)
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE", halign="center",
                           border="TopBottomLeftRight", borderColour="#000000")
  addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:dim(cl)[2])
  col.id <- which(colnames(cl)=="old.value")
  random.color <- ""
  for (r in 2:dim(cl)[1]){
    if(as.character(cl[r, "check.id"])=="Outlier.price" &
       as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"]) &
       !is.na(as.character(cl[r, "item"])) & !is.na(as.character(cl[r - 1, "item"])) &
       as.character(cl[r, "item"])==as.character(cl[r-1, "item"])){
      if (random.color == "") random.color <- randomColor(1, luminosity = "light")
      style <- createStyle(fgFill=random.color)
      style.input <- createStyle(fgFill=random.color,
                                 border="TopBottomLeftRight", borderColour="#000000")
      addStyle(wb, "Follow-up", style=style, rows=r:(r+1), cols=col.id)
      addStyle(wb, "Follow-up", style=style.input, rows=r:(r+1), cols=col.id+1)
      addStyle(wb, "Follow-up", style=style.input, rows=r:(r+1), cols=col.id+2)
    } else random.color=""
  }
  filename <- paste0("output/fu_requests/", assessment.month, "_", partner, "_follow_up_requests.xlsx")
  saveWorkbook(wb, filename, overwrite = TRUE)
}

##########################################################################################################
# FUNCTIONS TO CALCULATE AND TEST THE PRICE PER UNIT
##########################################################################################################
# convert price to price_per_unit
calculate_price_per_unit <- function(item, standard_unit, non_standard_unit, unit_g, unit_ml, price, test=F){
  case_when(
    standard_unit == "yes" ~ price,
    non_standard_unit == "gram" & item == "bath_soap" ~ price / unit_g * 125,
    non_standard_unit == "gram" & item == "bleach" ~ price / unit_g * 4,
    non_standard_unit == "gram" & item != "cooking_oil" ~ price / unit_g * 1000,
    non_standard_unit == "millilitre" & item == "cooking_oil" ~ price / unit_ml * 1000,
    non_standard_unit == "medeb" & item == "vegetables_leafy_darkgreen" ~ price / 0.25,
    non_standard_unit == "bundle_small" & item == "vegetables_leafy_darkgreen" ~ price / 0.25,
    non_standard_unit == "galaan" & item %in% c("maize", "sorghum", "wheat") ~ price / 1.15,
    non_standard_unit == "koombo" & item == "rice" ~ price / 0.6,
    non_standard_unit == "piece" & item == "bath_soap" ~ price,
    non_standard_unit == "piece" & item == "vegetables_leafy_darkgreen" ~ price / 0.5,
    non_standard_unit == "sachet" & item == "bleach" ~ price / 2 * 4,  # sachet of 2 grams
    non_standard_unit == "bundle_large" & item == "enset" ~ price / 10,  # bundle_large = 10 kg
    non_standard_unit == "cup_glass" & item == "cooking_oil" ~ price / 250 * 1000,
    !is.na(standard_unit) & test ~ -1,
    TRUE ~ NA_real_)
}
# function to calculate price_per_unit for all items in the tool
add.price.per.unit <- function(df, test=F){
  for (item in all.items){
    df[[paste0(item, "_price_per_unit")]] <- 
      calculate_price_per_unit(item,
                               df[[paste0(item, "_standard_unit")]],
                               df[[paste0(item, "_nonstandard_unit")]],
                               df[[paste0(item, "_nonstandard_unit_g")]],
                               df[[paste0(item, "_nonstandard_unit_ml")]],
                               df[[paste0(item, "_price")]], test)
  }
  # calculate price per unit for water
  df$water_price_per_unit <- df$water_price_base/df$truck_capacity
  df$water_5km_price_per_unit <- df$water_price_5km/df$truck_capacity
  df$water_10km_price_per_unit <- df$water_price_10km/df$truck_capacity
  return(df)
}
# generate a data.frame with the columns used to calculate the price_per_unit of a given item (to be inspected)
test.price.per.unit <- function(df, item){
  if (item=="water"){
    df <- df %>% 
      select("truck_capacity", "water_price_base", "water_price_5km", "water_price_10km",
             "water_price_per_unit", "water_5km_price_per_unit", "water_10km_price_per_unit") %>% 
      filter(!is.na(truck_capacity))
  } else{
    df <- df %>% 
      select(paste0(item, "_standard_unit"), 
             paste0(item, "_nonstandard_unit"),
             paste0(item, "_nonstandard_unit_g"),
             paste0(item, "_nonstandard_unit_ml"),
             paste0(item, "_nonstandard_unit_other"),
             paste0(item, "_price"),
             paste0(item, "_price_per_unit")) %>% 
      filter(!is.na(!!sym(paste0(item, "_standard_unit"))))
  }
  return(df)
}

##########################################################################################################
# FUNCTIONS FOR OUTLIERS DETECTION
##########################################################################################################
# function to detect outliers
detect.outliers <- function(df, method="sd", n.sd=3){
  res <- data.frame()
  for (col in colnames(df)[colnames(df)!="uuid"]){
    df.temp <- data.frame(uuid=df$uuid, value=as.numeric(df[[col]])) %>% filter(!is.na(value))
    if (method=="sd-linear"){
      df.temp <- df.temp %>%
        mutate(is.outlier.high=value > mean(value, na.rm=T) + n.sd*sd(value, na.rm=T),
               is.outlier.low=value < mean(value, na.rm=T) - n.sd*sd(value, na.rm=T))
    } else if (method=="sd-log"){
      df.temp <- df.temp %>%
        mutate(col.log=log(value),
               is.outlier.high=col.log > mean(col.log, na.rm=T) + n.sd*sd(col.log, na.rm=T),
               is.outlier.low=col.log < mean(col.log, na.rm=T) - n.sd*sd(col.log, na.rm=T))
    } else if (method=="iqr-linear") {
      df.temp <- df.temp %>%
        mutate(is.outlier.high=value > quantile(value, 0.75) + 1.5*IQR(value),
               is.outlier.low=value < quantile(value, 0.25) - 1.5*IQR(value))
    } else if (method=="iqr-log") {
      df.temp <- df.temp %>%
        mutate(col.log=log(value),
               is.outlier.high=col.log > quantile(col.log, 0.75) + 1.5*IQR(col.log),
               is.outlier.low=col.log < quantile(col.log, 0.25) - 1.5*IQR(col.log))
    } else stop("Method unknown")
    df.temp <- df.temp %>% 
      pivot_longer(c("is.outlier.high", "is.outlier.low"), 
                   names_to="outlier.type", values_to="is.outlier") %>% 
      filter(is.outlier) %>% 
      mutate(variable=col, old.value=value,
             outlier.type=ifelse(outlier.type=="is.outlier.high", "high", "low")) %>% 
      select(uuid, variable, old.value, outlier.type)
    res <- rbind(res, df.temp)
  }
  return(res)
}
# generate a cleaning log based on the detected outliers
create.outliers.cleaning.log <- function(outliers){
  cleaning.log.outliers <- data.frame()
  for (r in 1:nrow(outliers)){
    uuid <- as.character(outliers[r, "uuid"])
    variable <- as.character(outliers[r, "variable"])
    item <- str_split(variable, "_price_per_unit")[[1]][1]
    outlier.type <- as.character(outliers[r, "outlier.type"])
    # get price variable
    if (variable=="water_price_per_unit") price.variable <- "water_price_base"
    else if (variable=="water_5km_price_per_unit") price.variable <- "water_price_5km"
    else if (variable=="water_10km_price_per_unit") price.variable <- "water_price_10km"
    else price.variable <- paste0(item, "_price")
    # get reported unit and quantity
    if (item=="water"){
      unit <- "litre (truck capacity)"
      quantity <- get.value(raw.step1, "uuid", uuid, "truck_capacity")
    } else{
      if (get.value(raw.step1, "uuid", uuid, paste0(item, "_standard_unit"))=="yes"){
        unit <- as.character(standard.units[standard.units$item==item, "standard.unit"])
        quantity <- as.numeric(standard.units[standard.units$item==item, "quantity"])
      } else{
        if (get.value(raw.step1, "uuid", uuid, paste0(item, "_nonstandard_unit"))=="gram"){
          unit <- "gram"
          quantity <- get.value(raw.step1, "uuid", uuid, paste0(item, "_nonstandard_unit_g"))
        } else if (get.value(raw.step1, "uuid", uuid, paste0(item, "_nonstandard_unit"))=="millilitre"){
          unit <- "millilitre"
          quantity <- get.value(raw.step1, "uuid", uuid, paste0(item, "_nonstandard_unit_ml"))
        } else {
          unit <- get.value(raw.step1, "uuid", uuid, paste0(item, "_nonstandard_unit"))
          quantity <- 1
        }
      }
    }
    cleaning.log.outliers <- rbind(cleaning.log.outliers,
                                   data.frame(uuid=uuid, item=item,
                                              variable="quantity", old.value=quantity, issue=outlier.type),
                                   data.frame(uuid=uuid, item=item,
                                              variable="unit", old.value=unit, issue=outlier.type),
                                   data.frame(uuid=uuid, item=item,
                                              variable="price",
                                              old.value=get.value(raw.step1, "uuid", uuid, price.variable),
                                              issue=outlier.type))
  }
  cleaning.log.outliers$issue <- ifelse(cleaning.log.outliers$issue=="high",
                                        "Price seems to be too high. Please check/confirm the quantity, unit, and price.",
                                        "Price seems to be too low. Please check/confirm the quantity, unit, and price.")
  cleaning.log.outliers$check.id <- "Outlier.price"
  return(cleaning.log.outliers)
}
# generate a boxplot for all prices highligthing the outliers
generate.price.outliers.boxplot <- function(){
  df <- raw.step1[, c("uuid", "adm1_region", "adm2_zone", "adm3_woreda", all_of(cols.outliers1))] %>% 
    pivot_longer(cols=all_of(cols.outliers1), names_to="variable", values_to="value") %>% 
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
  ggsave(paste0("output/checking/", assessment.month, "_outlier_analysis_prices.pdf"), g, 
         width = 40, height = 40, units = "cm", device="pdf")
}
# generate a boxplot for all generic numeric variables highligthing the outliers
generate.generic.outliers.boxplot <- function(){
  df <- raw.step1[, c("uuid", "adm1_region", "adm2_zone", "adm3_woreda", all_of(cols.outliers.gen))] %>% 
    pivot_longer(cols=all_of(cols.outliers.gen), names_to="variable", values_to="value") %>% 
    filter(!is.na(value))
  cl <- cleaning.log.outliers.generic %>% select(uuid, variable) %>% mutate(detected=T)
  df <- left_join(df, cl, by=c("uuid", "variable")) %>% 
    mutate(detected=ifelse(is.na(detected), F, detected), adm0="national")
  f.alpha <- function(x) return(ifelse(x, 1, 0))
  f.colour <- function(x) return(ifelse(x, "#FF0000", "#00FF00"))
  num_variables <- length(unique(df$variable))
  g <- ggplot(df) +
    geom_boxplot(aes(x=adm0, y=(as.numeric(value)))) + ylab("Values") +
    geom_point(aes(x=adm0, y=(as.numeric(value))), 
               alpha=f.alpha(df$detected), colour=f.colour(df$detected)) +
    facet_wrap(~variable, scales="free_y", nrow = ceiling(num_variables/4), ncol = 4)
  ggsave(paste0("output/checking/", assessment.month, "_outlier_analysis_generic.pdf"), g, 
         width = 40, height = 40, units = "cm", device="pdf")
}

##########################################################################################################
# FUNCTIONS FOR DATA EDITING
##########################################################################################################
get.entry.log <- function(uuid, item, std.unit, nstd.unit, nstd.unit.g, nstd.unit.ml, price){
  return(rbind(data.frame(uuid=uuid, variable=paste0(item, "_standard_unit"), new.value=std.unit),
               data.frame(uuid=uuid, variable=paste0(item, "_nonstandard_unit"), new.value=nstd.unit),
               data.frame(uuid=uuid, variable=paste0(item, "_nonstandard_unit_g"), new.value=nstd.unit.g),
               data.frame(uuid=uuid, variable=paste0(item, "_nonstandard_unit_ml"), new.value=nstd.unit.ml),
               data.frame(uuid=uuid, variable=paste0(item, "_nonstandard_unit_other"), new.value=NA),
               data.frame(uuid=uuid, variable=paste0(item, "_price"), new.value=price)))
}
get.entry.log.water <- function(uuid, quantity, price){
  return(rbind(data.frame(uuid=uuid, variable="truck_capacity", new.value=quantity),
               data.frame(uuid=uuid, variable="water_price_base", new.value=price)))
}
get.cleaning.log <- function(x, y){
  uuid <- as.character(y$uuid)
  item <- as.character(y$item)
  quantity.old <- as.numeric(x[x$variable=="quantity", "old.value"])
  quantity.new <- as.numeric(x[x$variable=="quantity", "new.value"])
  unit.old <- as.character(x[x$variable=="unit", "old.value"])
  unit.new <- tolower(as.character(x[x$variable=="unit", "new.value"]))
  unit.new <- ifelse(unit.new=="kg", "kilogram", unit.new)
  unit.new <- ifelse(unit.new=="liter", "litre", unit.new)
  price.old <- as.numeric(x[x$variable=="price", "old.value"])
  price.new <- as.numeric(x[x$variable=="price", "new.value"])
  cl <- data.frame()
  if (is.na(quantity.new) & is.na(unit.new) & is.na(price.new)){
    # all new values are NAs -> remove price
    cl <- rbind(cl, get.entry.log(uuid, item, NA, NA, NA, NA, NA))
  } else if (quantity.old!=quantity.new | unit.old!=unit.new | price.old!=price.new){
    if (item=="water"){
      cl <- rbind(cl, get.entry.log.water(uuid, item, "no", "gram", quantity.new, NA, price.new))
    } else if (unit.new=="gram"){
      cl <- rbind(cl, get.entry.log(uuid, item, "no", "gram", quantity.new, NA, price.new))
    } else if (unit.new=="millilitre"){
      cl <- rbind(cl, get.entry.log(uuid, item, "no", "millilitre", NA, quantity.new, price.new))
    } else if (unit.new==as.character(standard.units[standard.units$item==item, "standard.unit"])){
      cl <- rbind(cl, get.entry.log(uuid, item, "yes", NA, NA, NA, price.new/quantity.new))
    } else if (unit.new %in% as.character(tool.choices[tool.choices$list_name=="nonstandard_units",]$name)){
      cl <- rbind(cl, get.entry.log(uuid, item, "no", unit.new, NA, NA, price.new/quantity.new))
    } else stop("unit.new unknown")
  }
  return(cl)
}

##########################################################################################################
# FUNCTIONS FOR THE ANALYSIS
##########################################################################################################
# function to aggregated availability variables at woreda level
get.availability <- function(x){
  if ("fully_available" %in% x) return("available")
  if ("limited" %in% x) return("limited")
  if ("unavailable" %in% x) return("unavailable")
  return("no data")
}
# function to aggregate 0/1 variables at woreda level
get.at.least.one <- function(x) if ("1" %in% x) return("1") else return("0")
# function to analyse select_one variables
analyse.select_one <- function(df, admin.col, variable){
  d <- df %>% 
    filter(!is.na(!!sym(variable))) %>% 
    group_by(!!sym(admin.col), !!sym(variable)) %>%
    summarise(n = n()) %>% 
    mutate(proportion = n / sum(n)) %>% ungroup() %>% 
    select(-n) %>% 
    mutate(!!variable := paste0(variable, ".", !!sym(variable))) %>% 
    pivot_wider(names_from=variable, values_from=proportion) %>% 
    mutate_if(is.numeric, ~replace_na(., 0))
  return(d)
}
# function to analyse select_multiple variables
analyse.select_multiple <- function(df, admin.col, variable){
  cols <- colnames(df)[str_starts(colnames(df), variable)]
  d <- df %>% 
    filter(!is.na(!!sym(cols[1]))) %>% 
    group_by(!!sym(admin.col)) %>%
    mutate(i=1) %>% 
    summarise_all(~sum(as.numeric(.))/sum(i)) %>% 
    select(-i)
  colnames(d) <- str_replace(colnames(d), "/", ".")
  return(d)
}
# function to calculate median of numeric variable
analyse.numeric.median <- function(df, admin.col, variable){
  d <- df %>% 
    filter(!is.na(!!sym(variable))) %>% 
    group_by(!!sym(admin.col)) %>%
    summarise(!!variable := median(!!sym(variable)))
  return(d)
}
# function to calculate sum of numeric variable
analyse.numeric.sum <- function(df, admin.col, variable){
  d <- df %>% 
    filter(!is.na(!!sym(variable))) %>% 
    group_by(!!sym(admin.col)) %>%
    summarise(!!variable := sum(!!sym(variable)))
  return(d)
}
# function to run the analysis on all indicators
run.analysis <- function(data, admin.output){
  indicators <- data.frame(col.name=colnames(data)) %>% 
    filter(!(col.name %in% c("adm1_region", "adm2_zone", "adm3_woreda")))
  indicators$variable <- apply(indicators, 1, function(x) str_split(x, "/")[[1]][1])
  if (admin.output=="adm0_nation") data$adm0_nation="ET"
  r <- lapply(unique(indicators$variable), function(variable){
    if (variable %in% tool.survey$name){
      q.type <- str_split(get.value(tool.survey, "name", variable, "type"), " ")[[1]][1]
    } else if (str_detect(variable, "_price_per_unit")) q.type <- "numeric"
    else if (str_detect(variable, "number_prices")) q.type <- "number_prices"
    else stop("Variable unknown")
    cols <- indicators$col.name[str_starts(indicators$col.name, variable)]
    df <- data[c(admin.output, all_of(cols))]
    if (q.type=="select_one") return(analyse.select_one(df, admin.output, variable))
    if (q.type=="select_multiple") return(analyse.select_multiple(df, admin.output, variable))
    if (q.type %in% c("numeric", "integer")) return(analyse.numeric.median(df, admin.output, variable))
    if (q.type=="number_prices") return(analyse.numeric.sum(df, admin.output, variable))
    stop("Uncatched error")
  })
  return(r %>% reduce(full_join, by=admin.output) %>% rename(admin=admin.output))
}
# function to generate boxplot
analysis.boxplot <- function(data, category){
  boxplot_statistics <- function(x) {
    r <- quantile(x, probs = c(0.00, 0.25, 0.5, 0.75, 1))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    return(r)
  }
  get.number.label <- function(item, value){
    rounding <- ifelse(str_starts(item, "Water"), 2, 0)
    df <- data.frame(value=value, rounding=rounding)
    return(apply(df, 1, function(x) 
      return(str_pad(format(x["value"], digits=x["rounding"]), width=3, side="left"))))
  }
  
  medians <- plyr::ddply(data, "item", summarise, med = median(price_per_unit, na.rm=T))
  mins <- plyr::ddply(data, "item", summarise, min = min(price_per_unit, na.rm=T))
  maxs <- plyr::ddply(data, "item", summarise, max = max(price_per_unit, na.rm=T))
  
  num.items <- length(unique(data$item))
  p <- ggplot(data=data, aes(x=reorder(item, -price_per_unit, median), y=price_per_unit, width=0.3)) +
    stat_summary(fun.data = boxplot_statistics, geom="boxplot", fill = "#D1D3D4") +
    theme_bw() + 
    geom_text(data=mins, 
              aes(x=item, y=min, label=get.number.label(item, min)),
              size=2.5, vjust=1.5) +
    geom_text(data=medians, 
              aes(x=item, y=med, label=get.number.label(item, med)),
              size=2.5, hjust = ifelse(num.items < 5, -.7, -1)) +
    geom_text(data=maxs, 
              aes(x=item, y=max, label=get.number.label(item, max)),
              size=2.5, vjust =-0.5) +
    theme(axis.text.x = element_text(angle = 0, size = 7, hjust = 0.5 ),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  ggsave(paste0("output/analysis/", assessment.month, "_analysis_boxplot_", category, ".pdf"), 
         width=2*num.items, height=12, units="cm", device="pdf")
}
# function to calculate basket cost (full or food basket)
calculate.basket.cost <- function(df, type){
  if (!(type %in% c("full", "food"))) stop("Type of basket is unknown")
  is.full <- ifelse(type=="full", 1, 0)
  col.name <- ifelse(type=="full", "full.basket.cost", "food.basket.cost")
  df <- df %>% 
    mutate(!!col.name := 
             1 * maize_price_per_unit +
             1 * sorghum_price_per_unit +
             1 * teff_price_per_unit +
             1 * wheat_price_per_unit +
             1 * barley_price_per_unit +
             1 * enset_price_per_unit +
             1 * rice_price_per_unit +
             1 * beef_price_per_unit +
             1 * mutton_price_per_unit +
             1 * goat_meat_price_per_unit +
             1 * vegetables_leafy_darkgreen_price_per_unit +
             1 * cooking_oil_price_per_unit +
             is.full * 1 * bath_soap_price_per_unit +
             is.full * 1 * bleach_price_per_unit + 
             is.full * 1000 * water_price_per_unit)
  return(df)
}
# function to get the list of columns containing prices and basket costs
get.columns.prices.baskets <- function(df){
  cols <- colnames(df)
  return(cols[str_ends(cols, "price_per_unit|.basket.cost")])
}
# function to calculate time trends
calculate.time.trends <- function(analysis, num.months){
  # load previous analysis to be used as reference in the calculation of the time trend
  this.month.date <- as.Date(paste0(assessment.month, "-01"), "%Y-%m-%d")
  ref.month <- str_sub(seq(this.month.date, length=2, by=paste0("-", num.months, " month"))[2], 1, 7)
  filename.ref.analysis <- paste0("output/analysis/", ref.month, "_analysis_InDesign.xlsx")
  if (!file.exists(filename.ref.analysis)) stop(paste0(filename.ref.analysis, " not found"))
  ref.analysis <- read_excel(filename.ref.analysis, guess_max=20000)
  # determine list of columns in common between the 2 periods -> to be used in the time trends calculation
  cols <- intersect(get.columns.prices.baskets(ref.analysis), get.columns.prices.baskets(analysis))
  # calculate time trends for all selected columns
  time.trends <- do.call(cbind, lapply(cols, function(x){
    item <- ifelse(x=="JMMI.basket.cost", x, str_split(x, "_price_per")[[1]][1])
    new.col.name <- paste0("price.change.", num.months, "months.", item)
    df <- data.frame(col = round((analysis[[x]]-ref.analysis[[x]])/ref.analysis[[x]], digits=4))
    colnames(df) <- new.col.name
    return(df)
  }))
  return(analysis %>% cbind(time.trends))
}
# function to add summary columns to the analysis
calculate.summary.columns <- function(data, data.partners, admin.level){
  if (admin.level=="adm0_nation") data$adm0_nation="ET"
  number.commodities.accessed <- length(all.items) + 1
  res <- data %>% left_join(data.partners, by="uuid") %>% group_by(!!sym(admin.level)) %>% 
    summarise(summary.first.date.data.collection = min(date_of_dc),
              summary.last.date.data.collection = max(date_of_dc),
              summary.number.participating.agencies = length(unique(partner)),
              summary.number.traders.interviewed = n(),
              summary.number.markets.accessed = length(unique(marketplace)),
              summary.number.woredas.accessed = length(unique(adm3_woreda)),
              summary.number.commodities.assessed = number.commodities.accessed) %>% 
    rename(admin.pcode=!!sym(admin.level))
  return(res)
}