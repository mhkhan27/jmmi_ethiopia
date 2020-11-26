get.list.std.units <- function(){
  standard.units <- do.call(rbind, lapply(all.items, function(x) {
    variable <- paste0(x, "_standard_unit")
    label <- tool.survey[!is.na(tool.survey$name) & tool.survey$name==variable, "label::English"]
    unit <- str_remove(str_split(as.character(label), "in units of ")[[1]][2], "\\?")
    return(data.frame(item=x, standard.unit=str_sub(unit, 3, str_length(unit)), quantity=1))}))
  return(standard.units)
}

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

to.double <- function(df, columns){
  for (col in columns) df[[col]] <- as.double(df[[col]])
  return(df)
}

calculate_price_per_unit <- function(item, standard_unit, non_standard_unit, unit_g, unit_ml, price){
  case_when(
    standard_unit == "yes" ~ price,
    non_standard_unit == "gram" & item == "bath_soap" ~ price / unit_g * 125,
    non_standard_unit == "gram" & item == "bleach" ~ price / unit_g * 4,
    non_standard_unit == "gram" ~ price / unit_g * 1000,
    non_standard_unit == "millilitre" ~ price / unit_ml * 1000,
    non_standard_unit == "medeb" & item == "vegetables_leafy_darkgreen" ~ price / 0.5,
    non_standard_unit == "piece" & item == "bath_soap" ~ price,
    non_standard_unit == "piece" & item == "vegetables_leafy_darkgreen" ~ price / 0.5,
    TRUE ~ NA_real_)
}

add.price.per.unit <- function(df){
  for (item in all.items){
    df[[paste0(item, "_price_per_unit")]] <- 
      calculate_price_per_unit(item,
                               df[[paste0(item, "_standard_unit")]],
                               df[[paste0(item, "_nonstandard_unit")]],
                               df[[paste0(item, "_nonstandard_unit_g")]],
                               df[[paste0(item, "_nonstandard_unit_ml")]],
                               df[[paste0(item, "_price")]])
  }
  # calculate price per unit for water
  df$water_price_per_unit <- df$water_price_base/df$truck_capacity
  df$water_price_per_unit_5km <- df$water_price_5km/df$truck_capacity
  df$water_price_per_unit_10km <- df$water_price_10km/df$truck_capacity
  return(df)
}

create.outliers.cleaning.log <- function(outliers){
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
      quantity <- get.value(raw.step1, uuid, "truck_capacity")
    } else{
      if (get.value(raw.step1, uuid, paste0(item, "_standard_unit"))=="yes"){
        unit <- as.character(standard.units[standard.units$item==item, "standard.unit"])
        quantity <- as.numeric(standard.units[standard.units$item==item, "quantity"])
      } else{
        if (get.value(raw.step1, uuid, paste0(item, "_nonstandard_unit"))=="gram"){
          unit <- "gram"
          quantity <- get.value(raw.step1, uuid, paste0(item, "_nonstandard_unit_g"))
        } else if (get.value(raw.step1, uuid, paste0(item, "_nonstandard_unit"))=="millilitre"){
          unit <- "millilitre"
          quantity <- get.value(raw.step1, uuid, paste0(item, "_nonstandard_unit_ml"))
        } else {
          unit <- get.value(raw.step1, uuid, paste0(item, "_nonstandard_unit"))
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
                                              old.value=get.value(raw.step1, uuid, price.variable)))
  }
  cleaning.log.outliers$issue <- "Price seems to be too low or to high. Please check/confirm the quantity, unit, and price."
  cleaning.log.outliers$check.id <- "Outlier.price"
  return(cleaning.log.outliers)
}

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
  ggsave(paste0("output/", assessment.month, "_outlier_analysis_prices.pdf"), g, 
         width = 40, height = 40, units = "cm", device="pdf")
}

generate.generic.outliers.boxplot <- function(){
  df <- raw.step1[, c("uuid", "adm1_region", "adm2_zone", "adm3_woreda", all_of(cols.outliers.gen))] %>% 
    pivot_longer(cols=all_of(cols.outliers.gen), names_to="variable", values_to="value") %>% 
    filter(!is.na(value))
  cl <- cleaning.log.outliers.generic %>% select(uuid, variable) %>% mutate(detected=T)
  df <- left_join(df, cl, by=c("uuid", "variable")) %>% 
    mutate(detected=ifelse(is.na(detected), F, detected), adm0="national")
  f.alpha <- function(x) return(ifelse(x, 1, 0))
  f.colour <- function(x) return(ifelse(x, "#FF0000", "#00FF00"))
  g <- ggplot(df) +
    geom_boxplot(aes(x=adm0, y=(as.numeric(value)))) + ylab("Values") +
    geom_point(aes(x=adm0, y=(as.numeric(value))), 
               alpha=f.alpha(df$detected), colour=f.colour(df$detected)) +
    facet_wrap(~variable, scales="free_y", nrow = 6, ncol = 4)
  ggsave(paste0("output/", assessment.month, "_outlier_analysis_generic.pdf"), g, 
         width = 40, height = 40, units = "cm", device="pdf")
}

get.value <- function(df, uuid, col){
  col.type <- class(df[[col]])
  if (col.type=="numeric") return(as.numeric(df[df$uuid==uuid, col]))
  else return(as.character(df[df$uuid==uuid, col]))
}

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

detect.outliers <- function(df, method="sd", n.sd=3){
  res <- data.frame()
  for (col in colnames(df)[colnames(df)!="uuid"]){
    df.temp <- data.frame(uuid=df$uuid, value=as.numeric(df[[col]])) %>% filter(!is.na(value))
    if (method=="sd-linear"){
      df.temp <- df.temp %>%
        mutate(is.outlier=ifelse(value > mean(value, na.rm=T) + n.sd*sd(value, na.rm=T) | 
                                   value < mean(value, na.rm=T) - n.sd*sd(value, na.rm=T), T, F))
    } else if (method=="sd-log"){
      df.temp <- df.temp %>%
        mutate(col.log=log(value),
               is.outlier=ifelse(col.log > mean(col.log, na.rm=T) + n.sd*sd(col.log, na.rm=T) | 
                                   col.log < mean(col.log, na.rm=T) - n.sd*sd(col.log, na.rm=T), T, F))
    } else if (method=="iqr-linear") {
      df.temp <- df.temp %>%
        mutate(is.outlier=ifelse(value > quantile(value, 0.75) + 1.5*IQR(value) |
                                   value < quantile(value, 0.25) - 1.5*IQR(value), T, F))
    } else if (method=="iqr-log") {
      df.temp <- df.temp %>%
        mutate(col.log=log(value),
               is.outlier=ifelse(col.log > quantile(col.log, 0.75) + 1.5*IQR(col.log) |
                                   col.log < quantile(col.log, 0.25) - 1.5*IQR(col.log), T, F))
    } else stop("Method unknown")
    df.temp <- filter(df.temp, is.outlier) %>% 
      mutate(variable=col, old.value=value) %>%
      select(uuid, variable, old.value)
    res <- rbind(res, df.temp)
  }
  return(res)
}

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
    df$item <- as.character(lapply(df$variable, function(x) str_split(x, "_")[[1]][3]))
    return(df)
  } else return(data.frame())
}

save.follow.up.requests <- function(cl){
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
  saveWorkbook(wb, filename.out.fu.requests, overwrite = TRUE)
}

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
  unit.new <- as.character(x[x$variable=="unit", "new.value"])
  price.old <- as.numeric(x[x$variable=="price", "old.value"])
  price.new <- as.numeric(x[x$variable=="price", "new.value"])
  cl <- data.frame()
  if (quantity.old!=quantity.new | unit.old!=unit.new | price.old!=price.new){
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
    }
  }
  return(cl)
}
