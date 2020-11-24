to.double <- function(df, columns){
  for (col in columns) df[[col]] <- as.double(df[[col]])
  return(df)
}

get.value <- function(df, uuid, col){
  col.type <- class(df[[col]])
  if (col.type=="numeric") return(as.numeric(df[df$uuid==uuid, col]))
  else return(as.character(df[df$uuid==uuid, col]))
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
    } else if (method=="iqr") {
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

save.follow.up.requests <- function(cl){
  # save follow-up requests
  wb <- createWorkbook()
  addWorksheet(wb, "Follow-up")
  writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)
  col.id <- which(colnames(cl)=="issue")
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows=1:(dim(cl)[1]+1), cols=col.id)
  setColWidths(wb, "Follow-up", cols=col.id, widths=35)
  setColWidths(wb, "Follow-up", cols=c(2, 4, 5, 6, 7, 12, 13), widths=13)
  setColWidths(wb, "Follow-up", cols=c(10, 11, 14), widths=22)
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE", halign="center",
                           border="TopBottomLeftRight", borderColour="#000000")
  addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:dim(cl)[2])
  col.id <- which(colnames(cl)=="old.value")
  random.color <- ""
  for (r in 2:dim(cl)[1]){
    if(as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"]) &
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
