
drawLine <- function(x) {
  if (missing(x)) 
    x <- options()$width
  line <- paste0(
    paste0(
      rep("=", times = min(x, options()$width)
    ), collapse = ""), "\n")
  return(line)
}

showStatTableList <- function(api_key) {
  df <- statTableList(api_key = api_key)
  nc_srch <- max(nchar(df$srch_yn))
  nc_code <- max(nchar(df$stat_code))
  nc_name <- max(nchar(df$stat_name))
  iter <- nc_srch + nchar(" | ") + nc_code + nchar(" | ") + nc_name
  line <- drawLine(min(iter, options()$width))
  result <- paste0(
    paste0(
      str_pad(df$srch_yn, width = nc_srch, pad = " ", side = "right"),
      " | ",
      str_pad(df$stat_code, width = nc_code, pad = " ", side = "right"),
      " | ", 
      df$stat_name
    ), collapse = "\n")
  cat(line)
  cat(result, "\n")
  cat(line)
}

checkStatSearchColumns <- function(df) {
  .statSearchColumns <- c(
    "stat_code" , "stat_name", 
    "item_code1", "item_name1",
    "item_code2", "item_name2", 
    "item_code3", "item_name3",
    "item_code4", "item_name4",
    "time", "data_value", "unit_name"
  )
  check <- all(colnames(df) %in% .statSearchColumns)
  return(check)
}

orderStatSearchColumns <- function(df) {
  .statSearchColumns <- c(
    "stat_code" , "stat_name", 
    "item_code1", "item_name1",
    "item_code2", "item_name2", 
    "item_code3", "item_name3",
    "item_code4", "item_name4",
    "time", "data_value", "unit_name"
  )
  if (checkStatSearchColumns(df))
    df <<- df[, .statSearchColumns]
}
