
# cycle date format -------------------------------------------------------

cycleDateFormatTest <- function(x, cycle) {
  if (cycle == "D") {
    if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
    } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
      yearmon <- substr(x, 1, 6)
      day <- switch(substr(x, 7, 8), "S1" = "01", "S2" = "16")
      x <- paste0(yearmon, day)
    } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
      x <- paste0(x, "01")
    } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      monday <- switch(substr(x, 5, 6), "Q1" = "0101", "Q2" = "0401", "Q3" = "0701", "Q4" = "1001")
      x <- paste0(year, monday)
    } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      monday <- switch(substr(x, 5, 6), "S1" = "0101", "S3" = "0701")
      x <- paste0(year, monday)
    } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
      x <- paste0(substr(x, 1, 4), "0101")
    } else {
      stop("invalid date format")
    }
  } else if (cycle == "SM") {
    if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
      yearmon <- substr(x, 1, 6)
      half <- if (as.numeric(substr(x, 7, 8)) < 16) "S1" else "S2"
      x <- paste0(yearmon, half)
    } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
    } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
      x <- substr(x, "S1")
    } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      mons <- switch(substr(x, 5, 6), "Q1" = "01S1", "Q2" = "04S1", "Q3" = "07S1", "Q4" = "10S1")
      x <- paste0(year, mons)
    } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      mons <- switch(substr(x, 5, 6), "S1" = "01S1", "S2" = "07S1")
      x <- paste0(year, mons)
    } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
      x <- paste0(x, "0101")
    } else {
      stop("invalid date format")
    }
  } else if (cycle == "M")  {
    if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
      x <- substr(x, 1, 6)
    } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
      x <- substr(x, 1, 6)
    } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
    } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      mon <- switch(substr(x, 6, 6), "1" = "01", "2" = "04", "3" = "07", "4" = "10")
      x <- paste0(year, mon)
    } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      mon <- switch(substr(x, 6, 6), "1" = "01", "2" = "07")
      x <- paste0(year, mon)
    } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
      x <- paste0(x, "01")
    } else {
      stop("invalid date format")
    }
  } else if (cycle == "Q") {
    if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      mon <- as.numeric(substr(x, 5, 6))
      if (mon >= 1 & mon < 4) {
        quarter <- "Q1"
      } else if (mon >= 4 & mon < 7) {
        quarter <- "Q2"
      } else if (mon >= 7 & mon < 10) {
        quarter <- "Q3"
      } else if (mon >= 10 & mon <= 12) {
        quarter <- "Q4"
      } else {
        stop("invalid date format")
      }
      x <- paste0(year, quarter)
    } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      mon <- as.numeric(substr(x, 5, 6))
      if (mon >= 1 & mon < 4) {
        quarter <- "Q1"
      } else if (mon >= 4 & mon < 7) {
        quarter <- "Q2"
      } else if (mon >= 7 & mon < 10) {
        quarter <- "Q3"
      } else if (mon >= 10 & mon <= 12) {
        quarter <- "Q4"
      } else {
        stop("invalid date format")
      }
      x <- paste0(year, quarter)
    } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      mon <- as.numeric(substr(x, 5, 6))
      if (mon >= 1 & mon < 4) {
        quarter <- "Q1"
      } else if (mon >= 4 & mon < 7) {
        quarter <- "Q2"
      } else if (mon >= 7 & mon < 10) {
        quarter <- "Q3"
      } else if (mon >= 10 & mon <= 12) {
        quarter <- "Q4"
      } else {
        stop("invalid date format")
      }
      x <- paste0(year, quarter)
    } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
    } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      quarter <- switch(substr(x, 5, 6), "S1" = "Q1", "S2" = "Q3")
      x <- paste0(year, quarter)
    } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
      x <- paste0(substr(x, 1, 4), "Q1")
    } else {
      stop("invalid date format")
    }
  } else if (cycle == "S")  {
    if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      half <- if (as.numeric(substr(x, 5, 6)) < 7) "S1" else "S2"
      x <- paste0(year, half)
    } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      half <- if (as.numeric(substr(x, 5, 6)) < 7) "S1" else "S2"
      x <- paste0(year, half)
    } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      half <- if (as.numeric(substr(x, 5, 6)) < 7) "S1" else "S2"
      x <- paste0(year, half)
    } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      half <- switch(substr(x, 5, 6), "Q1" = "S1", "Q2" = "S1", "Q3" = "S2", "Q4" = "S2")
      x <- paste0(year, half)
    } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
    } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
      x <- paste0(x, "S1")
    } else {
      stop("invalid date format")
    }
  } else if (cycle == "A") {
    if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
      x <- substr(x, 1, 4)
    } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
      x <- substr(x, 1, 4)
    } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
      x <- substr(x, 1, 4)
    } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
      x <- substr(x, 1, 4)
    } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
      x <- substr(x, 1, 4)
    } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
    } else {
      stop("invalid date format")
    }
  } else {
    stop("invalid cycle format")
  }
  return(x)
}

cycleDateFormat <- function(x, cycle) {
  if (cycle == "D") {
    if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
    } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
      yearmon <- substr(x, 1, 6)
      day <- switch(substr(x, 7, 8), "S1" = "01", "S2" = "16")
      x <- paste0(yearmon, day)
    } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
      x <- paste0(x, "01")
    } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      monday <- switch(substr(x, 5, 6), "Q1" = "0101", "Q2" = "0401", "Q3" = "0701", "Q4" = "1001")
      x <- paste0(year, monday)
    } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      monday <- switch(substr(x, 5, 6), "S1" = "0101", "S3" = "0701")
      x <- paste0(year, monday)
    } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
      x <- paste0(substr(x, 1, 4), "0101")
    } else {
      stop("invalid date format")
    }
  } else if (cycle == "SM") {
    if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
      yearmon <- substr(x, 1, 6)
      half <- if (as.numeric(substr(x, 7, 8)) < 16) "S1" else "S2"
      x <- paste0(yearmon, half)
    } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
    } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
      x <- substr(x, "S1")
    } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      mons <- switch(substr(x, 5, 6), "Q1" = "01S1", "Q2" = "04S1", "Q3" = "07S1", "Q4" = "10S1")
      x <- paste0(year, mons)
    } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      mons <- switch(substr(x, 5, 6), "S1" = "01S1", "S2" = "07S1")
      x <- paste0(year, mons)
    } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
      x <- paste0(x, "0101")
    } else {
      stop("invalid date format")
    }
  } else if (cycle == "M")  {
    if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
      x <- substr(x, 1, 6)
    } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      mon <- switch(substr(x, 6, 6), "1" = "01", "2" = "04", "3" = "07", "4" = "10")
      x <- paste0(year, mon)
    } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      mon <- switch(substr(x, 6, 6), "1" = "01", "2" = "07")
      x <- paste0(year, mon)
    } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
      x <- paste0(x, "01")
    } else {
      stop("invalid date format")
    }
  } else if (cycle == "Q") {
    if (grepl("^[0-9]{4}[01][0-9]", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      mon <- as.numeric(substr(x, 5, 6))
      if (mon >= 1 & mon < 4) {
        quarter <- "Q1"
      } else if (mon >= 4 & mon < 7) {
        quarter <- "Q2"
      } else if (mon >= 7 & mon < 10) {
        quarter <- "Q3"
      } else if (mon >= 10 & mon <= 12) {
        quarter <- "Q4"
      } else {
        stop("invalid date format")
      }
      x <- paste0(year, quarter)
    } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
    } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      quarter <- switch(substr(x, 5, 6), "S1" = "Q1", "S2" = "Q3")
      x <- paste0(year, quarter)
    } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
      x <- paste0(substr(x, 1, 4), "Q1")
    } else {
      stop("invalid date format")
    }
  } else if (cycle == "S")  {
    if (grepl("^[0-9]{4}[01][0-9]", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      half <- if (as.numeric(substr(x, 5, 6)) < 7) "S1" else "S2"
      x <- paste0(year, half)
    } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
      year <- substr(x, 1, 4)
      half <- switch(substr(x, 5, 6), "Q1" = "S1", "Q2" = "S1", "Q3" = "S2", "Q4" = "S2")
      x <- paste0(year, half)
    } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
    } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
      x <- paste0(x, "S1")
    } else {
      stop("invalid date format")
    }
  } else if (cycle == "A") {
    if (grepl("^[0-9]{4}", x, perl = TRUE)) {
      x <- substr(x, 1, 4)
    } else {
      stop("invalid date format")
    }
  } else {
    stop("invalid cycle format")
  }
  return(x)
}


# print -------------------------------------------------------------------

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

orderStatSearchColumns <- function(df) {
  .statSearchColumns <- c(
    "stat_code" , "stat_name", 
    "item_code1", "item_name1",
    "item_code2", "item_name2", 
    "item_code3", "item_name3",
    "item_code4", "item_name4",
    "time", "data_value", "unit_name"
  )
  check <- all(colnames(df) %in% .statSearchColumns)
  if (check)
    df <- df[, .statSearchColumns]
  return(df)
}
