
# glabal variables --------------------------------------------------------

utils::globalVariables(c("calendar"))

# print -------------------------------------------------------------------

#' drawLine Function
#'
#' @description draw a line
#' @param width width
#' @param mark mark
#' @keywords internal 
#' 
drawLine <- function(width, mark = "=") {
  if (missing(width)) 
    width <- options()$width
  line <- paste0(
    paste0(
      rep(mark, times = min(width, options()$width)
    ), collapse = ""), "\n")
  return(line)
}

#' showStatTableList Function
#'
#' @description Show neat return object of statTableList function
#' @param api_key Open API authentication key issued by the Bank of Korea
#' @param format File format of the result value - xml, json
#' @param lang Language of result value - kr (Korean), en (English)
#' @keywords internal 
#' 
showStatTableList <- function(api_key, format = c("xml", "json"), lang = c("kr", "en")) {
  df <- statTableList(api_key = api_key, format = format[[1L]], lang = lang[[1L]])
  nc_srch <- max(nchar(df$srch_yn))
  nc_code <- max(nchar(df$stat_code))
  nc_name <- max(nchar(df$stat_name))
  iter <- nc_srch + nchar(" | ") + nc_code + nchar(" | ") + nc_name
  # line <- drawLine(min(iter, options()$width))
  line <- drawLine()
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

#' showStatItemList Function
#'
#' @description Show neat return object of statItemList function
#' @param api_key Open API authentication key issued by the Bank of Korea
#' @param format File format of the result value - xml, json
#' @param lang Language of result value - kr (Korean), en (English)
#' @param stat_code Statistical table code
#' @keywords internal 
#' 
showStatItemList <- function(api_key, format = c("xml", "json"), lang = c("kr", "en"), stat_code) {
  df <- statItemList(api_key = api_key, format = format[[1L]], 
                     lang = lang[[1L]], stat_code = stat_code)
  # nc_cycle <- max(nchar(df$cycle))
  nc_code <- max(nchar(df$item_code))
  nc_name <- max(nchar(df$item_name))
  iter <- nc_cycle + nchar(" | ") + nc_code + nchar(" | ") + nc_name
  # line <- drawLine(min(iter, options()$width))
  line <- drawLine()
  result <- paste0(
    paste0(
      # str_pad(df$cycle, width = nc_cycle, pad = " ", side = "right"),
      # " | ", 
      str_pad(df$item_code, width = nc_code, pad = " ", side = "right"),
      " | ", 
      df$item_name
    ), collapse = "\n")
  cat(line)
  cat(result, "\n")
  cat(line)
}

#' orderStatSearchColumns Function
#'
#' @description check and sort column order 
#' @param x return object of function statSearch
#' @keywords internal 
#' 
orderStatSearchColumns <- function(x) {
  .statSearchColumns <- c(
    "stat_code" , "stat_name", 
    "item_code1", "item_name1",
    "item_code2", "item_name2", 
    "item_code3", "item_name3",
    "item_code4", "item_name4",
    "time", "data_value", "unit_name"
  )
  check <- all(colnames(x) %in% .statSearchColumns)
  if (check)
    x <- x[, .statSearchColumns]
  return(x)
}

# calendar ----------------------------------------------------------------

#' setCalendar Function
#'
#' @description Set calendar data frame to covert date format according to a cycle argument
#' @param start Start date
#' @param end End date
#' @keywords internal 
#' 
setCalendar <- function(start = "1900-01-01", end = "2099-12-31") {
  d <- format(as.Date(as.numeric(as.Date(start)):as.numeric(as.Date(end)), 
                      origin = "1970-01-01"), "%Y%m%d")
  m <- substr(d, 1, 6)
  a <- substr(d, 1, 4)
  
  sm <- paste0(m, ifelse(substr(d, 7, 8) < 16, "S1", "S2")) # semi-monthly
  
  mon <- as.numeric(substr(d, 5, 6))
  quarter <- ifelse(mon < 4, "Q1", ifelse(mon < 7, "Q2", ifelse(mon < 10, "Q3", ifelse(mon <= 12, "Q4", ""))))
  q <- paste0(a, quarter) # quarterly
  
  half <- ifelse(mon < 7, "S1", ifelse(mon <= 12, "S2", ""))
  s <- paste0(a, half) # semi-annually
  
  data.frame(D = d, SM = sm, M = m, Q = q, S = s, A = a)
}
  
#' getCalendarTime Function
#'
#' @description convert 
#' @param x Date (format: 2015, 2015S1, 2015Q1, 201501, 201501S1, 20150101, etc.)
#' @param cycle Cycle (Annual: A, Semi-Annual: S, Quarterly: Q, Monthly: M, Semi-Monthly: SM, Daily: D)
#' @keywords internal 
#' 
getCalendarTime <- function(x, cycle) {
  if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
    z <- calendar[calendar$D == x,][[cycle]]
  } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
    z <- calendar[calendar$SM == x,][[cycle]]
  } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
    z <- calendar[calendar$M == x,][[cycle]]
  } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
    z <- calendar[calendar$Q == x,][[cycle]]
  } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
    z <- calendar[calendar$S == x,][[cycle]]
  } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
    z <- calendar[calendar$A == x,][[cycle]]
  } else {
    stop("invalid date format")
  }
  return(unique(z))
}

# cycle time format -------------------------------------------------------

# cycleTimeFormat <- function(x, cycle) {
#   if (cycle == "D") {
#     if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
#     } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
#       yearmon <- substr(x, 1, 6)
#       day <- switch(substr(x, 7, 8), "S1" = "01", "S2" = "16")
#       x <- paste0(yearmon, day)
#     } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
#       x <- paste0(x, "01")
#     } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       monday <- switch(substr(x, 5, 6), "Q1" = "0101", "Q2" = "0401", "Q3" = "0701", "Q4" = "1001")
#       x <- paste0(year, monday)
#     } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       monday <- switch(substr(x, 5, 6), "S1" = "0101", "S3" = "0701")
#       x <- paste0(year, monday)
#     } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
#       x <- paste0(substr(x, 1, 4), "0101")
#     } else {
#       stop("invalid date format")
#     }
#   } else if (cycle == "SM") {
#     if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
#       yearmon <- substr(x, 1, 6)
#       half <- if (as.numeric(substr(x, 7, 8)) < 16) "S1" else "S2"
#       x <- paste0(yearmon, half)
#     } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
#     } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
#       x <- substr(x, "S1")
#     } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       mons <- switch(substr(x, 5, 6), "Q1" = "01S1", "Q2" = "04S1", "Q3" = "07S1", "Q4" = "10S1")
#       x <- paste0(year, mons)
#     } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       mons <- switch(substr(x, 5, 6), "S1" = "01S1", "S2" = "07S1")
#       x <- paste0(year, mons)
#     } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
#       x <- paste0(x, "0101")
#     } else {
#       stop("invalid date format")
#     }
#   } else if (cycle == "M")  {
#     if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
#       x <- substr(x, 1, 6)
#     } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
#       x <- substr(x, 1, 6)
#     } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
#     } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       mon <- switch(substr(x, 6, 6), "1" = "01", "2" = "04", "3" = "07", "4" = "10")
#       x <- paste0(year, mon)
#     } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       mon <- switch(substr(x, 6, 6), "1" = "01", "2" = "07")
#       x <- paste0(year, mon)
#     } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
#       x <- paste0(x, "01")
#     } else {
#       stop("invalid date format")
#     }
#   } else if (cycle == "Q") {
#     if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       mon <- as.numeric(substr(x, 5, 6))
#       if (mon >= 1 & mon < 4) {
#         quarter <- "Q1"
#       } else if (mon >= 4 & mon < 7) {
#         quarter <- "Q2"
#       } else if (mon >= 7 & mon < 10) {
#         quarter <- "Q3"
#       } else if (mon >= 10 & mon <= 12) {
#         quarter <- "Q4"
#       } else {
#         stop("invalid date format")
#       }
#       x <- paste0(year, quarter)
#     } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       mon <- as.numeric(substr(x, 5, 6))
#       if (mon >= 1 & mon < 4) {
#         quarter <- "Q1"
#       } else if (mon >= 4 & mon < 7) {
#         quarter <- "Q2"
#       } else if (mon >= 7 & mon < 10) {
#         quarter <- "Q3"
#       } else if (mon >= 10 & mon <= 12) {
#         quarter <- "Q4"
#       } else {
#         stop("invalid date format")
#       }
#       x <- paste0(year, quarter)
#     } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       mon <- as.numeric(substr(x, 5, 6))
#       if (mon >= 1 & mon < 4) {
#         quarter <- "Q1"
#       } else if (mon >= 4 & mon < 7) {
#         quarter <- "Q2"
#       } else if (mon >= 7 & mon < 10) {
#         quarter <- "Q3"
#       } else if (mon >= 10 & mon <= 12) {
#         quarter <- "Q4"
#       } else {
#         stop("invalid date format")
#       }
#       x <- paste0(year, quarter)
#     } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
#     } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       quarter <- switch(substr(x, 5, 6), "S1" = "Q1", "S2" = "Q3")
#       x <- paste0(year, quarter)
#     } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
#       x <- paste0(substr(x, 1, 4), "Q1")
#     } else {
#       stop("invalid date format")
#     }
#   } else if (cycle == "S")  {
#     if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       half <- if (as.numeric(substr(x, 5, 6)) < 7) "S1" else "S2"
#       x <- paste0(year, half)
#     } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       half <- if (as.numeric(substr(x, 5, 6)) < 7) "S1" else "S2"
#       x <- paste0(year, half)
#     } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       half <- if (as.numeric(substr(x, 5, 6)) < 7) "S1" else "S2"
#       x <- paste0(year, half)
#     } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       half <- switch(substr(x, 5, 6), "Q1" = "S1", "Q2" = "S1", "Q3" = "S2", "Q4" = "S2")
#       x <- paste0(year, half)
#     } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
#     } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
#       x <- paste0(x, "S1")
#     } else {
#       stop("invalid date format")
#     }
#   } else if (cycle == "A") {
#     if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
#       x <- substr(x, 1, 4)
#     } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
#       x <- substr(x, 1, 4)
#     } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
#       x <- substr(x, 1, 4)
#     } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
#       x <- substr(x, 1, 4)
#     } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
#       x <- substr(x, 1, 4)
#     } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
#     } else {
#       stop("invalid date format")
#     }
#   } else {
#     stop("invalid cycle format")
#   }
#   return(x)
# }

# cycleTimeFormat <- function(x, cycle) {
#   if (cycle == "D") {
#     if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
#     } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
#       yearmon <- substr(x, 1, 6)
#       day <- switch(substr(x, 7, 8), "S1" = "01", "S2" = "16")
#       x <- paste0(yearmon, day)
#     } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
#       x <- paste0(x, "01")
#     } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       monday <- switch(substr(x, 5, 6), "Q1" = "0101", "Q2" = "0401", "Q3" = "0701", "Q4" = "1001")
#       x <- paste0(year, monday)
#     } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       monday <- switch(substr(x, 5, 6), "S1" = "0101", "S3" = "0701")
#       x <- paste0(year, monday)
#     } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
#       x <- paste0(substr(x, 1, 4), "0101")
#     } else {
#       stop("invalid date format")
#     }
#   } else if (cycle == "SM") {
#     if (grepl("^[0-9]{4}[01][0-9][0-3][0-9]$", x, perl = TRUE)) {
#       yearmon <- substr(x, 1, 6)
#       half <- if (as.numeric(substr(x, 7, 8)) < 16) "S1" else "S2"
#       x <- paste0(yearmon, half)
#     } else if (grepl("^[0-9]{4}[01][0-9]S[12]$", x, perl = TRUE)) {
#     } else if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
#       x <- substr(x, "S1")
#     } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       mons <- switch(substr(x, 5, 6), "Q1" = "01S1", "Q2" = "04S1", "Q3" = "07S1", "Q4" = "10S1")
#       x <- paste0(year, mons)
#     } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       mons <- switch(substr(x, 5, 6), "S1" = "01S1", "S2" = "07S1")
#       x <- paste0(year, mons)
#     } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
#       x <- paste0(x, "0101")
#     } else {
#       stop("invalid date format")
#     }
#   } else if (cycle == "M")  {
#     if (grepl("^[0-9]{4}[01][0-9]$", x, perl = TRUE)) {
#       x <- substr(x, 1, 6)
#     } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       mon <- switch(substr(x, 6, 6), "1" = "01", "2" = "04", "3" = "07", "4" = "10")
#       x <- paste0(year, mon)
#     } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       mon <- switch(substr(x, 6, 6), "1" = "01", "2" = "07")
#       x <- paste0(year, mon)
#     } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
#       x <- paste0(x, "01")
#     } else {
#       stop("invalid date format")
#     }
#   } else if (cycle == "Q") {
#     if (grepl("^[0-9]{4}[01][0-9]", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       mon <- as.numeric(substr(x, 5, 6))
#       if (mon >= 1 & mon < 4) {
#         quarter <- "Q1"
#       } else if (mon >= 4 & mon < 7) {
#         quarter <- "Q2"
#       } else if (mon >= 7 & mon < 10) {
#         quarter <- "Q3"
#       } else if (mon >= 10 & mon <= 12) {
#         quarter <- "Q4"
#       } else {
#         stop("invalid date format")
#       }
#       x <- paste0(year, quarter)
#     } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
#     } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       quarter <- switch(substr(x, 5, 6), "S1" = "Q1", "S2" = "Q3")
#       x <- paste0(year, quarter)
#     } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
#       x <- paste0(substr(x, 1, 4), "Q1")
#     } else {
#       stop("invalid date format")
#     }
#   } else if (cycle == "S")  {
#     if (grepl("^[0-9]{4}[01][0-9]", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       half <- if (as.numeric(substr(x, 5, 6)) < 7) "S1" else "S2"
#       x <- paste0(year, half)
#     } else if (grepl("^[0-9]{4}Q[1-4]$", x, perl = TRUE)) {
#       year <- substr(x, 1, 4)
#       half <- switch(substr(x, 5, 6), "Q1" = "S1", "Q2" = "S1", "Q3" = "S2", "Q4" = "S2")
#       x <- paste0(year, half)
#     } else if (grepl("^[0-9]{4}S[12]$", x, perl = TRUE)) {
#     } else if (grepl("^[0-9]{4}$", x, perl = TRUE)) {
#       x <- paste0(x, "S1")
#     } else {
#       stop("invalid date format")
#     }
#   } else if (cycle == "A") {
#     if (grepl("^[0-9]{4}", x, perl = TRUE)) {
#       x <- substr(x, 1, 4)
#     } else {
#       stop("invalid date format")
#     }
#   } else {
#     stop("invalid cycle format")
#   }
#   return(x)
# }

