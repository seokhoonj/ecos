#################################################################################
## glabal variables
#################################################################################

utils::globalVariables(c("calendar"))

#################################################################################
## parse xml
#################################################################################
.parse_xml <- function(content, type = c("table", "item", "search", "meta",
                                         "word", "keystat")) {
  type <- match.arg(type)
  xml_all <- xmlParse(content)
  if (!is.null(unlist(xpathApply(xml_all, "//RESULT")))) {
    code <- xmlToList(xpathApply(xml_all, "//CODE")[[1L]])
    msg  <- xmlToList(xpathApply(xml_all, "//MESSAGE")[[1L]])
    stop(paste0(code, "\n ", msg), call. = FALSE)
  }
  xml_cnt <- xpathApply(xml_all, "//list_total_count")[[1L]]
  cnt <- as.integer(xmlToList(xml_cnt)[[1L]])
  xml_row <- xpathApply(xml_all, "//row")
  df <- xmlToDataFrame(xml_row, stringsAsFactors = FALSE)
  if (type == "item") {
    df$DATA_CNT <- as.numeric(df$DATA_CNT)
  }
  if (type == "search") {
    df[] <- lapply(df, trimws)
    df <- df[order(df$TIME),]
    df$DATA_VALUE <- as.numeric(df$DATA_VALUE)
  }
  names(df) <- tolower(names(df))
  attr(df, "list_total_count") <- cnt
  df
}

#################################################################################
## parse JSON
#################################################################################
.parse_json <- function(content, type = c("table", "item", "search", "meta",
                                          "word", "keystat")) {
  type <- match.arg(type)
  json_all <- fromJSON(content)
  if (!is.null(json_all$RESULT)) {
    code <- json_all$RESULT$CODE
    msg  <- json_all$RESULT$MESSAGE
    stop(paste0(code, "\n ", msg), call. = FALSE)
  }
  if (type == "table") {
    cnt <- json_all$StatisticTableList$list_total_count
    df  <- json_all$StatisticTableList$row
  } else if (type == "item") {
    cnt <- json_all$StatisticItemList$list_total_count
    df  <- json_all$StatisticItemList$row
    df$DATA_CNT <- as.numeric(df$DATA_CNT)
  } else if (type == "search") {
    cnt  <- json_all$StatisticSearch$list_total_count
    df   <- json_all$StatisticSearch$row
    df[] <- lapply(df, trimws)
    df$DATA_VALUE <- as.numeric(df$DATA_VALUE)
    df <- df[order(df$TIME),]
  } else if (type == "meta") {
    cnt <- json_all$StatisticMeta$list_total_count
    df  <- json_all$StatisticMeta$row
  } else if (type == "word"){
    cnt <- json_all$StatisticWord$list_total_count
    df  <- json_all$StatisticWord$row
  } else {
    cnt <- json_all$KeyStatisticList$list_total_count
    df  <- json_all$KeyStatisticList$row
  }
  names(df) <- tolower(names(df))
  attr(df, "list_total_count") <- cnt
  df
}

#################################################################################
## print
#################################################################################
##' drawLine Function
##'
##' @description draw a line
##' @param width width
##' @param mark mark
##' @keywords internal
##'
drawLine <- function(width, mark = "=") {
  if (missing(width)) {
    width <- getOption("width")
    ## width <- options()$width
  }
  paste0(paste0(rep(mark, times = min(width, getOption("width"))),
                collapse = ""), "\n")
}

reduceRows <- function(x, n = 30L) {
  tn <- nrow(x)
  if (tn > 1e3)
    return(rbind(head(x, n/2), tail(x, n/2)))
  return(x)
}

na2str <- function(x)
  if (is.character(x)) ifelse(is.na(x), "", x) else x

repaste <- function(x) {
  n <- length(x)
  if (n == 1L) {
    return(x[[1L]])
  } else {
    x[[n-1]] <- paste(x[[n-1]], "|", x[[n]])
    x[[n]] <- NULL
    repaste(x)
  }
}

adjustColumnWidth <- function(x, hchar, fullcols = TRUE) {
  df <- reduceRows(as.data.frame(x))
  cols <- names(df)
  nchar_cols <- nchar(cols)
  notc_cols_no <- which(sapply(df, class) != "character")
  if (length(notc_cols_no) > 0)
    df[, notc_cols_no] <- lapply(df[, notc_cols_no, drop = FALSE], as.character)
  width <- sapply(df, function(x) if (all(is.na(x))) 2L else max(nchar(x), na.rm = T))
  if (fullcols)
    width <- pmax(width, nchar_cols)
  if (!missing(hchar))
    width <- pmax(width, min(hchar, max(nchar_cols)))
  df[] <- lapply(df, na2str)
  side <- sapply(df, function(x) if (is.character(x)) "right" else "left")
  df[] <- lapply(seq_along(df), function(x)
    str_pad(df[[x]], width = width[x], side = side[x]))
  abb_cols <- substr(names(width), 1L, width)
  new_cols <- str_pad(abb_cols, width = width, pad = " ", side = "both")
  names(df) <- new_cols
  attr(df, "columns") <- cols
  attr(df, "width") <- width
  attr(df, "side") <- side
  return(df)
}

hprint <- function(x, hchar, fullcols = TRUE) {
  df <- adjustColumnWidth(x, fullcols)
  txt <- repaste(df)
  cols <- colnames(df)
  cat(drawLine())
  cat(paste0("| ", paste0(cols, collapse = " | "), "\n"))
  cat(drawLine())
  cat(paste0(paste0("| ", txt), collapse = "\n"), "\n")
  cat(drawLine())
}

##' showStatTableList Function
##'
##' @description Show neat return object of statTableList function
##' @param api_key Open API authentication key issued by the Bank of Korea
##' @param format File format of the result value - xml, json
##' @param lang Language of result value - kr (Korean), en (English)
##' @keywords internal
##'
showStatTableList <- function(api_key, format = c("xml", "json"), lang = c("kr", "en")) {
  format <- match.arg(format)
  lang <- match.arg(lang)
  df <- statTableList(api_key = api_key, format = format, lang = lang)
  hprint(df[, c("srch_yn", "stat_code", "stat_name")])
}

##' showStatItemList Function
##'
##' @description Show neat return object of statItemList function
##' @param api_key Open API authentication key issued by the Bank of Korea
##' @param format File format of the result value - xml, json
##' @param lang Language of result value - kr (Korean), en (English)
##' @param stat_code Statistical table code
##' @keywords internal
##'
showStatItemList <- function(api_key, format = c("xml", "json"),
                             lang = c("kr", "en"), stat_code) {
  format <- match.arg(format)
  lang <- match.arg(lang)
  df <- statItemList(api_key = api_key, format = format,
                     lang = lang, stat_code = stat_code)
  hprint(df[, c("cycle", "item_code", "item_name")])
}

##' orderStatSearchColumns Function
##'
##' @description check and sort column order
##' @param x return object of function statSearch
##' @keywords internal
##'
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

#################################################################################
## calendar
#################################################################################

##' setCalendar Function
##'
##' @description Set calendar data frame to covert date format according to a
##'   cycle argument
##' @param start Start date
##' @param end End date
##' @keywords internal
##'
setCalendar <- function(start = "1900-01-01", end = "2099-12-31") {
  d <- format(as.Date(as.numeric(as.Date(start)):as.numeric(as.Date(end)),
                      origin = "1970-01-01"), "%Y%m%d")
  m <- substr(d, 1, 6)
  a <- substr(d, 1, 4)
  sm <- paste0(m, ifelse(substr(d, 7, 8) < 16, "S1", "S2")) # semi-monthly
  mon <- as.numeric(substr(d, 5, 6))
  quarter <- ifelse(mon < 4, "Q1", ifelse(mon < 7, "Q2",
                                   ifelse(mon < 10, "Q3",
                                   ifelse(mon <= 12, "Q4", ""))))
  q <- paste0(a, quarter) # quarterly
  half <- ifelse(mon < 7, "S1", ifelse(mon <= 12, "S2", ""))
  s <- paste0(a, half) # semi-annually
  data.frame(D = d, SM = sm, M = m, Q = q, S = s, A = a)
}

##' getCalendarTime Function
##'
##' @description convert
##' @param x Date (format: 2015, 2015S1, 2015Q1, 201501, 201501S1, 20150101,
##'   etc.)
##' @param cycle Cycle (Annual: A, Semi-Annual: S, Quarterly: Q, Monthly: M,
##'   Semi-Monthly: SM, Daily: D)
##' @keywords internal
##'
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
