##' Search conditional statistics
##'
##' @details
##' \preformatted{
##' ## Example
##' statSearch(lang = "en", stat_code = "102Y004", item_code1 = "ABA1", cycle =
##' "M", start_time = "196001", end_time = "201812")
##' }
##'
## @param api_key A string specifying ECOS API key. Need not be specified if
##   the key was stored as an environment variable via \code{\link{setKey}} or
##   .Renviron.
##' @param stat_code A string specifying the statistical table code
##' @param item_code1 A string specifying the statistical item 1 code
##' @param item_code2 A string specifying the statistical item 2 code
##' @param item_code3 A string specifying the statistical item 3 code
##' @param item_code4 A string specifying the statistical item 4 code
##' @param cycle A string specifying the cycle (Annual: A, Semi-Annual: S,
##'   Quarterly: Q, Monthly: M, Semi-Monthly: SM, Daily: D)
##' @param start_time A string specifying the start date (according to cycle
##'   format: 2015, 2015S1, 2015Q1, 201501, 201501S1, 20150101, etc.)
##' @param end_time A string specifying the end date (according to cycle format:
##'   2015, 2015S1, 2015Q1, 201501, 201501S1, 20150101, etc.)
##' @param format A string specifying the file format to process - xml, json
##' @param lang A string specifying the language of result value - kr (Korean),
##'   en (English)
##' @param count An integer specifying the number of requests
##' @return A data.frame object containing queried information
##' @export
statSearch <- function(stat_code, item_code1, item_code2 = "?",
                       item_code3 = "?", item_code4 = "?",
                       cycle, start_time, end_time, format = c("xml", "json"),
                       lang = c("kr", "en"), count) {
  api_key <- ecos.getKey()
  format <- match.arg(format)
  lang <- match.arg(lang)
	if (missing(stat_code)) {
	  # op <- options("max.print" = .Machine$integer.max)
	  showStatTableList(format = format, lang = lang)
	  # on.exit(op)
	  stat_code <- readline("Please insert stat_code: ")
	}
  item_list <- statItemList(stat_code = stat_code, format = format, lang = lang)
	if (missing(item_code1)) {
	  showStatItemList(stat_code = stat_code, format = format, lang = lang)
	  item_code1 <- readline("Please insert item_code1: ")
	  item_args <- item_list[item_list$item_code == item_code1, ]
	} else {
	  item_args <- item_list[item_list$item_code == item_code1, ]
	}
	if (missing(cycle)) {
	  cycle <- readline(
      sprintf("Please select cycle code (%s): ",
              paste0(item_args$cycle, collapse = ", "))
    )
	}
	pos <- which(item_args$cycle == cycle)
	if (missing(start_time)) {
	  start_time <- item_args$start_time[pos]
  }
	if (missing(end_time)) {
	  end_time <- item_args$end_time[pos]
  }
	if (missing(count)) {
	  count <- item_args$data_cnt[pos]
  }
	start_time <- min(getCalendarTime(start_time, cycle))
	end_time <- max(getCalendarTime(end_time, cycle))
  url <- URLencode(
    sprintf(
      "http://ecos.bok.or.kr/api/StatisticSearch/%s/%s/%s/1/%s/%s/%s/%s/%s/%s/%s/%s/%s",
      api_key, format, lang, count, stat_code, cycle,
      start_time, end_time, item_code1,
      item_code2, item_code3, item_code4
    )
  )
  html <- GET(url)
  content <- rawToChar(html$content)
	if (format == "xml") {
    df <- parseXML(content, type = "search")
	} else {
    df <- parseJSON(content, type = "search")
	}
	orderStatSearchColumns(df)
}
