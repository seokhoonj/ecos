#' statSearch Function
#'
#' @description Search conditional statistics
#' @param api_key Open API authentication key issued by the Bank of Korea
#' @param format File format of the result value - xml, json
#' @param lang Language of result value - kr (Korean), en (English)
#' @param stat_code Statistical table code
#' @param item_code1 Statistical item 1 code
#' @param item_code2 Statistical item 2 code
#' @param item_code3 Statistical item 3 code
#' @param item_code4 Statistical item 4 code
#' @param cycle Cycle (Annual: A, Semi-Annual: S, Quarterly: Q, Monthly: M, Semi-Monthly: SM, Daily: D)
#' @param start_time Start date (according to cycle format: 2015, 2015S1, 2015Q1, 201501, 201501S1, 20150101, etc.)
#' @param end_time End date (according to cycle format: 2015, 2015S1, 2015Q1, 201501, 201501S1, 20150101, etc.)
#' @param count Number of requests
#' @keywords ecos, statSearch
#' @export
#' @examples
#' # do not test -- needs an API key
#' # statSearch(api_key = your_api_key, lang = "en", stat_code = "102Y004", item_code1 = "ABA1", 
#' # cycle = "M", start_time = "196001", end_time = "201812")
#' # statSearch(api_key = your_api_key, lang = "kr", stat_code = "102Y004", item_code1 = "ABA1", 
#' # cycle = "M", start_time = "196001", end_time = "201812")
#' # statSearch(api_key = your_api_key, lang = "en", stat_code = "281Y005", item_code1 = "203020", 
#' # item_code2 = "2", item_code3 = "C260", cycle = "A")
#' 
statSearch <- function(api_key, format = c("xml", "json"), lang = c("kr", "en"), stat_code, item_code1, item_code2, item_code3, item_code4, cycle, start_time, end_time, count) {
	if (missing(api_key)) 
	  stop("Please create your api key from website 'https://ecos.bok.or.kr/api/#/AuthKeyApply'")
	if (missing(stat_code)) { 
	  options("max.print" = .Machine$integer.max)
	  showStatTableList(api_key = api_key, format = format[[1L]], lang = lang[[1L]])
	  options("max.print" = 1e3)
	  stat_code <- readline("Please insert stat_code: ")
	} 
  item_list <- statItemList(api_key = api_key, format = format[[1L]], 
                            lang = lang[[1L]], stat_code = stat_code)
	if (missing(item_code1)) {
	  showStatItemList(api_key = api_key, format = format[[1L]], 
	                   lang = lang[[1L]], stat_code = stat_code)
	  item_code1 <- readline("Please insert item_code1: ")
	  item_args <- item_list[item_list$item_code == item_code1,]
	} else {
	  item_args <- item_list[item_list$item_code == item_code1,]
	}
  if (missing(item_code2))
    item_code2 <- "?"
  if (missing(item_code3))
    item_code3 <- "?"
  if (missing(item_code4))
    item_code4 <- "?"
	if (missing(cycle)) {
	  cycle <- readline(sprintf("Please select cycle code (%s): ", 
	                            paste0(item_args$cycle, collapse = ", ")))
	}
	pos <- which(item_args$cycle == cycle)
	if (missing(start_time)) 
	  start_time <- item_args$start_time[pos]
	if (missing(end_time)) 
	  end_time <- item_args$end_time[pos]
	if (missing(count)) 
	  count <- item_args$data_cnt[pos]
	start_time <- min(getCalendarTime(start_time, cycle))
	end_time <- max(getCalendarTime(end_time, cycle))
	if (format[[1L]] == "xml") {
		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticSearch/%s/%s/%s/1/%s/%s/%s/%s/%s/%s/%s/%s/%s", 
		                         api_key, format[[1L]], lang[[1L]], count, stat_code, cycle, start_time, end_time, item_code1, item_code2, item_code3, item_code4))
		html <- GET(url)
		content <- rawToChar(html$content)
		xml_all <- xmlParse(content)
		if (is.null(unlist(xpathApply(xml_all, "//RESULT")))) {
			xml_cnt <- xpathApply(xml_all, "//list_total_count")[[1L]]
			cnt <- as.integer(xmlToList(xml_cnt)[[1L]])
			xml_row <- xpathApply(xml_all, "//row") 
			df <- xmlToDataFrame(xml_row, stringsAsFactors = FALSE)
			df[] <- lapply(df, trimws)
			names(df) <- tolower(names(df))
			df <- df[order(df$time),]
			df$data_value <- as.numeric(df$data_value)
			attr(df, "list_total_count") <- cnt 
		} else {
			code <- xmlToList(xpathApply(xml_all, "//CODE")[[1L]])
			msg  <- xmlToList(xpathApply(xml_all, "//MESSAGE")[[1L]])
			stop(paste0(code, "\n ", msg))
		}
	} else if (format[[1L]] == "json") {
		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticSearch/%s/%s/%s/1/%s/%s/%s/%s/%s/%s/%s/%s/%s", 
		                         api_key, format[[1L]], lang[[1L]], count, stat_code, cycle, start_time, end_time, item_code1, item_code2, item_code3, item_code4))
		html <- GET(url)
		content <- rawToChar(html$content)
		json_all <- fromJSON(content)
		if (is.null(json_all$RESULT)) {
			cnt  <- json_all$StatisticSearch$list_total_count
			df   <- json_all$StatisticSearch$row
			df[] <- lapply(df, trimws)
			names(df) <- tolower(names(df))
			df$data_value <- as.numeric(df$data_value)
			df <- df[order(df$time),]
			attr(df, "list_total_count") <- cnt 
		} else {
			code <- json_all$RESULT$CODE	
			msg  <- json_all$RESULT$MESSAGE	
			stop(paste0(code, "\n ", msg))
		}
	} else {
		stop("This file format is not supported.")
	}
	df <- orderStatSearchColumns(df)
	return(df)
}
