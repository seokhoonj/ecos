#' statItemList Function
#'
#' @description Item List of Statistics
#' @param api_key Open API authentication key issued by the Bank of Korea
#' @param format File format of the result value - xml, json
#' @param lang Language of result value - kr (Korean), en (English)
#' @param count Number of requests
#' @param stat_code Statistical table code
#' @keywords ecos, statItemList 
#' @export
#' @examples
#' # do not test -- needs an API key
#' # statItemList(api_key = your_api_key, lang = "en", count = 100, stat_code = "902Y001")
#' # statItemList(api_key = your_api_key, lang = "kr", count = 100, stat_code = "902Y001")
#' 
statItemList <- function(api_key, format = c("xml", "json"), lang = c("kr", "en"), count, stat_code) {
	if (missing(api_key))
	  stop("Please create your api key from website 'https://ecos.bok.or.kr/api/#/AuthKeyApply'")
	if (missing(count))
		count <- 100
	if (missing(stat_code)) {
	  options("max.print" = .Machine$integer.max)
	  showStatTableList(api_key, format = format[[1L]], lang = lang[[1L]])
	  options("max.print" = 1e3)
	  stat_code <- readline("Please insert stat_code: ")
	}
	if (format[[1L]] == "xml") {
		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticItemList/%s/%s/%s/1/%s/%s/", 
		                         api_key, format[[1L]], lang[[1L]], count, stat_code))
		html <- GET(url)
		content <- rawToChar(html$content)
		xml_all <- xmlParse(content)
		if (is.null(unlist(xpathApply(xml_all, "//RESULT")))) {
			xml_cnt <- xpathApply(xml_all, "//list_total_count")[[1L]]
			cnt <- as.integer(xmlToList(xml_cnt)[[1L]])
			xml_row <- xpathApply(xml_all, "//row") 
			df <- xmlToDataFrame(xml_row, stringsAsFactors = FALSE)
			names(df) <- tolower(names(df))
			df$data_cnt <- as.numeric(df$data_cnt)
			attr(df, "list_total_count") <- cnt 
		} else {
			code <- xmlToList(xpathApply(xml_all, "//CODE")[[1L]])
			msg  <- xmlToList(xpathApply(xml_all, "//MESSAGE")[[1L]])
			stop(paste0(code, "\n ", msg))
		}
	} else if (format[[1L]] == "json") {
		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticItemList/%s/%s/%s/1/%s/%s/", 
		                         api_key, format[[1L]], lang[[1L]], count, stat_code))
		html <- GET(url)
		content <- rawToChar(html$content)
		json_all <- fromJSON(content)
		if (is.null(json_all$RESULT)) {
			cnt <- json_all$StatisticItemList$list_total_count
			df  <- json_all$StatisticItemList$row
			names(df) <- tolower(names(df))
			df$data_cnt <- as.numeric(df$data_cnt)
			attr(df, "list_total_count") <- cnt 
		} else {
			code <- json_all$RESULT$CODE	
			msg  <- json_all$RESULT$MESSAGE	
			stop(paste0(code, "\n ", msg))
		}
	} else {
		stop("This file format is not supported.")
	}
	return(df)
}

