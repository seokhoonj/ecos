#' keyStatList Function
#'
#' @title Top 100 Statistical Indicators
#' @param api_key,format,lang,count input parameters
#' @keywords ecos, keyStatList 
#' @export
#' @examples 
#' # do not test -- needs an API key
#' # keyStatList(api_key = your_api_key, lang = "en", count = 100)
#' # keyStatList(api_key = your_api_key, lang = "kr", count = 100)
#' 
keyStatList <- function(api_key, format = c("xml", "json"), lang = c("kr", "en"), count) {
	if (missing(api_key))
	  stop("Please create your api key from website 'https://ecos.bok.or.kr/api/#/AuthKeyApply'")
	if (missing(count))
	  count <- 100
	if (format[[1L]] == "xml") {
		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/KeyStatisticList/%s/%s/%s/1/%s/", 
		                         api_key, format[[1L]], lang[[1L]], count))
		html <- GET(url)
		content <- rawToChar(html$content)
		xml_all <- xmlParse(content)
		if (is.null(unlist(xpathApply(xml_all, "//RESULT")))) {
			xml_cnt <- xpathApply(xml_all, "//list_total_count")[[1L]]
			cnt <- as.integer(xmlToList(xml_cnt)[[1L]])
			xml_row <- xpathApply(xml_all, "//row") 
			df <- xmlToDataFrame(xml_row, stringsAsFactors = FALSE)
			names(df) <- tolower(names(df))
			attr(df, "list_total_count") <- cnt 
		} else {
			code <- xmlToList(xpathApply(xml_all, "//CODE")[[1L]])
			msg  <- xmlToList(xpathApply(xml_all, "//MESSAGE")[[1L]])
			stop(paste0(code, "\n ", msg))
		}
	} else if (format[[1L]] == "json") {
		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/KeyStatisticList/%s/%s/%s/1/%s/", 
		                         api_key, format[[1L]], lang[[1L]], count))
		html <- GET(url)
		content <- rawToChar(html$content)
		json_all <- fromJSON(content)
		if (is.null(json_all$RESULT)) {
			cnt <- json_all$KeyStatisticList$list_total_count
			df  <- json_all$KeyStatisticList$row
			names(df) <- tolower(names(df))
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
