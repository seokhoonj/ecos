#' statWord Function
#'
#' @title Glossary of Statistical Terms
#' @param api_key,format,lang,count,word input parameters
#' @keywords ecos, statMeta 
#' @export
#' @examples
#' statWord(api_key = your_api_key, lang = "kr", count = 10, word = "CPIS")
#' statWord(api_key = your_api_key, lang = "kr", count = 10, word = "소비자동향지수")
#' 
statWord <- function(api_key, format = c("xml", "json"), lang = c("kr", "en"), count, word) {
	if (missing(api_key))
	  stop("Please create your api key from website 'https://ecos.bok.or.kr/api/#/AuthKeyApply'")
	if (missing(count))
		count <- 10 
	if (missing(word))
		word <- "CPIS" 
	if (format[[1]] == "xml") {
		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticWord/%s/%s/%s/1/%s/%s/", 
		                         api_key, format[[1]], lang[[1]], count, word))
		html <- GET(url)
		content <- rawToChar(html$content)
		xml_all <- xmlParse(content)
		if (is.null(unlist(xpathApply(xml_all, "//RESULT")))) {
			xml_cnt <- xpathApply(xml_all, "//list_total_count")[[1]]
			cnt <- as.integer(xmlToList(xml_cnt))
			xml_row <- xpathApply(xml_all, "//row") 
			df <- xmlToDataFrame(xml_row, stringsAsFactors = FALSE)
			names(df) <- tolower(names(df))
			attr(df, "list_total_count") <- cnt 
		} else {
			code <- xmlToList(xpathApply(xml_all, "//CODE")[[1]])
			msg  <- xmlToList(xpathApply(xml_all, "//MESSAGE")[[1]])
			stop(paste0(code, "\n ", msg))
		}
	} else if (format[[1]] == "json") {
		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticWord/%s/%s/%s/1/%s/%s/", 
		                         api_key, format[[1]], lang[[1]], count, word))
		html <- GET(url)
		content <- rawToChar(html$content)
		json_all <- fromJSON(content)
		if (is.null(json_all$RESULT)) {
			cnt <- json_all$StatisticWord$list_total_count
			df  <- json_all$StatisticWord$row
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
	return(df); gc()
}
