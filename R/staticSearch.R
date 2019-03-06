#' A staticSearch Function
#'
#' ECOS is the economy statistics (from Bank of Korea, http://ecos.bok.or.kr)
#' @param you can get korea economy data through the api
#' @keywords ecos, staticSearch
#' @examples
#' df <- staticSearch(key = "sample", stat_code = "010Y002", cycle = "MM", start = "196001", end = "201812", item_code = "AAAA11")
#' head(df)

staticSearch <- function(key, stat_code, count, cycle, start, end, item_code) {
	url <- URLencode(paste0("http://ecos.bok.or.kr/api/StatisticSearch/", key, 
							"/xml/kr/1/1000/", stat_code, "/", cycle, "/", start, "/", end, "/", item_code, "/?/?/"))
	html <- getURLContent(url)
	xml_all <- xmlParse(html)
	xml_cnt <- xpathApply(xml_all, "//list_total_count")[[1]]
	list_total_count <- as.integer(xmlToList(xml_cnt))
	xml_row <- xpathApply(xml_all, "//row") 
	df <- xmlToDataFrame(xml_row, stringsAsFactors = FALSE)
	names(df) <- tolower(names(df))
	df$time <- as.Date(paste0(as.character(df$time), "01"), format = "%Y%m%d")
	df$data_value <- as.numeric(df$data_value)
	attr(df, "list_total_count") <- list_total_count
	return(df); gc()
}
