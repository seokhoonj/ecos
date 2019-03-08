#' staticSearch Function
#'
#' You can access economic statistics from Bank of Korea through the open API (https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp)
#' @param api_key,format,lang,count,stat_code,cycle,start_date,end_date,item_code input parameters
#' @keywords ecos, staticSearch
#' @export
#' @examples
#' # basic
#' df <- staticSearch()
#' head(df)
#' 
#' # stock index futures 
#' df <- staticSearch(stat_code = "085Y007", item_code = "S25B")
#' head(df)
staticSearch <- function(api_key, format, lang, count, stat_code, cycle, start_date, end_date, item_code) {

	if (missing(api_key))
		api_key <- "LBVUDMTWICYRKCSJAYO6"

	if (missing(format))
		format <- "json"

	if (missing(lang))
		lang <- "kr"

	if (missing(count))
		count <- 1000

	if (missing(stat_code))
		stat_code <- "010Y002"
	
	if (missing(cycle))
		cycle <- "MM"

	if (missing(start_date))
		start_date <- "196001"

	if (missing(end_date))
		end_date <- "201812"

	if (missing(item_code))
		item_code <- "AAAA11"

	if (format == "json") {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticSearch/%s/%s/%s/1/%s/%s/%s/%s/%s/%s/?/?/", 
								 api_key, format, lang, count, stat_code, cycle, start_date, end_date, item_code))
		html <- getURLContent(url)
		json_all <- fromJSON(html)
		cnt  <- json_all$StatisticSearch[[1]]
		df   <- json_all$StatisticSearch[[2]]
		df[] <- lapply(df, trimws)
		names(df) <- tolower(names(df))
		df$time <- as.Date(paste0(df$time, "01"), format = "%Y%m%d")
		df$data_value <- as.numeric(df$data_value)
		attr(df, "list_total_count") <- cnt 

	} else {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticSearch/%s/%s/%s/1/%s/%s/%s/%s/%s/%s/?/?/", 
								 api_key, format, lang, count, stat_code, cycle, start_date, end_date, item_code))
		html <- getURLContent(url)
		xml_all <- xmlParse(html)
		xml_cnt <- xpathApply(xml_all, "//list_total_count")[[1]]
		cnt <- as.integer(xmlToList(xml_cnt))
		xml_row <- xpathApply(xml_all, "//row") 
		df <- xmlToDataFrame(xml_row, stringsAsFactors = FALSE)
		names(df) <- tolower(names(df))
		df$time <- as.Date(paste0(as.character(df$time), "01"), format = "%Y%m%d")
		df$data_value <- as.numeric(df$data_value)
		attr(df, "list_total_count") <- cnt 
	}

	return(df); gc()
}
