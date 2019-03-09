#' statItemList Function
#'
#' You can access economic statistics from Bank of Korea through the OPEN API (https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp)
#' @param api_key,format,lang,count,stat_code input parameters
#' @keywords ecos, statItemList 
#' @export
#' @examples
#' # Economic Statistic Item List from Bank of Korea through the OPEN API
#' df <- statItemList()
#' head(df)
statItemList <- function(api_key, format, lang, count, stat_code) {

	if (missing(api_key))
		api_key <- "LBVUDMTWICYRKCSJAYO6"

	if (missing(format))
		format <- "json"

	if (missing(lang))
		lang <- "kr"

	if (missing(count))
		count <- 100

	if (missing(count))
		stat_code <- "010Y002"

		api_key <- "LBVUDMTWICYRKCSJAYO6"
		format <- "json"
		lang <- "kr"
		count <- 100
		stat_code <- "010Y002"

	if (format == "json") {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticItemList/%s/%s/%s/1/%s/%s/",
								 api_key, format, lang, count, stat_code))
		html <- getURLContent(url)
		json_all <- fromJSON(html)
		cnt <- json_all$StatisticItemList$list_total_count
		df  <- json_all$StatisticItemList$row
		names(df) <- tolower(names(df))
		df$data_cnt <- as.numeric(df$data_cnt)
		attr(df, "list_total_count") <- cnt 

	} else if (format == "xml") {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticItemList/%s/%s/%s/1/%s/%s/",
								 api_key, format, lang, count, stat_code))
		html <- getURLContent(url)
		xml_all <- xmlParse(html)
		xml_cnt <- xpathApply(xml_all, "//list_total_count")[[1]]
		cnt <- as.integer(xmlToList(xml_cnt))
		xml_row <- xpathApply(xml_all, "//row") 
		df <- xmlToDataFrame(xml_row, stringsAsFactors = FALSE)
		names(df) <- tolower(names(df))
		df$data_cnt <- as.numeric(df$data_cnt)
		attr(df, "list_total_count") <- cnt 

	} else {

		stop("not supported data format.")

	}

	return(df); gc()
}
