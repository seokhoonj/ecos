#' statTableList Function
#'
#' You can access economic statistics from Bank of Korea through the open API (https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp)
#' @param api_key,format,lang,count input parameters
#' @keywords ecos, statTableList 
#' @export
#' @examples
#' # Statistics Table List 
#' df <- statTableList()
#' head(df)
statTableList <- function(api_key, format, lang, count) {
	
	if (missing(api_key))
		api_key <- "LBVUDMTWICYRKCSJAYO6"

	if (missing(format))
		format <- "json"

	if (missing(lang))
		lang <- "kr"

	if (missing(count))
		count <- 1000 
		
	if (format == "json") {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticTableList/%s/%s/%s/1/%s/?",
								 api_key, format, lang, count))
		html <- getURLContent(url)
		json_all <- fromJSON(html)
		cnt <- json_all$StatisticTableList$list_total_count
		df  <- json_all$StatisticTableList$row
		names(df) <- tolower(names(df))
		attr(df, "list_total_count") <- cnt

	} else if (format == "xml") {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticTableList/%s/%s/%s/1/%s/?",
								 api_key, format, lang, count))
		html <- getURLContent(url)
		xml_all <- xmlParse(html)
		xml_cnt <- xpathApply(xml_all, "//list_total_count")[[1]]
		cnt <- as.integer(xmlToList(xml_cnt))
		xml_row <- xpathApply(xml_all, "//row")
		df <- xmlToDataFrame(xml_row, stringsAsFactors = FALSE)
		names(df) <- tolower(names(df))
		attr(df, "list_total_count") <- cnt

	} else {

		stop("not supported data format.")

	}

	return(df); gc()
}
