#' statSearch Function
#'
#' You can access economic statistics from Bank of Korea through the open API (https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp)
#' @param api_key,format,lang,count,stat_code,cycle,start_date,end_date,item_code input parameters
#' @keywords ecos, statSearch
#' @export
#' @examples
#' # basic
#' df <- statSearch()
#' head(df)
#' 
#' # stock index futures 
#' df <- statSearch(stat_code = "085Y007", item_code = "S25B")
#' head(df)
statSearch <- function(api_key, format, lang, count, stat_code, cycle, start_date, end_date, item_code) {

	if (missing(api_key))
		api_key <- "LBVUDMTWICYRKCSJAYO6" # instant code (to be deleted)

	if (missing(format))
		format <- "xml" # file format

	if (missing(lang))
		lang <- "kr"     # en is second option

	if (missing(count))
		count <- 1000

	if (missing(stat_code))
		stat_code <- "010Y002"
	
	if (missing(cycle))
		cycle <- "MM"	 # YY,QQ,MM,DD 

	if (missing(start_date))
		start_date <- "196001"

	if (missing(end_date))
		end_date <- "201812"

	if (missing(item_code))
		item_code <- "AAAA11"

	if (format == "xml") {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticSearch/%s/%s/%s/1/%s/%s/%s/%s/%s/%s/?/?/", api_key, format, lang, count, stat_code, cycle, start_date, end_date, item_code))
		html <- getURLContent(url)
		xml_all <- xmlParse(html)

		if (is.null(unlist(xpathApply(xml_all, "//RESULT")))) {

			xml_cnt <- xpathApply(xml_all, "//list_total_count")[[1]]
			cnt <- as.integer(xmlToList(xml_cnt))
			xml_row <- xpathApply(xml_all, "//row") 
			df <- xmlToDataFrame(xml_row, stringsAsFactors = FALSE)
			df[] <- lapply(df, trimws)
			names(df) <- tolower(names(df))
			df$time <- as.Date(paste0(as.character(df$time), "01"), format = "%Y%m%d")
			df$data_value <- as.numeric(df$data_value)
			attr(df, "list_total_count") <- cnt 

		} else {

			code <- xmlToList(xpathApply(xml_all, "//CODE")[[1]])
			msg  <- xmlToList(xpathApply(xml_all, "//MESSAGE")[[1]])

			stop(paste0(code, "\n ", msg))

		}

	} else if (format == "json") {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticSearch/%s/%s/%s/1/%s/%s/%s/%s/%s/%s/?/?/", api_key, format, lang, count, stat_code, cycle, start_date, end_date, item_code))
		html <- getURLContent(url)
		json_all <- fromJSON(html)

		if (is.null(json_all$RESULT)) {

			cnt  <- json_all$StatisticSearch$list_total_count
			df   <- json_all$StatisticSearch$row
			df[] <- lapply(df, trimws)
			names(df) <- tolower(names(df))
			df$time <- as.Date(paste0(df$time, "01"), format = "%Y%m%d")
			df$data_value <- as.numeric(df$data_value)
			attr(df, "list_total_count") <- cnt 

		} else {

			code <- json_all$RESULT$CODE	
			msg  <- json_all$RESULT$MESSAGE	

			stop(paste0(code, "\n ", msg))

		}

	} else {

		stop("not supported data format.")

	}

	return(df); gc()
}
