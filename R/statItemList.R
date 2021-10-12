#' statItemList Function
#'
#' You can access economic statistics from Bank of Korea through the OPEN API (https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp)
#' @param api_key,format,lang,count,stat_code input parameters
#' @keywords ecos, statItemList 
#' @export
#' @examples
#' # Economic Statistic Item List from Bank of Korea through the OPEN API
#' df <- statItemList(api_key = your_api_key, format = "xml", lang = "kr", count = 100, stat_code = "010Y002")
#' head(df)
statItemList <- function(api_key, format = c("xml", "json"), lang = c("kr", "en"), count, stat_code) {

	if (missing(api_key))
	  stop("Please get your api key from website 'https://ecos.bok.or.kr/jsp/openapi/OpenApiController.jsp'")

	if (missing(count))
		count <- 100

	if (missing(stat_code))
		stat_code <- "010Y002"

	if (format[[1]] == "xml") {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticItemList/%s/%s/%s/1/%s/%s/", 
		                         api_key, format[[1]], lang[[1]], count, stat_code))
		html <- getURLContent(url)
		xml_all <- xmlParse(html)

		if (is.null(unlist(xpathApply(xml_all, "//RESULT")))) {

			xml_cnt <- xpathApply(xml_all, "//list_total_count")[[1]]
			cnt <- as.integer(xmlToList(xml_cnt))
			xml_row <- xpathApply(xml_all, "//row") 
			df <- xmlToDataFrame(xml_row, stringsAsFactors = FALSE)
			names(df) <- tolower(names(df))
			df$data_cnt <- as.numeric(df$data_cnt)
			attr(df, "list_total_count") <- cnt 

		} else {

			code <- xmlToList(xpathApply(xml_all, "//CODE")[[1]])
			msg  <- xmlToList(xpathApply(xml_all, "//MESSAGE")[[1]])

			stop(paste0(code, "\n ", msg))

		}

	} else if (format[[1]] == "json") {

		url <- URLencode(sprintf("http://ecos.bok.or.kr/api/StatisticItemList/%s/%s/%s/1/%s/%s/", 
		                         api_key, format[[1]], lang[[1]], count, stat_code))

		html <- getURLContent(url)
		json_all <- fromJSON(html)

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

	return(df); gc()
}

